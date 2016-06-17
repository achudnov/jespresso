{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Extraction and consolidation of JavaScript code in an HTML page.
module Text.Html.Consolidate (-- * Simple API
                              consolidate
                             ,extract
                             ,extractPretty
                              -- * Advanced arrow-based API
                             ,TArr                              
                             ,consolidateArr
                             ,extractJSArr
                             ,initialConsState
                             ,insertJSArr
                             ,parseHTML
                             ,renderHTML
                             ,ConsState
                             ) where

import Text.XML.HXT.Core hiding (swap)
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.TagSoup
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.CodeGen hiding (this)
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.PrettyPrint
import Language.ECMAScript5.Parser hiding (program)
import qualified Language.ECMAScript5.Parser as Parse (program)
import Data.List (isInfixOf)
import Data.Default.Class
import Network.HTTP
import Network.Browser (Cookie)
import Network.Browser.Simple
import Network.URI
import Network.HTTP.Encoding
import Data.ByteString.Lazy (ByteString)
import System.Random
import Data.Char
import Data.Maybe (isJust, fromJust, maybeToList, fromMaybe)
import Control.Monad hiding (when)
import Data.Default.Instances.Base
import Control.Applicative ((<$>))

-- | Consolidation state
data ConsState = ConsState Bool  -- Ignore errors?
                           (Maybe URI) -- Base URI of the web page,
                                       -- for resolving relative URI's
                           [Cookie] -- Cookies to include with all
                                    -- HTTP requests
                           [Statement ()]

-- | A constructor function for making an initial consolidation state
-- (needed for running the arrows in the advanced API). Example usage:
--
-- > runXIOState (initialState $ initialConsState ignoreErrors muri []) $ someArrow
initialConsState :: Bool -- ^ Whether to ignore errors (parse errors,
                         -- resource not found etc.)
                 -> Maybe URI -- ^ base URI
                 -> [Cookie] -- ^ Cookies
                 -> ConsState
initialConsState grace base cookies = ConsState grace base cookies []

-- | Our XML transformation arrow type
type TArr a b = IOStateArrow ConsState a b

-- | A wrapper around the hxt parser with commonly used arguments
parseHTML :: String -> Maybe URI -> TArr a XmlTree
parseHTML s mbase_uri = 
  let config = map (withDefaultBaseURI . show) (maybeToList mbase_uri)
             ++[withParseHTML yes
               ,withTagSoup
               ,withValidate no
               ,withSubstDTDEntities no
               ,withSubstHTMLEntities yes
               ,withCanonicalize no
               ,withOutputHTML]
  in readString config s

-- | A wrapper around hxt to pretty print html out of the arrow
renderHTML :: ConsState -> TArr XmlTree XmlTree -> IO String
renderHTML ns a = 
  let state  = initialState ns
  in liftM head $ runXIOState state (single a >>> writeDocumentToString
                                     -- Using plain text output to
                                     -- prevent entity substitution on
                                     -- special XML/HTML characters
                                     -- that are part of inline
                                     -- scripts
                                     [withOutputPLAIN, withOutputEncoding utf8])

-- | Takes an HTML page source as a string and an optional base URI
-- (for resolving relative URI's) and produces an HTML page with all
-- the scripts consolidated in one inline script element.
consolidate :: String -> Maybe URI -> IO String
consolidate s mbase_uri = 
  renderHTML (initialConsState True mbase_uri []) $ 
  parseHTML s mbase_uri >>> consolidateArr

-- | The consolidation function with an arrow interface.
--
-- > consolidateArr = extractJSArr >>> insertJSArr
consolidateArr :: TArr XmlTree XmlTree
consolidateArr = extractJSArr >>> insertJSArr

-- | Extacts all JavaScript code in the given
-- HTML page source as a single program. Takes an optional base URI
-- for resolving relative URI's.
extract :: String -> Maybe URI -> IO (Program ())
extract s mbase_uri =
  let state = initialState $ initialConsState True mbase_uri [] in
  do [(_, js)] <- runXIOState state $ single $ 
                  parseHTML s mbase_uri >>> extractJSArr
     return js

-- | Extacts and pretty-prints all JavaScript code in the given
-- HTML page source as a single program. Takes an optional base URI
-- for resolving relative URI's.
-- 
-- > extractPretty s muri = liftM (show . prettyPrint) $ extract s muri
extractPretty :: String -> Maybe URI -> IO String
extractPretty s muri = liftM (show . prettyPrint) $ extract s muri

-- | Extracts all JavaScript from HTML. There shouldn't be any
-- JavaScript in the resulting XmlTree
extractJSArr :: TArr XmlTree (XmlTree, Program ())
extractJSArr =
  (choiceA [isAJavaScript :-> ifA (hasAttr "src") extractExternalScript
                                                   extractInlineScript
            ,(isElem >>> hasOneOfNames src_tags >>> hasOneOfAttrs src_attrs) :-> extractURLProp
            ,(isElem >>> hasOneOfAttrs event_handlers) :-> extractEventHandler
            ,this :-> this
            ]
   `processTopDownUntilAndWhenMatches` isFrame)
  >>>returnScript
  where returnScript = (returnA &&& getUserState)
                    >>> second (arr (\s -> let ConsState _ _ _ stmts = s 
                                           in  Program () stmts))
        isFrame = isElem >>> (hasName "frame" <+> hasName "iframe")
        hasOneOfNames tagNames = (getName >>> isA (`elem` tagNames)) `guards` this
        hasOneOfAttrs attrNames = (getAttrl >>> hasOneOfNames attrNames) `guards` this
        -- Elements and properties that might contain a 'javascript:' url 
        -- IMG.SRC, A.HREF, FORM.ACTION, FRAME.SRC, IFRAME.SRC, LINK.HREF
        src_tags = ["img", "a", "form", "frame", "iframe", "link"]
        src_attrs = ["src", "href", "action"]
        event_handlers = ["onabort", "onblur", "onclick", "oncompositionstart"
                         ,"oncompositionupdate", "oncompositionend", "ondblclick"
                         ,"onerror", "onfocus", "onfocusin", "onfocusout"
                         ,"onkeydown", "onkeypress", "onkeyup", "onload"
                         ,"onmousedown", "onmouseenter", "onmouseleave"
                         ,"onmousemove", "onmouseout", "onmouseover"
                         ,"onmouseup", "onreset", "onresize","onscroll"
                         ,"onselect", "onsubmit", "ontextinput", "onunload"
                         , "onwheel"]

-- | Like Control.Arrow.ArrowTree, but instead has a separate arrow
-- that serves as a predicate signalling that this subtree shouldn't
-- be transformed any further; 'processTopDownUntilMatches transformer
-- predicate'. Almost dual of 'processBottomUpWhenNot', but transforms
-- the node that matches the predicate as well (but doesn't look
-- inside that node).
processTopDownUntilAndWhenMatches :: (ArrowTree a, Tree t)
                                  => a (t b) (t b) -- ^ the transformer arrow
                                  -> a (t b) (t b) -- ^ the predicate arrow
                                  -> a (t b) (t b)
processTopDownUntilAndWhenMatches t p =
  t >>> (processChildren (processTopDownUntilAndWhenMatches t p) `whenNot` p)
  -- ifA p (t >>> processChildren (processTopDownUntilAndWhenMatches t p)) returnA
                       
-- | Inserts JavaScript at the end of the HTML body.
insertJSArr :: TArr (XmlTree, Program a) XmlTree
insertJSArr = (swap ^<< second scriptElement) >>>
              arr2A (\scr ->  processTopDown $ changeChildren (++ [scr]) `when`
                              hasName "body")
              
-- extractors
-- | Extracts the contents of inline scripts
extractInlineScript :: TArr XmlTree XmlTree
extractInlineScript = 
  firstChild >>> 
  getText >>> 
  parseJS >>> 
  arr removeAnnotations >>> 
  appendScript >>>
  cmt "Removed Inline Script"
  
-- | Extracts the contents of externally references scripts
extractExternalScript :: TArr XmlTree XmlTree
extractExternalScript = 
  (getAttrValue "src" >>>
   (downloadArr >>>
    parseJS >>>
    arr removeAnnotations >>>  
    appendScript) 
   &&&
   (arr ("Removed External Script: " ++) >>>
    mkCmt)) >>>
  arr snd
  
-- |Downloads the content, considering the input as a URL; performs
-- decoding automatically.
downloadArr :: TArr String String
downloadArr = 
  (returnA &&& arr parseURIReference) >>> 
  arrIO (\(url, muri) -> 
          failIfNothing ("download: error parsing a URI: " ++ url) muri) >>>
  consolidateURI >>>
  (getUserState &&& returnA) >>>
  arrIO (\(ConsState _ _ cookies _, uri) -> liftM fst (download uri cookies))

  where failIfNothing :: String -> Maybe a -> IO a
        failIfNothing message Nothing  = fail message
        failIfNothing _       (Just x) = return x
        -- RFC 3986, section 4.1: if the URI-reference's prefix does
        -- not match the syntax of a scheme followed by its colon
        -- separator, then the URI reference is a relative reference.
        isURIRelative :: URI -> Bool
        isURIRelative = null . uriScheme
        consolidateURI :: TArr URI URI
        consolidateURI = 
          (getUserState &&& returnA) >>>
          arrIO (\(ConsState _ mbaseURI _ _, uri) ->
          return $ if isURIRelative uri && isJust mbaseURI
                   then uri `relativeTo` fromJust mbaseURI
                   else uri)
        
-- Removes the URL containing properties from elements and adds a
-- JavaScript assignment instead.
extractURLProp :: TArr XmlTree XmlTree
extractURLProp = 
  -- Elements and properties that might contain a 'javascript:' url 
  -- IMG.SRC, A.HREF, FORM.ACTION, FRAME.SRC, IFRAME.SRC, LINK.HREF
  -- isElem >>>
  -- selectTags ["img", "a", "form", "frame", "iframe", "link"] >>>
  addIdIfNotPresent >>>
  (((selectAttrValues ["src", "href", "action"] &&& selectId) >>>
    arr (\((url, attrName), id) -> 
     Program () [ExprStmt () $ AssignExpr () OpAssign 
                (DotRef () (CallExpr ()
                          (DotRef () (VarRef () (Id () "document")) (Id () "getElementById"))
                          [StringLit () id]) (ident attrName)) (StringLit () url)]) >>>
   appendScript) &&& 
  removeAttributes ["src", "href", "action"]) >>>
  arr snd
  
-- Removes the event handlers from HTML tags and converts them to
-- JavaScript assignments
extractEventHandler :: TArr XmlTree XmlTree
extractEventHandler = 
  -- Names of HTML tag attributes that are event handler declarations:
  let attrNames = ["onabort", "onblur", "onclick", "oncompositionstart",
                   "oncompositionupdate", "oncompositionend", "ondblclick", 
                   "onerror", "onfocus", "onfocusin", "onfocusout", "onkeydown",
                   "onkeypress", "onkeyup", "onload", "onmousedown",
                   "onmouseenter", "onmouseleave", "onmousemove", "onmouseout", 
                   "onmouseover", "onmouseup", "onreset", "onresize","onscroll",
                   "onselect", "onsubmit", "ontextinput", "onunload", "onwheel"]
  in addIdIfNotPresent >>>
     (((selectId &&& selectAttrValues attrNames) >>>
      arr (\(id, (handler, attrName)) -> makeHandlerDeclaration id attrName handler)
      >>> appendScript) &&&
     removeAttributes attrNames) >>>
     arr snd

makeHandlerDeclaration :: Default a => String -> String -> String -> Program a
makeHandlerDeclaration id handlerName handlerSource =
  let mEventName = case handlerName of
                    'o':'n':s | not $ null s -> Just s
                    _                        -> Nothing
      isErrorHandler = handlerName == "onerror"
      either2maybe (Left _) = Nothing
      either2maybe (Right x)= Just x
  in fromMaybe (program []) $
     do eventName <- mEventName
        hsrc <- either2maybe $ reannotate def <$> parseFromString handlerSource
        return $ program [expr $ call (
                            call (var "document" `dot` "getElementById") [string id]
                            `dot` "addEventListener")
                          [string eventName, (if isErrorHandler then makeErrorHandler else makeHandler) hsrc, bool False]
                         ]

-- | Makes a handler function expression from the parsed inlined
-- handler source for any event, but onerror.
-- 
-- HTML5 spec, section 6.1.5.1, "internal raw uncompiled handler":
-- using the script execution environment obtained above, create a
-- function object (as defined in ECMAScript edition 5 section 13.2
-- Creating Function Objects), with Parameter list
-- FormalParameterList. If H is an onerror event handler of a Window
-- object, let the function have five arguments, named event, source,
-- lineno, colno, and error. 
-- 
-- Unsupported as of yet: let Scope be the result of
-- NewObjectEnvironment(document, the global environment). If form
-- owner is not null, let Scope be the result of
-- NewObjectEnvironment(form owner, Scope). If element is not null,
-- let Scope be the result of NewObjectEnvironment(element, Scope).
makeErrorHandler :: Default a => Program a -> Expression a
makeErrorHandler = lambda ["event", "source", "lineno", "colno", "error"] . unProgram

-- | Otherwise, let the function have a single argument called event.
makeHandler :: Default a => Program a -> Expression a
makeHandler = lambda ["event"] . unProgram

parseJS :: TArr String (Program SourceSpan)
parseJS = arr (parse Parse.program "") >>> eitherToFailure >>> arr (reannotate fst)

-- Arrow tools
-- | Failure reporting arrow constructor
arrowFail :: ArrowIO ar => String -> ar b c
arrowFail = arrIO . fail

isStrict :: TArr a Bool
isStrict = getUserState >>> arr (\s -> let ConsState strict _ _ _ = s in strict)

-- | If in strict mode, fails with the message given; otherwise,
-- behaves like an identity arrow
failIfStrict :: String -> TArr a a
failIfStrict msg = 
  proc a -> do is <- isStrict -< ()
               if is then arrowFail msg -< () else returnA -< a

eitherToFailure :: (Show err, Default a) => TArr (Either err a) a
eitherToFailure = (isStrict &&& returnA) >>> arrIO f
  where f (False, Left err) = return def
        f (True,  Left err) = fail $ show err
        f (_   ,  Right x)  = return x

maybeToFailure :: (Default a) => Maybe String -> TArr (Maybe a) a
maybeToFailure message = (isStrict &&& returnA) >>> arrIO f
  where f (False, Nothing) = return def
        f (True , Nothing) = 
          case message of
            Nothing  -> fail "Unexpected maybe in strict mode"
            Just msg -> fail msg
        f (_    , Just x)  = return x

appendStatements :: TArr [Statement ()] ()
appendStatements = (getUserState &&& returnA) >>>
                   arr (\(state, addScript) -> 
                         let ConsState grace baseURI cookies script = state in
                         ConsState grace baseURI cookies (script++addScript)) >>>
                   setUserState >>> arr (const ())

appendScript :: TArr (Program ()) ()
appendScript = arr (\s -> let Program _ stmts = s in stmts) >>> appendStatements
                   
-- constructors
-- | Constructs a new JavaScript element
scriptElement :: ArrowXml ar => ar (Program a) XmlTree
scriptElement = (mkElement (mkName "script") (sattr "type" "text/javascript") $< arr (txt . show . prettyPrint)) >>> addAttr "defer" ""

-- Selectors
-- | A selector for SCRIPT tags with JavaScript or empty type
isAJavaScript :: ArrowXml ar => ar XmlTree XmlTree
isAJavaScript = 
  isElem >>> hasName "script" >>>
  (((hasAttr "language" >>> hasAttrValue "language" (isInfixOf "javascript")) <+>
    (hasAttr "type" >>> hasAttrValue "type" (isInfixOf "javascript"))) `orElse`
   returnA)
        
-- | Selects the first child of a node
firstChild :: (ArrowTree a, Tree t) => a (t b) (t b)
firstChild = single getChildren
  
-- | Selects the last child fo a node
lastChild :: (ArrowTree a, Tree t) => a (t b) (t b)
lastChild = getChildren >>. (take 1 . reverse)

-- | Selects the <html> tag
html :: ArrowXml a => a XmlTree XmlTree
html = deep $ hasName "html"

-- | Selects <body> tag
body :: ArrowXml a => a XmlTree XmlTree
body = html /> hasName "body"
  --deep $ hasName "html" /> hasName "body"

selectTags :: ArrowXml a => [String] -> a XmlTree XmlTree
selectTags = foldl (\arr tag -> arr <+> hasName tag) zeroArrow

selectAttrValues :: ArrowXml a => [String] -> a XmlTree (String, String)
selectAttrValues = foldl f zeroArrow
  where f :: ArrowXml a => 
             a XmlTree (String, String) -> String -> a XmlTree (String, String)
        f a attr = a <+> (hasAttr attr >>> 
                          (getAttrValue attr &&& arr (const attr)))
                   
hasAnyAttrs :: ArrowXml a => [String] -> a XmlTree XmlTree
hasAnyAttrs = foldl f zeroArrow
  where f :: ArrowXml a => a XmlTree XmlTree -> String -> a XmlTree XmlTree
        f a attr = a <+> hasAttr attr
                   
removeAttributes :: ArrowXml a => [String] -> a XmlTree XmlTree
removeAttributes names =
  processAttrl $ none `when` (foldr (\n a -> a <+> hasName n) none names)
  
addIdIfNotPresent :: TArr XmlTree XmlTree
addIdIfNotPresent =
  ((genIdA &&& this) >>> (arr2A $ addAttr "id")) `when`
  (getAttrValue "id" >>> isA null)

-- | Selects the id of an element or adds a new one (and returns) if
-- it's not present
selectId :: TArr XmlTree String
selectId = isElem >>> getAttrValue "id"
                        
genIdA :: ArrowIO ar => ar a String
genIdA = arrIO $ const genId

genId :: IO String
genId = do firstLetter <- genLetter
           return [firstLetter]
           length <- getStdRandom $ randomR (minIdLength-1, maxIdLength-1)
           restId <- mapM (const genLetter) [1..length] 
           return $ firstLetter:restId
  where minIdLength :: Int
        minIdLength = 16
        maxIdLength = 32
        
genLetter :: IO Char
genLetter = do letter <- getStdRandom $ randomR (capitalACode, capitalZCode)
               lettercase <- getStdRandom $ randomR (0,1)
               return $ chr $ letter + lettercase * (lowercaseACode-capitalACode)
  where capitalACode = 65
        capitalZCode = 90
        lowercaseACode = 97
        
swap (a,b) = (b,a)
