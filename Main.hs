module Main where

import Text.Html.Consolidate
import System.Console.CmdArgs.Explicit
import System.IO
import Network.URI
import Network.Browser
import Network.Browser.Simple

-- | The entry point to the `jespresso` program. Can be run in two
-- modes:
-- * extraction; `jespresso [-e] [<uri>]`. Takes text (HTML) via stdin
-- and outputs all the JavaScript in it (including externally
-- referenced) via stdout. If an (optional) url is supplied, the HTML
-- is downloaded from that URL instead of reading it from the stdin.
-- * normalization: `jespresso -n [<uri>]`. Transforms the HTML and
-- JavaScript into an equivalent one so that all the JavaScript is
-- inlined and in a single <script> tag. If an (optional) url is
-- supplied, the HTML is downloaded from that URL instead of reading
-- it from the stdin.

main :: IO Int
main = do args <- processArgs arguments 
          (input, cookies) <- case muri args of
                Just uri -> download uri []
                Nothing  -> getContents >>= \c -> return (c, [])
          output <- case pmode args of
            Extraction -> extractPretty input (muri args)
            Consolidation -> consolidate input (muri args)
          putStr output
          return 1

arguments :: Mode Parameters
arguments = (modeEmpty defaultArgs) 
            {modeNames = ["jespresso"]
            ,modeHelp = ""
            ,modeArgs = ([flagArg addURI "URI"], Nothing)
            ,modeGroupFlags = 
              toGroup
              [flagNone ["e", "extract"] addExtract extractHelp
              ,flagNone ["n", "normalize"] addNormalize normalizeHelp]}
  where defaultArgs = Parameters {pmode = Extraction
                                 ,muri  = Nothing}
        addURI u params = case parseURI u of
          Nothing  -> Left "Invalid URI"
          Just uri -> Right $ params {muri = Just uri}
        addExtract   params = params {pmode = Extraction}
        addNormalize params = params {pmode = Consolidation}
        extractHelp = "Extract JavaScript from the supplied HTML and \
                      \print to stdout"
        normalizeHelp = "Transform HTML (with JavaScript in it) into an \
                        \equivalent one so that all the JavaScript is inlined \
                        \and in a single <script> tag."

data Parameters = Parameters {pmode :: ProgramMode
                             ,muri  :: Maybe URI}
                  deriving (Show)
data ProgramMode = Extraction | Consolidation
                 deriving (Show)
