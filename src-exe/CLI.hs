-- | Parser and usage message for command line arguments.
module CLI where

import Control.Monad
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Params = Params{
    ghcver :: Maybe String,        -- ^ optional GHC numeric version
    keeps :: [String],             -- ^ which packages the user wants to keep
    commitment :: Commitment,      -- ^ dry-run or do-it
    command :: Command             -- ^ chosen operation
    }
    deriving Show

data Command = GC | List
    deriving Show

data Commitment = Dryrun | Doit
    deriving Show

blankParams = Params{ghcver = Nothing, keeps = [], commitment = Dryrun, command = GC}

options :: [OptDescr (Params -> IO Params)]
options =
    [ Option ['h'] ["help"]
      (NoArg (\_ -> help))
      "this help message"
    , Option "g" ["ghc"]
      (ReqArg (\s o -> pure o{ghcver = Just s}) "VERSION")
      "GHC version, e.g., 8.10 or 8.10.7"
    , Option "l" ["list"]
      (NoArg (\o -> pure o{command = List}))
      "just list packages, remove nothing"
    , Option "y" ["yes"]
      (NoArg (\o -> pure o{commitment = Doit}))
      "perform the removals (default is dry-run)"
    ]

-- | Call 'getArgs' and parse into 'Params'.
--
-- Exit the whole program, instead of returning, upon --help (exit status 0) or
-- unrecognized options (exit status 1).
getParams :: IO Params
getParams = do
    args <- getArgs
    case getOpt RequireOrder options args of
      (fs, ks, []) -> foldM (flip ($)) blankParams{keeps=ks} fs
      (_, _, es) -> do
          hPutStr stderr (unlines es)
          hPutStrLn stderr "Please use --help or -h for usage."
          exitWith (ExitFailure 1)

helpmsg = usageInfo header options
  where
    header =
      "Usage: cabalgc [OPTION...] PKGID...\n\
      \Remove library packages except those you specify and transitive dependencies.\n\
      \BUT: Dry-run unless you say -y or --yes ."

help = do
    putStr helpmsg
    exitSuccess
