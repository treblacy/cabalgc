-- | Parser and usage message for command line arguments.
module CLI where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Version (showVersion)
import Paths_cabalgc (version)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Params = Params{
    ghcver :: Maybe String,        -- ^ optional GHC numeric version
    pkgIDs :: [String],            -- ^ packages specified by the user
    printFullHash :: Bool,         -- ^ full hash when printing package IDs
    commitment :: Commitment,      -- ^ dry-run or do-it
    command :: Command             -- ^ chosen operation
    }
    deriving Show

data Command = GC | RM | List | ListDeps | ListTops | ListRevDeps
    deriving Show

data Commitment = Dryrun | Doit
    deriving Show

blankParams = Params{ghcver = Nothing, printFullHash = False, commitment = Dryrun,
                     pkgIDs = [],
                     command = GC}

options :: [OptDescr (Params -> IO Params)]
options =
    [ Option ['h'] ["help"]
      (NoArg (\_ -> help))
      "this help message"
    , Option ['V'] ["version"]
      (NoArg (\_ -> printVersion))
      "print version number"
    , Option "g" ["ghc"]
      (ReqArg (\s o -> pure o{ghcver = Just s}) "VERSION")
      "GHC version, e.g., 8.10 or 8.10.7"
    , Option "l" ["list"]
      (NoArg (\o -> pure o{command = List}))
      "just list packages, remove nothing"
    , Option "d" ["deps"]
      (NoArg (\o -> pure o{command = ListDeps}))
      "just list dependencies, remove nothing"
    , Option "r" ["rdeps"]
      (NoArg (\o -> pure o{command = ListRevDeps}))
      "just list reverse dependencies, remove nothing"
    , Option "t" ["tops"]
      (NoArg (\o -> pure o{command = ListTops}))
      "just list packages not depended on, remove nothing"
    , Option "" ["fullhash"]
      (OptArg (\m o -> do
                    b <- parseBool (fromMaybe "y" m)
                    pure o{printFullHash = b})
              "y|n")
      "full hashes when printing package IDs"
    , Option "x" ["remove"]
      (NoArg (\o -> pure o{command = RM}))
      "remove only specified packages (dry-run default applies)"
    , Option "y" ["yes"]
      (NoArg (\o -> pure o{commitment = Doit}))
      "perform the removals (default is dry-run)"
    ]
  where
    parseBool s | s `elem` ["1", "Y", "y"] = pure True
                | otherwise = pure False
    -- This lives in IO for future printing error messages and exiting.

-- | Call 'getArgs' and parse into 'Params'.
--
-- Exit the whole program, instead of returning, upon --help (exit status 0) or
-- unrecognized options (exit status 1).
getParams :: IO Params
getParams = do
    args <- getArgs
    case getOpt RequireOrder options args of
      (fs, ks, []) -> foldM (flip ($)) blankParams{pkgIDs=ks} fs
      (_, _, es) -> do
          hPutStr stderr (concat es)
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

printVersion = do
    putStrLn (showVersion version)
    exitSuccess
