
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.INotify
import System.IO
import System.Process


data Config =
  Config
    { confExecFile :: FilePath
    , confWorking :: Bool
    } deriving (Show,Read)


-- Options
data Options =
   Options
      { optUsage    :: Bool
      , optExecFile :: FilePath
      , optWatchDir :: FilePath
      } deriving (Show)


options :: [OptDescr (Options -> IO Options)]
options =
   [ Option "?" ["help"]     (NoArg  runUsage)               "Show usage/help"
   , Option "f" ["execfile"] (ReqArg setExecFile "execfile") "The file to read execution commands from"
   , Option "d" ["watchdir"] (ReqArg setWatchDir "watchdir") "The directory to observe for changes"
   ]


defaultOptions = Options { optUsage    = False
                         , optExecFile = []
                         , optWatchDir = []
                         }


runUsage      opt = return opt { optUsage    = True }
setExecFile x opt = return opt { optExecFile = x }
setWatchDir x opt = return opt { optWatchDir = x }


showUsage :: IO ()
showUsage = do
   let header = "Usage: continuous-hs [OPTIONS...]"
   putStrLn $ usageInfo header options
   exitFailure


main :: IO ()
main = do
   args <- getArgs
   let (actions, nonOpts, msgs) = getOpt RequireOrder options args
   opts <- foldl (>>=) (return defaultOptions) $ reverse actions
   if optUsage opts || optExecFile opts == [] || optWatchDir opts == []
      then showUsage
      else runWorker opts
-- /OPTIONS


runWorker
   :: Options
   -> IO ()
runWorker opts = do
  let dir = optWatchDir opts
  let execFile = optExecFile opts
  putStrLn $ "Exec file: " ++ execFile
  putStrLn $ "Watching directory: " ++ dir
  fileExists <- doesFileExist execFile
  when (not fileExists) $ do
      putStrLn $ "Exec file does not exist!"
      putStrLn "Exiting"
      exitFailure
  dirExists <- doesDirectoryExist dir
  when (not dirExists) $ do
      putStrLn $ "Watch directory does not exist!"
      putStrLn "Exiting"
      exitFailure
  let config = Config execFile False
  runThread config dir


runThread config dir = do
  config <- newTVarIO config
  n <- initINotify
  putStrLn "Press <Enter> to exit"
  putStrLn n
  wd <- addWatch n
                 [ Modify, CloseWrite, Create, Delete, MoveIn, MoveOut ]
                 dir
                 (eventHandler config)
  putStrLn wd
  getLine
  removeWatch wd
  killINotify n


eventHandler :: TVar Config -> Event -> IO ()
eventHandler conf x@(Modified _ (Just fp)) = handleFilteredFile conf x fp
eventHandler conf x@(MovedIn _ fp _) = handleFilteredFile conf x fp
eventHandler conf x@(MovedOut _ fp _) = handleFilteredFile conf x fp
eventHandler conf x@(Created _ fp) = handleFilteredFile conf x fp
eventHandler conf x@(Deleted _ fp) = handleFilteredFile conf x fp
eventHandler _ _ = return ()


handleFilteredFile conf evt fp =
  when (filterHS fp) $ putStrLn evt >> doWork conf


filterHS fp = fileExt fp == "hs"


fileExt = reverse
        . takeWhile (/= '.')
        . reverse


doWork :: TVar Config -> IO ()
doWork conf = do
  config <- readTVarIO conf
  if confWorking config
    then do
      putStrLn "Already working!"
      return ()
    else do
      putStrLn "New work available!"
      atomically $ writeTVar conf (config { confWorking = True })
      _ <- forkIO $ runCI conf
      return ()


runCI :: TVar Config -> IO ()
runCI conf = do
  config <- readTVarIO conf
  contents <- readFile $ confExecFile config
  execContents $ lines contents
  atomically $ writeTVar conf (config { confWorking = False })
  return ()


execContents :: [String] -> IO Bool
execContents [] = return True
execContents (x:xs) = do
  let p = shell x
  (_,_,_,h) <- createProcess p
  exitCode <- waitForProcess h
  case exitCode of
    ExitSuccess -> execContents xs
    ExitFailure _ -> return False
