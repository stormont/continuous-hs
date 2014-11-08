
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
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


main = do
  args <- getArgs
  let dir = head args
  let execFile = args !! 1
  putStrLn $ "Watching directory: " ++ dir
  putStrLn $ "Exec file: " ++ execFile
  fileExists <- doesFileExist execFile
  if fileExists
    then do
      let config = Config execFile False
      runThread config dir
    else do
      putStrLn $ "Exec file does not exist!"
      putStrLn "Exiting"


runThread config dir = do
  config <- newTVarIO config
  n <- initINotify
  putStrLn "Press <Enter> to exit"
  print n
  wd <- addWatch n
                 [ Modify, CloseWrite, Create, Delete, MoveIn, MoveOut ]
                 dir
                 (eventHandler config)
  print wd
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
  when (filterHS fp) $ print evt >> doWork conf


filterHS fp = fileExt fp == "hs"


fileExt = reverse
        . takeWhile (/= '.')
        . reverse


doWork :: TVar Config -> IO ()
doWork conf = do
  config <- readTVarIO conf
  if confWorking config
    then do
      print "Already working!"
      return ()
    else do
      print "New work available!"
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
