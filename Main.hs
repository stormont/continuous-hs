
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
    { confOutputFile :: FilePath
    , confCabalFile :: FilePath
    , confWorking :: Bool
    } deriving (Show,Read)


main = do
  args <- getArgs
  let dir = head args
  let cabalOutput = args !! 1
  putStrLn $ "Watching directory: " ++ dir
  putStrLn $ "Cabal output file: " ++ cabalOutput
  contents <- getDirectoryContents dir
  let contents' = filter filterCabal contents
  case contents' of
    (x:_) -> do
      let config = Config cabalOutput x False
      runThread config dir
    [] -> do
      putStrLn "No cabal file found!"
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
filterCabal fp = fileExt fp == "cabal"


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
  runCIChain conf
  config <- readTVarIO conf
  atomically $ writeTVar conf (config { confWorking = False })
  return ()


runCIChain :: TVar Config -> IO ()
runCIChain conf = do
  cabalBuild <- runCabal conf ["build"]
  print $ "*** cabal build result: " ++ show cabalBuild
  case cabalBuild of
    False -> return ()
    True -> do
      cabalTest <- runCabal conf ["test"]
      print $ "*** cabal test result: " ++ show cabalTest


runCabal :: TVar Config -> [String] -> IO Bool
runCabal conf args = do
  (code, out, err) <- readProcessWithExitCode "cabal" args ""
  config <- readTVarIO conf
  let outputFile = confOutputFile config
  _ <- when (out /= []) $ appendFile outputFile out
  _ <- when (err /= []) $ appendFile outputFile err
  case code of
    ExitSuccess -> return True
    ExitFailure _ -> return False

