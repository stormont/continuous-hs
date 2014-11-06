
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import System.Directory
import System.Environment (getArgs)
import System.INotify
import System.IO


data Config =
  Config
    { confCabalFile :: FilePath
    , confWorking :: Bool
    } deriving (Show,Read)


main = do
  args <- getArgs
  let dir = head args
  putStrLn $ "Watching directory: " ++ dir
  contents <- getDirectoryContents dir
  let contents' = filter filterCabal contents
  case contents' of
    (x:_) -> runThread x dir
    [] -> do
      putStrLn "No cabal file found!"
      putStrLn "Exiting"


runThread cabal dir = do
  config <- newTVarIO $ Config cabal False
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


handleFilteredFile conf evt fp = do
  if filterHS fp
    then print evt >> doWork conf fp
    else return ()


filterHS fp = fileExt fp == "hs"
filterCabal fp = fileExt fp == "cabal"


fileExt = reverse
        . takeWhile (/= '.')
        . reverse


doWork :: TVar Config -> FilePath -> IO ()
doWork conf fp = do
  config <- readTVarIO conf
  if confWorking config
    then do
      print "Already working!"
      return ()
    else do
      print "New work available!"
      atomically $ writeTVar conf (config { confWorking = True })
      return ()

