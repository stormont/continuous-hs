
import System.Directory
import System.Environment (getArgs)
import System.INotify
import System.IO


main = do
  args <- getArgs
  let dir = head args
  putStrLn $ "Watching directory: " ++ dir
  n <- initINotify
  putStrLn "Press <Enter> to exit"
  print n
  wd <- addWatch n
                 [ Modify, CloseWrite, Create, Delete, MoveIn, MoveOut ]
                 dir
                 eventHandler
  print wd
  getLine
  removeWatch wd
  killINotify n


eventHandler :: Event -> IO ()
eventHandler x@(Modified _ (Just fp)) = handleFilteredFile x fp
eventHandler x@(MovedIn _ fp _) = handleFilteredFile x fp
eventHandler x@(MovedOut _ fp _) = handleFilteredFile x fp
eventHandler x@(Created _ fp) = handleFilteredFile x fp
eventHandler x@(Deleted _ fp) = handleFilteredFile x fp
eventHandler _ = return ()


handleFilteredFile evt fp = do
  if filterHS fp
    then print evt >> doWork fp
    else return ()


filterHS fp = (== "hs")
            $ reverse
            $ takeWhile (/= '.')
            $ reverse fp


doWork :: FilePath -> IO ()
doWork fp = return ()

