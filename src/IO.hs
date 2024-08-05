module IO where

import System.IO
    ( Handle, hClose, openFile, hPutStrLn, IOMode(WriteMode) )
import Simulation ( Interval(stopTime, startTime) )
import Solver ( Solver(dt) )

addTime :: IO [[a]] -> Interval -> Solver -> IO [(Double, [a])]
addTime answers intv solver = fmap (zip input) answers
  where input = [begin, begin + step .. end]
        begin = startTime intv
        end = stopTime intv
        step = dt solver

writeList :: Show a => [a] -> String
writeList [] = ""
writeList (x:xs) = show x ++ " " ++ writeList xs

writeData :: Show a => Handle -> (Double, [a]) -> IO ()
writeData handle (t, v) = hPutStrLn handle (show t ++ ", " ++ writeList v)

exportData :: Show a => [(Double, [a])] -> String -> IO ()
exportData dataSamples filename = do outh <- openFile filename WriteMode
                                     mapM_ (writeData outh) dataSamples
                                     hClose outh
