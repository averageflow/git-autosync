module ConsolePrinter where

red :: String
red = "\ESC[31m"

green :: String
green = "\ESC[32m"

cyan :: String
cyan = "\ESC[36m"

white :: String
white = "\ESC[37m"

fancyPrint :: String -> IO ()
fancyPrint message = putStrLn $ white ++ message

fancyPrint' :: String -> IO ()
fancyPrint' message = do
  putStrLn ""
  fancyPrint message

cyanPrint :: String -> IO ()
cyanPrint message = putStrLn $ cyan ++ message

errorPrint :: String -> IO ()
errorPrint message = putStrLn $ red ++ message

successPrint :: String -> IO ()
successPrint message = putStrLn $ green ++ message

fancySeparatorPrint :: IO ()
fancySeparatorPrint = putStrLn $ cyan ++ "<<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>"
