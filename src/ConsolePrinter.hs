module ConsolePrinter where

red = "\ESC[31m"

green = "\ESC[32m"

cyan = "\ESC[36m"

white = "\ESC[37m"

fancyPrint :: String -> IO ()
fancyPrint message = putStrLn $ message

errorPrint :: String -> IO ()
errorPrint message = putStrLn $ red ++ message

successPrint :: String -> IO ()
successPrint message = putStrLn $ green ++ message

fancySeparatorPrint :: IO ()
fancySeparatorPrint = putStrLn $ cyan ++ "+-------------------------------------------------+"