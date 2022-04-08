module ConsolePrinter where

fancyPrint :: String -> IO ()
fancyPrint message = putStrLn $ "\033[0;31m" ++ message

errorPrint :: String -> IO ()
errorPrint message = putStrLn message

successPrint :: String -> IO ()
successPrint message = putStrLn message

fancySeparatorPrint :: IO ()
fancySeparatorPrint = putStrLn "+-------------------------------------------------+"