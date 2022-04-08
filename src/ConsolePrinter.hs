module ConsolePrinter where

fancyPrint :: String -> IO ()
fancyPrint message = putStrLn $ "\ESC[31m" ++ message

errorPrint :: String -> IO ()
errorPrint message = putStrLn message

successPrint :: String -> IO ()
successPrint message = putStrLn message

fancySeparatorPrint :: IO ()
fancySeparatorPrint = putStrLn "+-------------------------------------------------+"