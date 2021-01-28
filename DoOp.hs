{-
--EPITECH PROJECT, 2021
--FUN_SEMINAR
--File description:
--DoOp.hs
-}

import System.Environment
import System.Exit

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) | a == x = True
                | otherwise = myElem a xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing
safeNth (x:_) 0 = Just x
safeNth (_:xs) a = safeNth xs (a - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc a = (+1) <$> a

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup a ((x, y):t) | a == x = Just y
                      | otherwise = myLookup a t

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo _ Nothing _ = Nothing
maybeDo _ _ Nothing = Nothing
maybeDo func a b = func <$> a <*> b

isDigit :: [Char] -> Bool -> Bool
isDigit [] _ = True
isDigit (x:xs) f | f && myElem x ['-', '0', '1', '2', '3', '4'
                        , '5', '6', '7', '8', '9'] = isDigit xs False
                 | not f && myElem x ['0', '1', '2', '3', '4'
                        ,'5', '6', '7', '8', '9'] = isDigit xs False
                 | otherwise = False

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt s | isDigit s True = Just (read s)
          | otherwise = Nothing

getLineLength :: IO Int
getLineLength = length <$> getLine

printAndGetLength :: String -> IO Int
printAndGetLength s = putStrLn s >> return (length s)

printLine :: Char -> Char -> Int -> Int -> String
printLine bae _ 1 _ = [bae] ++ [bae]
printLine bae mid _ 1 = [mid] ++ [bae]
printLine bae mid len ctr | ctr == len = [bae] ++ [mid]
                              ++ printLine bae mid len (ctr - 1)
                          | otherwise = [mid] ++ [mid]
                              ++ printLine bae mid len (ctr - 1)

printBoxBis :: Int -> Int -> String
printBoxBis 1 _ = printLine '+' '-' 1 1
printBoxBis len 1 = printLine '+' '-' len len
printBoxBis len ctr | ctr == len = printLine '+' '-' len len
                        ++ "\n" ++ printBoxBis len (ctr - 1)
                    | otherwise = printLine '|' ' ' len len
                        ++ "\n" ++ printBoxBis len (ctr - 1)

printBox :: Int -> IO ()
printBox len | len < 1 = putStrLn "\0"
             | otherwise = putStrLn (printBoxBis len len)

concatString :: String -> String -> String
concatString a b = a ++ b

concatLines :: Int -> IO String
concatLines 0 = return ""
concatLines ctr = concatString <$> getLine <*> concatLines (ctr - 1)

getInt :: IO (Maybe Int)
getInt = readInt <$> getLine

zero :: Maybe Int
zero = Just 0

bool :: Maybe Bool -> Bool
bool (Just a) = a

printMaybe :: Maybe Int -> IO ()
printMaybe (Just a) = print a

makeDiv :: Maybe Int -> Maybe Int -> IO ()
makeDiv a b | bool ((==) <$> b <*> zero) = exitWith (ExitFailure 84)
            | otherwise = printMaybe (div <$> a <*> b)

makeMod :: Maybe Int -> Maybe Int -> IO ()
makeMod a b | bool ((==) <$> b <*> zero) = exitWith (ExitFailure 84)
            | otherwise = printMaybe (mod <$> a <*> b)

doop :: [String] -> IO ()
doop s = let a = readInt (head s)
             b = readInt (last s)
             op = s!!1
          in case op of
                "+" -> printMaybe ((+) <$> a <*> b)
                "-" -> printMaybe ((-) <$> a <*> b)
                "doop" -> printMaybe ((*) <$> a <*> b)
                "/" -> makeDiv a b
                "%" -> makeMod a b
                _ -> exitWith (ExitFailure 84)

--doop :: [String] -> IO ()
--doop s = let a = readInt (head s)
--             b = readInt (last s)
--             op = s!!1
--             os = getProgName
--          in case op of
--                "+" -> printMaybe ((+) <$> a <*> b)
--                "-" -> printMaybe ((-) <$> a <*> b)
--                "/" -> makeDiv a b
--                "%" -> makeMod a b
--                z -> if (==) <$> z <*> os
--                        then printMaybe ((*) <$> a <*> b)
--                        else exitWith (ExitFailure 84)

main :: IO ()
main = getArgs >>= doop