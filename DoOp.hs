{-
--EPITECH PROJECT, 2021
--FUN_SEMINAR
--File description:
--DoOp.hs
-}

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
safeSucc a | a == Nothing = a
           | otherwise = fmap (+1) a

myLookup :: Eq a => a -> [(a, b)] -> Maybe b