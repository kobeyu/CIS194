module HW01(validate,hanoi,hanoiN) where
import Data.List(unfoldr,sort)
import Data.Tuple(swap)
import Data.Function.Memoize
import Data.Maybe(listToMaybe,maybeToList)
--LUHN

toDigitsRev :: Integer -> [Integer]
toDigitsRev = unfoldr step 
 where 
    step 0 = Nothing
    step i = Just . swap . (`divMod` 10) $ i

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id,(*2)])

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev

validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev

--HANOI

type Move a = (a,a)

hanoi :: Integer -> a -> a -> a -> [Move a]
hanoi 0 _ _ _ = []
hanoi n a b c = concat [hanoi (n-1) a c b,[(a,b)],hanoi (n-1) c b a]

-- HANOI N

bigT :: Int -> Int -> Maybe (Int,Int)
bigT = memoFix2 bT

bT :: (Int -> Int -> Maybe (Int,Int)) -> Int -> Int -> Maybe (Int,Int)
bT _ _ 0 = Nothing
bT _ 0 _ = Just (0,0) 
bT _ _ 1 = Nothing 
bT _ 1 _ = Just (1,1) 
bT _ _ 2 = Nothing
bT f n r = listToMaybe $ sort list  
    where
        list :: [(Int,Int)]
        list = do
            k <- [1..n-1]
            a <- fmap fst . maybeToList $ f k r
            b <- fmap fst . maybeToList $ f (n-k) (r-1)
            return $ (2 * a + b,k)

hanoiN :: Int -> [a] -> [Move a]
hanoiN 0 _  = []
hanoiN 1 (a:b:_) = [(a,b)]
hanoiN n (a:b:c:rest) = concat [ step1 bestK, step2 bestK, step3 bestK]
    where 
        step1 k = hanoiN k (a:c:b:rest)
        step2 k = hanoiN (n-k) (a:b:rest)
        step3 k = hanoiN k (c:b:a:rest)
        Just (_,bestK) = bigT n (length rest + 3)
