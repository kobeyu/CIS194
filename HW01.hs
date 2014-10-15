module HW01(validate,hanoi,hanoiN) where
import Data.List(unfoldr)
import Data.Tuple(swap)
import Data.List(minimumBy)
import Data.Function(on)

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
hanoiN :: Integer -> [a] -> [Move a]
hanoiN 0 _  = []
hanoiN 1 (a:b:_) = [(a,b)]
hanoiN n (a:b:c:rest) = concat [ step1 bestK, step2 bestK, step3 bestK]
    where 
        step1 k = hanoiN k (a:c:b:rest)
        step2 k = hanoiN (n-k) (a:b:rest)
        step3 k = hanoiN k (c:b:a:rest)
        bigT k = 2 * length (step1 k) + length (step2 k) 
        bestK :: Integer
        bestK = if null rest 
            then n - 1 
            else fromIntegral . minimumBy (compare `on` bigT) $  [1..n-1]