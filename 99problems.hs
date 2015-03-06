data NestedList a = Elem a | List [NestedList a]
                    deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

compress :: (Eq a) => [a] -> [a]
compress (x:y:xs) = if x == y then compress (y:xs) else x:compress (y:xs)
compress [x] = [x]
compress [] = []

pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' [x] = [[x]]
pack' (x:xs) = if x `elem` (head (pack' xs))
               then (x:head (pack' xs)):(tail (pack' xs))
               else [x]:(pack' xs)

encode' :: (Eq a) => [a] -> [(a, Int)]
encode' [] = []
encode' [x] = [(x, 1)]
encode' (x:xs) = if x == fst (head (encode' xs))
                 then (x, (snd (head (encode' xs))) + 1):(tail (encode' xs))
                 else (x, 1):(encode' xs)

data Element a = Single a
               | Multiple a Int
                 deriving (Show, Eq)

--addElement (Single x) y = if x == y 
--                          then Multiple x 2
--                          else [(Single y), (Single x)]

encodeModified :: (Eq a) => [a] -> [Element a]
encodeModified = map encodehelper . encode'
                  where
                      encodehelper (x, 1) = Single x
                      encodehelper (x, n) = Multiple x n

decodeModified :: [Element a] -> [a]
decodeModified [] = []
decodeModified (y:ys) =  decodehelper y ++ decodeModified ys
               where
                 decodehelper (Multiple x n) = take n (repeat x) --if n == 2 then x:(decodehelper (Single x)) else x:(decodehelper (Multiple x (n-1)))
                 decodehelper (Single x) = [x]

encodeDirect :: (Eq a) => [a] -> [Element a]
encodeDirect [] = []
encodeDirect (x:xs) = (encodehelper x ((length (takeWhile (==x) xs)) + 1)):(encodeDirect (dropWhile (==x) xs))
               where
                 encodehelper y n = if n==1 then (Single y) else (Multiple y n)

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x:duplicate xs

replicate' :: [a] -> Int ->  [a]
replicate' [] _ = []
replicate' x 1 = x
replicate' _ 0 = []
replicate' (x:xs) n = take n (repeat x) ++ (replicate' xs n)

drop' :: [a] -> Int -> [a]
drop' [] _ = []
drop' x n = take (n-1) x ++ (drop' (drop n x) n)

drop2 :: [a] -> Int -> [a]
drop2 x n = drophelper 1 n x
       where
            drophelper _ _ [] = []
            drophelper count n (y:ys) = if count == n  then drophelper 1 n ys else y:drophelper (count+1) n ys

split' :: [a] -> Int -> ([a],[a])
split' [] _ = ([],[])
split' (x:xs) n 
    | n == 0 = ([], x:xs)
    | otherwise = (x:ys, zs)
    where (ys, zs) = split' xs (n-1)

slice' :: [a] -> Int -> Int -> [a]
slice' [] _ _ = []
--slice' x m n = fst (split' (snd split' x m) (n-m))
slice' (x:xs) m n
     | (m <= 0) && (n >= 0) = x:slice' xs (m-1) (n-1)
     | (m > 0) && (n > 0)= slice' xs (m-1) (n-1)
     | otherwise = []

rotate' :: [a] -> Int -> [a]
rotate' [] _ = []
rotate' x n 
     | n == 0 = x
     | n > 0 = rotate' (tail x ++ [head x]) (n-1)
     | otherwise = rotate' ([last x] ++ init x) (n+1)

removeAt :: [a] -> Int -> ([a], [a])
removeAt [] _ = ([], [])
removeAt (x:xs) n 
     | n < 1 = ([], (x:xs))
     | n == 1 = ([], xs)
     | otherwise = (x:ys, zs)
   where (ys, zs) = removeAt xs (n-1)
    
insertAt :: a -> [a] -> Int -> [a]
insertAt y [] _ = [y]
insertAt y (x:xs) n
     | n == 0 = y:x:xs
     | otherwise = x:insertAt y xs (n-1)

range' :: Integer -> Integer -> [Integer]
range' m n
     | m == n = [m]
     | m < n = m: range' (m+1) n
     | otherwise = m:range' (m-1) n
