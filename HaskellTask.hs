import Data.Binary.Builder (append)
import Data.Char
import Data.List (intercalate)
--Сheck if the number is equal to the sum of the numbers to the power of their number 
main :: IO () 
main = do 
        putStrLn "Enter number:" 
        av <- getLine 
        let a =  read av :: Int;
        check a av av (tt (filtfun av))

fun lst n =(sum (map(\x -> x^n) lst))

check num av l lst = if num == (fun lst (length lst)) then putStrLn $ "Yes, " ++ l ++ " = "++ sus av else putStrLn $ "No, "++ l ++ " /= "  ++ sus av

--Make str--
dostr lst = unwords["^" ++ (show (length lst)), "+ "]

dolaststr lst = unwords["^"++(show (length lst))]

sus lst =  (intercalate (dostr lst) (filtfun lst)) ++ (dolaststr lst)
----
subsequences            :: [a] -> [[a]]
subsequences xs         =  [] : nonEmptySubsequences xs

nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r
----
filtfun a = filter (\x -> (length x == 1)) (subsequences a)

pp :: [Char] -> Char
pp [a] = a--["2","3","4"]

tt lst= (map (\x -> digitToInt (pp x)) lst)