import qualified Data.Set as Set

--Normal-order Evaluator of λ-terms

type Symbol = String
data Expr = Var Symbol | App Expr Expr | Lambda Symbol Expr deriving Eq

eval :: Expr -> Expr
eval var = if var == reduce var 0 then var else eval (reduce var 0)


reduce :: Expr -> Int -> Expr
reduce (Var s) n = Var s
reduce (Lambda s e) n = Lambda s (reduce e n)
reduce (App e1 e2) n = pup e1 e2 n

pup::Expr->Expr->Int->Expr
pup (Lambda s e) e2 n = substitute e s e2 n --reduce
pup e1 e2 n = if red == e1 then App e1 (reduce e2 n) else App red e2 where red = reduce e1 n--App

substitute:: Expr->Symbol->Expr->Int->Expr
substitute (Var s) var val n = if s == var then val else Var s 
substitute (App e1 e2) var val n = App (substitute e1 var val n) (substitute e2 var val n)
substitute (Lambda s e) var val n 
    | s == var = Lambda s e 
    | Set.member s (free val s) = Lambda  (asymbols !! n) (substitute (substitute e s (Var (asymbols !! n)) n) var val (n+1)) 
    | otherwise = Lambda s (substitute e var val n)

free::Expr -> Symbol -> Set.Set Symbol
free (Var s) t = Set.fromList [s]
free (App e1 e2) t = Set.union (free e1 t)  (free e2 t)
free (Lambda s e) t =  Set.delete s (free e t)

instance Show Expr where
    show (Var x) = x
    show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
    show (Lambda x y) = "(\\" ++ x ++ "." ++ show y ++ ")"

symbols :: [Symbol]
symbols = [ show x | x <- [0..]]
asymbols = map ("a" ++) symbols

