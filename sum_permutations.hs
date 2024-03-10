{-  define a function:
solve :: [ Int ] → Int → [ Expr ]
that returns all expressions whose list of values is a permutation of the given list and whose value is the given value. -}

data Expr = Val Int | App Op Expr Expr deriving Show
data Op = Add | Mul deriving Show

delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete n (x:xs)
  | n == x    = xs
  | otherwise = x : delete n xs

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = [x:ys | x <- xs, ys <- perms (delete x xs)]


eval :: Expr -> Int
eval (Val n)      = n
eval (App Add x y) = eval x + eval y
eval (App Mul x y) = eval x * eval y

values :: Expr -> [Int]
values (Val n)      = [n]
values (App _ x y)   = values x ++ values y

split :: [Int] -> [([Int], [Int])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs xs  = [App op l r | (ls, rs) <- split xs,
                          l <- exprs ls,
                          r <- exprs rs,
                          op <- [Add, Mul]]

solve :: [Int] -> Int -> [Expr]
solve xs n = [e | ns <- perms xs, e <- exprs ns, eval e == n]
