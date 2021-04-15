-- Code to Haskell lab assignment 2 in the course D7012E by HÃ¥kan Jonsson
--Tom Brander (tombra-7) 
import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App func value) = func ++ "(" ++ unparse value ++ ")" 

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env

eval (App "sin" e1) env = sin(eval e1 env)                                              ------------------------------------------------- TASK 1
eval (App "cos" e1) env = cos(eval e1 env)                                              ------------------------------------------------- TASK 1
eval (App "exp" e1) env = exp(eval e1 env)                                              ------------------------------------------------- TASK 1
eval (App "log" e1) env = log(eval e1 env)                                              ------------------------------------------------- TASK 1

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)

diff v (App "sin" e1) = Op "*" (diff v e1) (App "cos" e1)                               ------------------------------------------------- TASK 1
diff v (App "cos" e1) = Op "-" (Const 0) (Op "*" (diff v e1) (App "sin" e1))            ------------------------------------------------- TASK 1
diff v (App "exp" e1) = Op "*" (diff v e1) (App "exp" e1)                               ------------------------------------------------- TASK 1
diff v (App "log" e1) = Op "*" (diff v e1) (Op "/" (Const 1) (e1))                      ------------------------------------------------- TASK 1
diff _ _ = error "can not compute the derivative"

simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
simplify (App func value) =
  let(values) = (simplify value) in
    case (func, values) of
      ("sin", Const 0) -> Const 0                                                       ------------------------------------------------- TASK 1
      --("sin", pi) -> Const 1                                                          ------------------------------------------------- TASK 1
      ("cos", Const 0) -> Const 1                                                       ------------------------------------------------- TASK 1
      --("cos", pi) -> Const 0                                                          ------------------------------------------------- TASK 1
      ("exp", Const 0) -> Const 1                                                       ------------------------------------------------- TASK 1
      ("log", Const 0) -> error "Illegal: Log of 0"                                     ------------------------------------------------- TASK 1
      (func, values)   -> App func values


--------------------------TASK 2-----------------------------
mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (body,var) = \x -> eval body [((unparse var),x)]


--------------------------TASK 3-----------------------------
findzero :: String -> String -> Float -> Float
findzero s1 s2 x0 = findzeroHelper 
                      (mkfun ((parse s2),(parse s1)))                               --f
                          (mkfun ((diff (parse s1) (parse s2)),(parse s1)))         --f' 
                            (x0)                                                    --x0

findzeroHelper :: (Float->Float)->(Float-> Float)->Float->Float
findzeroHelper f f' x
  |x-xn<0.0001      = xn
  |otherwise        = findzeroHelper f f' xn
    where xn = x - ((f x)/(f' x))
----------------------------TESTS-------------------------------
--Test 1
--unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))")))

--Test 2
--mkfun (parse "x*x+2", Var "x")

--Test 3
--findzero "x" "x*x*x+x-1" 1.0                0.68232775
--findzero "y" "cos(y)*sin(y)" 2.0            1.5707964 