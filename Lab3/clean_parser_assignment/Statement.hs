module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Read String |
    Write Expr.T |
    While Expr.T Statement |
    Begin [Statement] |
    Repeat Statement Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

-------------------------CONSTRUCTORS---------------------
skip = accept "skip" #- require ";">-> const Skip

read = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e1,s1) = While e1 s1

ifStatement = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e1, s1), s2) = If e1 s1 s2

begin = accept "begin" -# iter parse #- require "end">-> Begin

repeat = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> buildRepeat
buildRepeat(s,e) = Repeat s e
----------------------------------------------------------
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
------------------EXECS-------------------------
exec (Skip:stmts) dict input = [] ++ exec (stmts) dict input

exec (Read s1 : stmts) dict (i:input) = case Dictionary.insert (s1,i) dict of
    newDict -> exec stmts newDict input

exec (Assignment str e1:stmts) dict input = exec stmts ( Dictionary.insert (str, Expr.value e1 dict) dict ) input

exec (Write e1 : stmts) dict input = (Expr.value e1 dict) : exec stmts dict input

exec (While e1 s1 :stmts) dict input =
    if(Expr.value e1 dict)>0
    then exec (s1:(While e1 s1):stmts) dict input
    else exec (stmts) dict input

exec (Begin list :stmts) dict input = exec (list++stmts) dict input

exec (Repeat s e :stmts) dict input =
    if(Expr.value e dict)<0
    then exec (s:(Repeat s e):stmts) dict input
    else exec (s:stmts) dict input

instance Parse Statement where
  parse = assignment ! skip ! Statement.read ! write ! while ! begin ! ifStatement ! Statement.repeat
  toString = show
