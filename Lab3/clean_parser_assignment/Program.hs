--Tom Brander (tombra-7)
module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program p) = concatMap Statement.toString p

exec (Program p) = Statement.exec p Dictionary.empty
