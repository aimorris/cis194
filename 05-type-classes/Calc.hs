import ExprT ( ExprT(..) )
import Parser ( parseExp )

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  mul = Mul
  add = Add

instance Expr Integer where
  lit = id
  mul = (*)
  add = (+)

instance Expr Bool where
  lit = (<) 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer
  deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y ) = MinMax $ min x y

newtype Mod7 = Mod7 Integer
  deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)