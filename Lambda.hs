
--type Name = String

data Expr
  = Var String
  | Apply Expr Expr
  | Lamda String Expr
  deriving (Eq, Show)

-- const = λxy.x
-- in haskell:
-- const = \x y -> x

type Closure = Closure String Expr

-- need to define some kind of environment

-- lambda returns a Closure

eval :: Expr Env -> Integer
eval expr = case expr of

  Var x e -> x  -- look up the string x in the env and return that value

  Apply a b e -> 0

  Lambda x a e -> Closure x a
