-- const = λxy.x
-- in haskell:
-- const = \x y -> x

import qualified Data.Map as Map
import Data.Maybe as Maybe

--type Env k v = [(k,v)]
-- can use lookup, can extend with cons

type Env = Map.Map String Val

data Closure = Closure String Expr Env

data Val
  = IntVal Integer
  | ClosVal Closure

data Expr
  = Var String
  | Apply Expr Expr
  | Lambda String Expr
  deriving (Eq, Show)


-- lambda returns a Closure

eval :: Expr -> Env -> Val
eval expr env = case expr of

  -- look up the string x in the env and return that value
  Var x -> Maybe.fromJust (Map.lookup x env)

  Apply e1 e2 -> IntVal 0

  Lambda x e -> ClosVal (Closure x e env)
