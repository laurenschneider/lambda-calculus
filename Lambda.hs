-- const = λxy.x
-- in haskell:
-- const = \x y -> x

import qualified Data.Map as Map
import Data.Maybe as Maybe

--type Env k v = [(k,v)]
-- can use lookup, can extend with cons

type Env = Map.Map String Val

--environ :: String -> Val -> Env
--environ k v = Map.fromList [(k,v)]

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Env -> String -> Val -> Env
extendEnv e k v = Map.insert k v e

--data Closure = Closure String Expr Env

data Val
  = IntVal Integer
  | ClosVal String Expr Env

instance Show Val where
  show (IntVal x) = show x
  show ClosVal{} = "closure"

data Expr
  = Var String
  | Int Integer
  | Apply Expr Expr
  | Lambda String Expr
  deriving (Eq, Show)


-- lambda returns a Closure

eval :: Expr -> Env -> Val
eval expr env = case expr of
  Int a -> IntVal a

  -- look up the string x in the env and return that value
  Var x -> Maybe.fromJust (Map.lookup x env)

  Apply e1 e2 -> apply (eval e1 env) (eval e2 env)

  Lambda x e -> ClosVal x e env


apply :: Val -> Val -> Val
apply (ClosVal s e env) v = eval e (extendEnv env s v)
apply _ _ = error "Must apply a closure value"

--------------------
-- testing evaluate
--------------------

-- ID
xId = Lambda "x" (Var "x")
applyId = Apply xId (Int 3)
evalId = eval applyId emptyEnv


-- Fst
first = Lambda "y" (Lambda "x" (Var "y"))
appFst = Apply (Apply first (Int 1111)) (Int 2)
evalFst = eval appFst emptyEnv
