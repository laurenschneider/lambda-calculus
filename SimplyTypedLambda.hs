
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Control.Monad.Except
import Control.Monad.Reader

type Env = Map.Map String Val

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Env -> String -> Val -> Env
extendEnv e k v = Map.insert k v e

data Val
  = IntVal Integer
  | VarVal String
  | ClosVal String Expr Env

instance Show Val where
  show (IntVal x) = show x
  show (VarVal x) = show x
  show ClosVal{} = "closure"

-- syntax

data Expr
  = Var String
  | Int Integer
  | Apply Expr Expr
  | Lambda String Expr
  deriving (Eq, Show)

-- todo: add Bool as type
data Type
  = Nat
  | Arrow Type Type
  deriving (Eq, Show)

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  -- | NotInScope String


-- type checking

type Check a = Except TypeError a    --(Reader [(String, Type)])

check :: Expr -> Either TypeError Type
check = runExcept . getType

getType :: Expr -> Check Type
getType expr = case expr of
  Int a -> return Nat

  Var x -> return Nat -- lookup Var x in env

  Apply e1 e2 -> do
    t1 <- getType e1
    t2 <- getType e2
    case t1 of
      (Arrow x y) | x == t2 -> return y
                  | otherwise -> throwError (Mismatch t2 x)
      t -> throwError (NotFunction t)

  Lambda x e -> return (Arrow Nat Nat)


-- evaluate

eval :: Expr -> Env -> Val
eval expr env = case expr of
  Int a -> IntVal a

  -- look up the string x in the env and return that value, or the variable
  Var x -> evalVar (VarVal x) (Map.lookup x env)

  Apply e1 e2 -> apply (eval e1 env) (eval e2 env)

-- lambda returns a Closure
  Lambda x e -> ClosVal x e env


apply :: Val -> Val -> Val
apply (ClosVal s e env) v = eval e (extendEnv env s v)
apply _ _ = error "Must apply a closure value"

evalVar :: Val -> Maybe Val -> Val
evalVar _ (Just a) = a
evalVar x Nothing = x


--------------------
-- testing evaluate
--------------------

-- ID  \ x.x 3 = 3
xId = Lambda "x" (Var "x")
applyId = Apply xId (Int 3)
evalId = eval applyId emptyEnv


-- Fst:  \ xy.x 1111 2 = 1111
first = Lambda "y" (Lambda "x" (Var "y"))
appFst = Apply (Apply first (Int 1111)) (Int 2)
evalFst = eval appFst emptyEnv


-- Snd  \ xy.y 1111 2 = 2
second = Lambda "y" (Lambda "x" (Var "x"))
appSnd = Apply (Apply second (Int 1111)) (Int 2)
evalSnd = eval appSnd emptyEnv


-- shadowing \ xx.x 1111 2 = 2
shadow = Lambda "x" (Lambda "x" (Var "x"))
appShadow = Apply (Apply shadow (Int 1111)) (Int 2)
evalShadow = eval appShadow emptyEnv


-- rename \ xy.x y 1111 = y
rename = Lambda "y" (Lambda "x" (Var "y"))
appRename = Apply (Apply rename (Var "y")) (Int 1111)
evalRename = eval appRename emptyEnv
