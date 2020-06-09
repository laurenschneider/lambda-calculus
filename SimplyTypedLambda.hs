
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Control.Monad.Except
import Control.Monad.Reader

type Env = Map.Map String Val

-- syntax

data Val
  = IntVal Integer
  | VarVal String
  | ClosVal String Expr Env

instance Show Val where
  show (IntVal x) = show x
  show (VarVal x) = show x
  show ClosVal{} = "closure"

data Expr
  = Var String
  | Int Integer
  | Apply Expr Expr
  | Lambda String Type Expr
  deriving (Eq)

instance Show Expr where
  show (Var x) = show x
  show (Int i) = show i
  show (Apply e1 e2) = "@ " ++ show e1 ++ " " ++ show e2
  show (Lambda x t e) = "\\" ++ show x ++ ":" ++ show t ++ "." ++ show e

data Type
  = Nat
  | Arrow Type Type
  deriving (Eq)

instance Show Type where
  show Nat = "Natural number"
  show (Arrow t1 t2) = show t1 ++ " -> " ++ show t2

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope String
  deriving (Show)


-- type checking

type TypeEnv = Map.Map String Type

emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.empty

extendTypeEnv :: TypeEnv -> String -> Type -> TypeEnv
extendTypeEnv e k v = Map.insert k v e

type Check a = Except TypeError a

check :: TypeEnv -> Expr -> Either TypeError Type
check e x = runExcept (getType e x)

getType :: TypeEnv -> Expr -> Check Type
getType env expr = case expr of
  Int a -> return Nat

  Var x -> checkVar env x

  Apply e1 e2 -> do
    t1 <- getType env e1
    t2 <- getType env e2
    case t1 of
      Arrow x y -> checkArrow x y t2
      t -> throwError (NotFunction t)

  Lambda x t e -> do
    res <- getType (extendTypeEnv env x t) e
    return (Arrow t res)

checkVar :: TypeEnv -> String -> Check Type
checkVar e x = case Map.lookup x e of
  Just a -> return a
  Nothing -> throwError (NotInScope x)

checkArrow :: Type -> Type -> Type -> Check Type
checkArrow a1 a2 t = if a1 == t then return a2 else throwError (Mismatch a1 t)


-- evaluate

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Env -> String -> Val -> Env
extendEnv e k v = Map.insert k v e

eval :: Expr -> Env -> Val
eval expr env = case expr of
  Int a -> IntVal a

  -- look up the string x in the env and return that value, or the variable
  Var x -> evalVar (VarVal x) (Map.lookup x env)

  Apply e1 e2 -> apply (eval e1 env) (eval e2 env)

  -- lambda returns a Closure
  Lambda x _ e -> ClosVal x e env


apply :: Val -> Val -> Val
apply (ClosVal s e env) v = eval e (extendEnv env s v)
apply _ _ = error "Must apply a closure value"

evalVar :: Val -> Maybe Val -> Val
evalVar _ (Just a) = a
evalVar x Nothing = x


-- examples: validate some types

x = Var "x"
checkForError = check emptyTypeEnv x
xEnv = extendTypeEnv emptyTypeEnv "x" Nat
checkForNat = check xEnv x

-- ID  \ x.x 3 = 3
xId = Lambda "x" Nat x
checkForArr = check xEnv xId
appX = Apply xId (Int 2)
checkAppX = check xEnv appX

f = Lambda "f" (Arrow Nat Nat) x
checkF = check xEnv f

checkLit = check emptyTypeEnv (Int 2)

appMismatch = Apply f (Int 2)
checkMismatch = check xEnv appMismatch
