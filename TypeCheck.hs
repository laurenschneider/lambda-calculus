module TypeCheck (
  check
) where

import Control.Monad.Except
import Control.Monad.Reader

-- NOTE: this file is not working 

-- syntax
data Type
  = Bool
  | Nat
  | Arrow Type Type
  deriving (Eq, Show)

--type tEnv = [(String, Type)]

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope String

type Check = ExceptT TypeError (Reader [(String, Type)])

check :: Expr -> Check Type
check expr = case expr of
  Int a -> Nat

  Var x -> 0 -- lookupVar x

  Apply e1 e2 -> do
    let t1 = check e1
    let t2 = check e2
    case t1 of
      (Arrow x y)
        | x == t2 -> return y
        | _ -> throwError Mismatch t2 x
      t -> throwError NotFunction t

  Lambda x e -> 0
