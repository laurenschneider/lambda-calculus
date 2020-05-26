import Control.Monad.Except
import Control.Monad.Reader


data Type
  = Bool
  | Nat
  | Arrow Type Type
  deriving (Eq, Show)

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope Name

type Check = ExceptT TypeError (Reader Env)

check :: Expr -> Check Type
check expr = case expr of
  Int a -> Nat

  Var x -> 0 -- lookupVar x

  Apply e1 e2 -> do
    t1 = check e1
    t2 = check e2
    case t1 of
      (Arrow x y)
        | x == t2 -> return y
        | _ -> throwError Mismatch t2 x
      t -> throwError NotFunction t

  Lambda x e -> 0
