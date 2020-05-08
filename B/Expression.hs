module Expression where

data Expr   = Abstr String Expr
            | Appl Expr Expr
            | Var String
            | Defer String Expr
            deriving (Eq)

instance Show Expr where
    show (Abstr x e)    = "(\\" ++ x      ++ "." ++ show e ++ ")"
    show (Appl a b)     = "("   ++ show a ++ " " ++ show b ++ ")"
    show (Var x)        = x
    show (Defer _ e)    = show e
