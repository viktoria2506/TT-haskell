module Main where


import Data.Maybe


import Parser
import Expression


data Reduct = NormRed | NoRed | DefferedRed String Expr deriving (Eq)


solve :: Expr -> [Expr] -> Int -> Int -> Int -> [Expr]
solve e ans m = solveHelper (find e m Nothing) e ans m
    where
        solveHelper :: (Reduct, Expr) -> Expr -> [Expr] -> Int -> Int -> Int -> [Expr]
        solveHelper (_, n) _ ans 0 _ 0 = n : ans
        solveHelper _ _ ans 0 _ _      = ans
        solveHelper (NoRed, n) _ ans _ k i
            | i == k - 1               = ans                            
            | otherwise                = n : ans
        solveHelper (_, n) e ans m k 0 = solve n (n : ans) (m - 1) k (k - 1)
        solveHelper (_, n) e ans m k i = solve n ans       (m - 1) k (i - 1)

        processReductHelper :: Expr -> String -> Expr -> Expr
        processReductHelper (Abstr a b) x en               = Abstr a (processReductHelper b x en)
        processReductHelper (Appl a b) x en                = Appl (processReductHelper a x en) (processReductHelper b x en)
        processReductHelper (Var nm) _ _                   = Var nm
        processReductHelper (Defer a e) x en | a == x      = Defer a en
                                  | otherwise   = Defer a (processReductHelper e x en)

        processReduct :: Expr -> Int -> Maybe String -> Expr -> (Reduct, Expr) -> (Reduct, Expr)
        processReduct q i m e (NoRed, _)           = let (r, n) = find q i m in (r, Appl e n)
        processReduct q i m e (NormRed, n)         = (NormRed, Appl n q)
        processReduct q i m e (d@(DefferedRed x r), n)  = (d, Appl n (processReductHelper q x r))

        processM :: Maybe String -> Expr -> (Reduct, Expr)
        processM (Just v) r = (DefferedRed v r, r)
        processM Nothing  r = (NormRed, r)

        matchAbstr :: Expr -> Maybe Expr
        matchAbstr e@(Abstr _ _)  = Just e
        matchAbstr (Defer _ e)    = matchAbstr e
        matchAbstr _              = Nothing

        find :: Expr -> Int -> Maybe String -> (Reduct, Expr)
        find (Var nm) _ _          = (NoRed, Var nm)
        find (Abstr a b) i m          = let (r, ne) = find b i m in (r, Abstr a ne) 
        find (Appl e@(Defer _ _) q) i m = case matchAbstr e of 
                                                (Just (Abstr a ee)) -> processM m $ rename (del ee) a q i 0
                                                Nothing             -> processReduct q i m e $ find e i m
        find (Appl (Abstr a b) c) i m = processM m $ rename (del b) a c i 0
        find (Appl a b) i m           = processReduct b i m a $ find a i m 
        find (Defer x e) i _          = case find e i (Just x) of
                                                (DefferedRed n nn, rs)     -> if n == x then (DefferedRed x rs, Defer x rs) else (DefferedRed n nn, rs)
                                                (r, rs)                    -> (r, Defer x rs)
        
        del :: Expr -> Expr
        del (Appl a b)          = Appl (del a) (del b)
        del e@(Var _)           = e
        del (Abstr v e)         = Abstr v (del e)
        del (Defer _ e)         = del e

        renameHelper :: Expr -> String -> String -> Expr
        renameHelper (Var nm)      t name    = if nm == t then Var name else Var nm
        renameHelper (Appl a b)    t name    = Appl (renameHelper a t name) (renameHelper b t name)
        renameHelper (Abstr a b)   t name    = if a == t then (Abstr a b) else Abstr a (renameHelper b t name)
        renameHelper other _ _               = other

        rename :: Expr -> String -> Expr -> Int -> Integer -> Expr
        rename e@(Appl a b)  x n m i = Appl (rename a x n m i) (rename b x n m i)
        rename e@(Var nm)    x n i _ = if nm == x then (Defer ("l" ++ show i) n) else e
        rename e@(Defer _ _) _ _ _ _ = e
        rename e@(Abstr a b) x n m i = if a == x 
                                     then e 
                                     else let name = "m" ++ show m ++ "v" ++ show i
                                              bb = renameHelper b a name
                                          in Abstr name (rename bb x n m (i + 1))


main :: IO ()
main = do
    line <- getLine
    let [m, k] = map (read :: String -> Int) $ words line
    contents <- getContents
    let e = parse contents
    mapM_ print $ reverse (solve e [e] (m - 1) k (k - 1))
