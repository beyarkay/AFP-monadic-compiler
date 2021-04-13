G52AFP Coursework 2 - Monadic Compiler
Your full name(s) - Thomas Cotter, Ray Garner
Your full email address(es) - psytc8@nottingham.ac.uk, psyrg4@nottingham.ac.uk

--------------------------------------------------------------------------------

Imperative language:

> data Prog = Assign Name Expr
>           | If Expr Prog Prog
>           | While Expr Prog
>           | Seqn [Prog]
>             deriving Show
>
> data Expr = Val Int | Var Name | App Op Expr Expr
>             deriving Show
>
> type Name = Char
>
> data Op   = Add | Sub | Mul | Div
>             deriving Show

Factorial example:

> fac :: Int -> Prog
> fac n = Seqn [Assign 'A' (Val 1),
>               Assign 'B' (Val n),
>               While (Var 'B') (Seqn
>                  [Assign 'A' (App Mul (Var 'A') (Var 'B')),
>                   Assign 'B' (App Sub (Var 'B') (Val 1))])]

> test :: Expr
> test = App Mul (App Add (Val 1) (Val 2)) (App Add (Val 3) (Val 4))


Virtual machine:

> type Stack = [Int]
>
> type Mem   = [(Name,Int)]
>
> type Code  = [Inst]
> 
> data Inst  = PUSH Int
>            | PUSHV Name
>            | POP Name
>            | DO Op
>            | JUMP Label
>            | JUMPZ Label
>            | LABEL Label
>              deriving Show
> 
> type Label = Int

State monad:

> type State = Label
>
> newtype ST a = S (State -> (a, State))
>
> app :: ST a -> State -> (a,State)
> app (S st) x 	=  st x
>
> instance Functor ST where
>    -- fmap :: (a -> b) -> ST a -> ST b
>    fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))
>
> instance Applicative ST where
>    -- pure :: a -> ST a
>    pure x = S (\s -> (x,s))
>
>    -- (<*>) :: ST (a -> b) -> ST a -> ST b
>    stf <*> stx = S (\s ->
>       let (f,s')  = app stf s
>           (x,s'') = app stx s' in (f x, s''))
>
> instance Monad ST where
>    -- return :: a -> ST a
>    return x = S (\s -> (x,s))
>
>    -- (>>=) :: ST a -> (a -> ST b) -> ST b
>    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

> comp :: Prog -> Code
> comp p = c
>          where
>             (c,_) = app (comprog' p) 0

> compexpr :: Expr -> Code
> compexpr (Val n) = [PUSH n]
> compexpr (Var x) = [PUSHV x]
> compexpr (App o x y) = compexpr x ++ compexpr y ++ [DO o]                                      


> comprog :: Prog -> Label -> (Code, Label)
> comprog (Assign n e) l = (compexpr e ++ [POP n], l)
> comprog (While e p) l = ([LABEL l] ++ compexpr e ++ [JUMPZ (l+1)] ++ cp ++ [JUMP l, LABEL (l+1)], lp)
>                         where
>                           (cp, lp) = comprog p (l+2)
> comprog (Seqn []) l = ([],l)
> comprog (Seqn (p:ps)) l = (c ++ c', l'')
>                           where
>                            (c,l') = comprog p l
>                            (c',l'') = comprog (Seqn ps) l' 
> comprog (If e p1 p2) l = (compexpr e ++ [JUMPZ (l+1), JUMP l, LABEL (l+1)] ++ cp2 ++ [JUMP (l+2), LABEL l] ++ cp1 ++ [LABEL (l+2)], lp1)
>                          where
>                           (cp2, lp2) = comprog p2 (l+2)
>                           (cp1, lp1) = comprog p1 lp2

> nextLabel :: ST Label
> nextLabel = S (\n -> (n, n+1))

> incLabel :: Int -> ST Label
> incLabel i = S (\n -> (n, n+i))

                      State -> (Code, State)
                      Label -> (Code, Label)

> comprog' :: Prog -> ST Code
> comprog' (Assign n e) = return (compexpr e ++ [POP n]) 
> comprog' (While e p) = do l <- nextLabel
>                           l' <- nextLabel
>                           cp <- comprog' p
>                           return ([LABEL l] ++ compexpr e ++ [JUMPZ l'] ++ cp ++ [JUMP l, LABEL l'])

> comprog' (If e p1 p2) = do l <- nextLabel
>                            l' <- nextLabel
>                            l'' <- nextLabel
>                            cp1 <- comprog' p1
>                            cp2 <- comprog' p2
>                            return (compexpr e ++ [JUMPZ l', JUMP l, LABEL l'] ++ cp2 ++ [JUMP l'', LABEL l] ++ cp1 ++ [LABEL l''])
> comprog' (Seqn []) = return []
> comprog' (Seqn (p:ps)) = do c <- comprog' p
>                             c' <- comprog' (Seqn ps)
>                             return (c ++ c')

> exec :: Code -> Mem
> exec c = m 
>          where
>              (m,_,_) = exec' ([],[],c) c

> exec' :: (Mem,Stack,Code) -> Code -> (Mem,Stack,Code)
> exec' (m,s,[]) c = (m,s,[])
> exec' (m,s,(PUSH i):is) c    = exec' (m,i:s,is) c
> exec' (m,s,(PUSHV n):is) c   = exec' (m,(getVarData n m):s,is) c
> exec' (m,h:s,(POP n):is) c   = exec' (setVarData n h m,s,is) c
>                              -- | otherwise = exec' ((n,h):m,s,is) c
> exec' (m,y:x:s,(DO op):is) c = exec' (m,(execOp op x y):s,is) c
> exec' (m,s,(JUMP l):is) c    = exec' (m,s,jumpToLabel c l) c
> exec' (m,h:s,(JUMPZ l):is) c | h == 0 = exec' (m,s,jumpToLabel c l) c
>                              | otherwise = exec' (m,s,is) c
> exec' (m,s,(LABEL l):is) c = exec' (m,s,is) c

> getVarData :: Name -> Mem -> Int
> getVarData _ [] = -1
> getVarData n m = head [d | (n',d) <- m, n'==n]

> setVarData :: Name -> Int -> Mem -> Mem
> setVarData n i [] = (n,i) : []
> setVarData n i ((n',i'):vs) | n == n' = (n',i) : vs
>                             | otherwise = (n',i') : setVarData n i vs
                        
> execOp :: Op -> Int -> Int -> Int
> execOp Add x y = x+y
> execOp Sub x y = x-y
> execOp Mul x y = x*y
> execOp Div x y = x `div` y

> jumpToLabel :: Code -> Label -> Code
> jumpToLabel [] _ = [] 
> jumpToLabel (LABEL i:is) l | i == l = is
>                            | otherwise = jumpToLabel is l
> jumpToLabel (i:is) l = jumpToLabel is l

--------------------------------------------------------------------------------
