module ASTInterpreter where

import Ast
import StatefulUnsafeMonad
import Prelude hiding (lookup)
import Data.Map(Map, lookup, insert, empty, fromList)  -- for State
import CParser
import ParserMonad
import TestsProject

-- TO EVALUATE AN AST
--     testEval (program)



type State = (Map String Integer,Program,[String])
ex1 = "def main() {x=1;print x;y=2;print y;z=f();print z;return 0;} def f(){f=123;print f;return f;}"
testEval :: String -> [String]
testEval p = eval $ (\(Just(x,y))->x) $ parse parser p
                            
eval :: Program -> [String]
eval p = let mainLoc = getFunc "main" p
         in case (mainLoc) of
              (Just x) -> getPrints $ (\(x,y) -> y) $ app (eval' x) (Data.Map.empty,p,[])
              (Nothing) -> ["Program does not have a main function!"]
--getPrints $ (\(x,y) -> y) $ app (eval' ((\(Just x)->x)(getFunc "main" p))) (Data.Map.empty,p,[])
--
getPrints :: State -> [String]
getPrints (m,p,s) = s
--
getFunc :: String -> Program -> Maybe Stmnt
getFunc n [] = Nothing
getFunc n ((Def id args body):fs) = if(n==id)then Just (Def id args body) else getFunc n fs
--
withVal :: String -> Integer -> StatefulUnsafe State a -> StatefulUnsafe State a
withVal var v comp = StatefulUnsafe $ \ (env,p,s) -> let newEnv = insert var v env
                                                     in app comp (newEnv,p,s)
--
addString :: String -> StatefulUnsafe State ()
addString str = StatefulUnsafe $ \ (env,p,s) -> (Ok (), (env,p,(s++[str])))

addStrings :: [String] -> StatefulUnsafe State ()
addStrings (x:xs) = do addString x
                       addStrings xs
addStrings [] = return ()
                       
--
setVal :: String -> Integer ->StatefulUnsafe State ()
setVal var i = StatefulUnsafe $ \ (env,p,s) -> (Ok () , ((insert var i env),p,s))
--
valOf :: String -> StatefulUnsafe State Integer
valOf var = do (env,p,s) <- get
               case (lookup var env) of
                 Nothing -> err $ "ERR: variable" ++ var++"not found"
                 Just a -> return a

establishArgs :: [String] -> [Integer] -> Map String Integer
establishArgs (x:xs) (y:ys) = insert x y (establishArgs xs ys)
establishArgs _ _ = Data.Map.empty
--
eval' :: Stmnt -> StatefulUnsafe State Integer
eval' (Def name args body) = do ans <- eval''' body
                                return ans
eval' (Return e) = do ans <- eval'' e
                      return ans
eval' (Print v) = do a <- valOf v
                     addString (v++" = "++(show a))
                     return a
eval' (Debug s) = do addString (s)
                     return 0
eval' (Assign x y) = do a <- eval'' y
                        setVal x a
                        return a   
eval' (Continue) = do return (-3948692386429386420836)
eval' (Break) = do return (-39486923864293864208364)
eval' (Block st) = do eval''' st   
eval' (While cond body) = do c <- eval'' cond
                             if(c==1)then do flag <- eval' body
                                             if flag==(-39486923864293864208364)
                                             then return 0
                                             else do eval' (While cond body)
                                     else return 0
eval' (DoWhile body cond) = do eval' body
                               c <- eval'' cond
                               if(c==1)then do eval' (DoWhile body cond)
                                       else return 0
eval' (If_Else cond s1 s2) = do c <- eval'' cond
                                if(c==1) then do flag <- eval' s1
                                                 return flag
                                         else do flag <- eval' s2
                                                 return flag
eval' (If cond s1) = do c <- eval'' cond
                        if(c==1) then do flag <- eval' s1
                                         return flag
                                 else return 0
--
evalChelp :: [Expr] -> StatefulUnsafe State [Integer]
evalChelp [] = return []
evalChelp (e:es) = do a <- eval'' e
                      b <- evalChelp es
                      return (a:b)
--
eval'' :: Expr -> StatefulUnsafe State Integer
eval'' (Call fn inputs) = do (env,p,s) <- get
                             inps <- (evalChelp inputs)
                             case (getFunc fn p) of
                               (Just (Def fn args body)) -> case (app (eval''' body) ((establishArgs args inps),p,[])) of 
                                                              ((Ok x),(e1,p1,s1)) -> do addStrings s1
                                                                                        return x
                                                              ((Error s),x) -> err s
                               (Nothing) -> do addString $ "ERR: Function "++ fn++" not found"
                                               err "func not found"
eval'' (Not x) = do a <- eval'' x
                    case a of 0 -> return 1
                              _ -> return 0
eval'' (And x1 x2) = do a <- eval'' x1
                        if(a==0)
                        then return 0
                        else do b <- eval'' x2
                                if(b==1)
                                then return 1
                                else return 0
eval'' (Or x1 x2) =  do a <- eval'' x1
                        if(a==1)
                        then return 1
                        else do b <- eval'' x2
                                if(b==1)
                                then return 1
                                else return 0
                       
eval'' (Lt x1 x2) = do a <- eval'' x1
                       b <- eval'' x2
                       if(a<b) then return 1 else return 0
eval'' (Le x1 x2) = do a <- eval'' x1
                       b <- eval'' x2
                       if(a<=b) then return 1 else return 0
eval'' (Gt x1 x2) = do a <- eval'' x1
                       b <- eval'' x2
                       if(a>b) then return 1 else return 0
eval'' (Ge x1 x2) = do a <- eval'' x1
                       b <- eval'' x2
                       if(a>=b) then return 1 else return 0
eval'' (Ne x1 x2) = do a <- eval'' x1
                       b <- eval'' x2
                       if(a==b) then return 0 else return 1
eval'' (Eq x1 x2) = do a <- eval'' x1
                       b <- eval'' x2
                       if(a==b) then return 1 else return 0
eval'' (ValInt i) = do return $ fromIntegral i
eval'' (Plus x1 x2) = do a <- eval'' x1
                         b <- eval'' x2
                         return $ a + b
eval'' (Minus x1 x2) = do a <- eval'' x1
                          b <- eval'' x2
                          return $ a - b
eval'' (Div x1 x2) = do a <- eval'' x1
                        b <- eval'' x2
                        if (b==0) then err "cannot divide by zero"
                        else return $ a `div` b
eval'' (Mult x1 x2) = do a <- eval'' x1
                         b <- eval'' x2
                         return $ a * b
eval'' (Mod x1 x2) = do a <- eval'' x1
                        b <- eval'' x2
                        return $ a `mod` b
eval'' (Var s) = do a <- valOf s
                    return $ fromIntegral a
--               
--
eval''' :: [Stmnt] -> StatefulUnsafe State Integer
eval''' [x] = do a <- eval' x
                 return a
eval''' (x:xs) = case x of (Return exp) -> do eval' x
                           (Break)      -> do return (-39486923864293864208364)
                           (Continue)   -> do return 0
                           y            -> do flag <- eval' x
                                              if flag==(-39486923864293864208364)
                                              then return (-39486923864293864208364)
                                              else if flag==(-3948692386429386420836)
                                                   then return 0
                                                   else do eval''' xs
                    
eval''' [] = return 0