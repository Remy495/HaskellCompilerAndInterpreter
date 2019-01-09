module CCompiler where

import Ast
import qualified ICInterpreter hiding (State)
import TestsProject
import Prelude hiding (lookup)
import CParser
import Data.Map(Map, lookup, insert, empty, fromList)  -- for State
import ParserMonad

import Debug.Trace

--TO GET THE IC_Program FROM A PROGRAM SUCH AS "def main(){return 0;}" 
--      testCompileO (program)         --using optimizer
--      testCompile (program)          --without opitmizer
--TO EXECUTE A STRING PROGRAM SUCH AS "def main(){return 0;}" 
--      execute $ testCompileO (program)         --using optimizer
--      execute $ testCompile (program)          --without opitmizer







data Op = Var' String | Val' Int
type IC_Program = [IC_Instruction]
data IC_Instruction
        = Plus'  Op Op Op
        | Minus' Op Op Op
        | Times' Op Op Op
        | Div'   Op Op Op
        | Mod'   Op Op Op
        | Equal' Op Op Op
        | NotEq' Op Op Op
        | Lt'    Op Op Op
        | Gt'    Op Op Op
        | Le'    Op Op Op
        | Ge'    Op Op Op
        | And'   Op Op Op        
        | Or'    Op Op Op
        | Uminus' Op Op
        | Not'    Op Op
        | Assign' Op Op
        | Bzero'  Op Int
        | Jump'   Int
        | Call'   Int
        | Call'' String
        | Push'
        | Return'  Op
        | Print'  String Op
        | Halt'
type State = (Program,IC_Program, [String], BackPatch, FList)

--                TList     FList     Break     Continue
type BackPatch = ([Int],[Int],[Int],[Int])
--               FuncName -> (Index,Args)
type FList = Map String (Int,[String])
testCompile :: String -> ICInterpreter.IC_Program
testCompile str = compile $ (\(Just (x,y))->x) (parse parser str)

testCompileO :: String -> ICInterpreter.IC_Program
testCompileO str = compileO $ (\(Just (x,y))->x) (parse parser str)

execute = ICInterpreter.execute

temps :: [String]
temps = ["_t"++show(n)|n<-[1..]]

compile :: Program -> ICInterpreter.IC_Program
compile p = let (_,ic,_,_,_) = compile' (p,[Push',(Call'' "main"),(Halt')],temps,([],[],[],[]),(findFuncs p))
            in (convert ic)
compileO :: Program -> ICInterpreter.IC_Program
compileO p = let (_,ic,_,_,_) = compile' (p,[Push',(Call'' "main"),(Halt')],temps,([],[],[],[]),(findFuncs p))
             in convert $ optimize ic           
            
convert :: IC_Program -> ICInterpreter.IC_Program
convert [] = []
convert (x:xs) = case x of 
                (Plus' x y z) -> 
                    ((ICInterpreter.Plus' (c x) (c y) (c z)):(convert xs))
                (Minus' x y z) -> 
                    ((ICInterpreter.Minus' (c x) (c y) (c z)):(convert xs))
                (Div' x y z) -> 
                    ((ICInterpreter.Div' (c x) (c y) (c z)):(convert xs))
                (Times' x y z) -> 
                    ((ICInterpreter.Times' (c x) (c y) (c z)):(convert xs))
                (Mod' x y z) -> 
                    ((ICInterpreter.Mod' (c x) (c y) (c z)):(convert xs))
                (Equal' x y z) -> 
                    ((ICInterpreter.Equal' (c x) (c y) (c z)):(convert xs))
                (NotEq' x y z) -> 
                    ((ICInterpreter.NotEq' (c x) (c y) (c z)):(convert xs))
                (Lt' x y z) -> 
                    ((ICInterpreter.Lt' (c x) (c y) (c z)):(convert xs))
                (Le' x y z) -> 
                    ((ICInterpreter.Le' (c x) (c y) (c z)):(convert xs))
                (Gt' x y z) -> 
                    ((ICInterpreter.Gt' (c x) (c y) (c z)):(convert xs))
                (Ge' x y z) -> 
                    ((ICInterpreter.Ge' (c x) (c y) (c z)):(convert xs))
                (And' x y z) -> 
                    ((ICInterpreter.And' (c x) (c y) (c z)):(convert xs))
                (Or' x y z) -> 
                    ((ICInterpreter.Or' (c x) (c y) (c z)):(convert xs))
                (Not' x y) -> 
                    ((ICInterpreter.Not' (c x) (c y)):(convert xs))
                (Assign' x y) -> 
                    ((ICInterpreter.Assign' (c x) (c y)):(convert xs))
                (Bzero' x y) -> 
                    ((ICInterpreter.Bzero' (c x) y):(convert xs))
                (Jump' x) -> 
                    ((ICInterpreter.Jump' x):(convert xs))
                (Call' x) -> 
                    ((ICInterpreter.Call' x):(convert xs))
                (Push') -> 
                    ((ICInterpreter.Push' ):(convert xs))
                (Return' x) -> 
                    ((ICInterpreter.Return' (c x)):(convert xs))
                (Print' x y) -> 
                    ((ICInterpreter.Print' x (c y)):(convert xs))
                (Halt') -> 
                    ((ICInterpreter.Halt'):(convert xs))
                    
c :: Op -> ICInterpreter.Op
c (Var' x) = ICInterpreter.Var' x
c (Val' x) = ICInterpreter.Val' x
   
--
--
compile' :: State -> State
compile' (f:fs,ic,t,b,fl) = let (_,ic1,t1,b1,fl1) = compileF f (f:fs,ic,t,b,fl)
                            in compile' (fs,ic1,t1,b1,fl1)
compile' ([],a,b,c,d) = backPatch ([],a,b,c,d) ([],[],b,c,d)
--
compileF :: Stmnt -> State -> State
compileF (Def name args body) (fs,ic,t,b,fl) = let fl1 = (insert (name) ((length ic),args) fl)
                                               in  compileSs body (fs,ic,t,b,fl1)
--           Argument    Temps
--
--
compileS :: Stmnt -> State -> State
compileS (Block []) s = s
compileS (Block (x:xs)) s = let s2 = compileS x s
                            in  compileS (Block xs) s2
compileS (Assign str expr) (fs,ic,t,b,fl) = case expr of 
                                                (ValInt value) -> (fs,(ic++[(Assign' (Var' str) (Val' (fromIntegral value)))]),t,b,fl)
                                                x              -> let (loc,(p,ic2,t2,b2,fl2)) = compileE expr (fs,ic,t,b,fl)
                                                                  in  (p,(ic2++[(Assign' (Var' str) (Var' loc))]),t2,b2,fl2)
compileS (Return expr) (fs,ic,t,b,fl) = case expr of
                                            (ValInt value) -> (fs,(ic++[(Return' (Val' (fromIntegral value)))]),t,b,fl)
                                            x              -> let (loc,(p,ic2,t2,b2,fl2)) = compileE expr (fs,ic,t,b,fl)
                                                              in  (p,(ic2++[(Return' (Var' loc))]),t2,b2,fl2)
compileS (Print x) (fs,ic,t,b,fl) = (fs,(ic++[(Print' (x++" = ") (Var' x))]),t,b,fl)
compileS (While expr body) (p,ic,t,b,fl) = let start = length ic
                                               (_,(fs1,ic1,t1,(tList,fList,yy,zz),fl1)) = compileE expr (p,ic,t,b,fl)
                                               true = length ic1
                                               (fs2,ic2,t2,(xs,ys,bs,cs),fl2) = compileS body (fs1,ic1,t1,([],fList,yy,zz),fl1)
                                               ic3 = ic2++[(Jump' start)]
                                               false = length ic3
                                               ic4 = backPatchTF tList true fList false ic3 0
                                               ic5 = backPatchBC bs false cs start ic4 0
                                           in  (fs2,ic5,t2,([],[],[],[]),fl2)
compileS (DoWhile body expr) (p,ic,t,b,fl)=let start = length ic
                                               ((fs1,ic1,t1,(tList,fList,yy,zz),fl1)) = compileS body (p,ic,t,b,fl)
                                               (_,(fs2,ic2,t2,(xs,ys,bs,cs),fl2)) = compileE expr (fs1,ic1,t1,(tList,fList,yy,zz),fl1)
                                               ic3 = ic2
                                               false = ((length ic3))
                                               ic4 = backPatchTF xs start ys false ic3 0
                                               ic5 = backPatchBC bs false cs start ic4 0
                                           in  (fs2,ic5,t2,([],[],[],[]),fl2)
compileS (If expr body) (p,ic,t,b,fl) =    let start = length ic
                                               (_,(fs1,ic1,t1,(tList,fList,zz,xx),fl1)) = compileE expr (p,ic,t,b,fl)
                                               true = length ic1
                                               (fs2,ic2,t2,(xs,ys,a,z),fl2) = compileS body (fs1,ic1,t1,([],fList,zz,xx),fl1)
                                               false = length ic2
                                               ic4 = backPatchTF tList true fList false ic2 0
                                               
                                           in  (fs2,ic4,t2,([],[],a,z),fl2)
compileS (If_Else expr body1 body2)
         (p,ic,t,b,fl)                =    let start = length ic
                                               (_,(fs1,ic1,t1,(tList,fList,yy,xx),fl1)) = compileE expr (p,ic,t,b,fl)
                                               true = length ic1
                                               
                                               (fs2,ic2,t2,(xs,ys,a,z),fl2) = compileS body1 (fs1,ic1,t1,([],fList,yy,xx),fl1)
                                               ic3 = ic2++[(Jump' (-1))]
                                               false = length ic3
                                               (fs4,ic4,t4,(trueL,falseL,zz,zzz),fl4) = compileS body2 (fs2,ic3,t2,(xs,ys,a,z),fl2)
                                               ic5 = backPatchTF tList true fList false ic4 0
                                               ic6 = backPatchTF [(false-1)] (length ic5) [] 0 ic5 0
                                               
                                           in  (fs4,ic6,t4,([],[],zz,zzz),fl4)
compileS (Continue) 
         (p,ic,t,(tr,fa,b,c),fl) =         let index = length ic
                                               ic2 = ic++[(Jump' (-1))]
                                               c2 = index:b
                                           in (p,ic2,t,(tr,fa,b,c2),fl)
compileS (Break) 
         (p,ic,t,(tr,fa,b,c),fl) =         let index = length ic
                                               ic2 = ic++[(Jump' (-1))]
                                               b2 = index:b
                                           in (p,ic2,t,(tr,fa,b2,c),fl)                                           
                                                
--  
--
compileE :: Expr -> State -> (String,State)
compileE (Call name args) (fs,ic,t,b,fl) = let (Just (_,params)) = (lookup name fl)
                                               (locs,(fs2,ic2,t2,b2,fl2)) = setupArgs args (fs,ic,t,b,fl)
                                               ic3 = ic2++[(Push')]
                                               (fs4,ic4,t4,b4,fl4) = setupArgs2 params locs (fs2,ic3,t2,b2,fl2)
                                           in  ("_ret_val",(fs4,(ic4++[(Call'' name)]),t4,b4,fl4))
compileE (Var s) st = (s,st)
compileE (ValInt i) (fs,ic,(t:ts),b,fl) = (t,(fs,(ic++[(Assign' (Var' t) (Val' (fromIntegral i)))]),ts,b,fl))
compileE (Plus l r) s = let (locL,(p,ic,(t:ts),b,fl)) = compileE l s
                            ic2 = ic++[(Assign' (Var' t) (Var' locL))]
                            (locR,(p3,ic3,(t2:t3:ts2),b3,fl3)) = compileE r (p,ic2,(ts),b,fl)
                            ic4 = ic3++[(Assign' (Var' t2) (Var' locR))]
                            instruction = (Plus' (Var' t3) (Var' t) (Var' t2))
                        in  (t3,(p3,(ic4++[instruction]),ts2,b,fl))
compileE (Minus l r) s =let (locL,(p,ic,(t:ts),b,fl)) = compileE l s
                            ic2 = ic++[(Assign' (Var' t) (Var' locL))]
                            (locR,(p3,ic3,(t2:t3:ts2),b3,fl3)) = compileE r (p,ic2,(ts),b,fl)
                            ic4 = ic3++[(Assign' (Var' t2) (Var' locR))]
                            instruction = (Minus' (Var' t3) (Var' t) (Var' t2))
                        in  (t3,(p3,(ic4++[instruction]),ts2,b,fl))
compileE (Mult l r) s = let (locL,(p,ic,(t:ts),b,fl)) = compileE l s
                            ic2 = ic++[(Assign' (Var' t) (Var' locL))]
                            (locR,(p3,ic3,(t2:t3:ts2),b3,fl3)) = compileE r (p,ic2,(ts),b,fl)
                            ic4 = ic3++[(Assign' (Var' t2) (Var' locR))]
                            instruction = (Times' (Var' t3) (Var' t) (Var' t2))
                        in  (t3,(p3,(ic4++[instruction]),ts2,b,fl))
compileE (Div l r) s =  let (locL,(p,ic,(t:ts),b,fl)) = compileE l s
                            ic2 = ic++[(Assign' (Var' t) (Var' locL))]
                            (locR,(p3,ic3,(t2:t3:ts2),b3,fl3)) = compileE r (p,ic2,(ts),b,fl)
                            ic4 = ic3++[(Assign' (Var' t2) (Var' locR))]
                            instruction = (Div' (Var' t3) (Var' t) (Var' t2))
                        in  (t3,(p3,(ic4++[instruction]),ts2,b,fl))
compileE (Mod l r) s =  let (locL,(p,ic,(t:ts),b,fl)) = compileE l s
                            ic2 = ic++[(Assign' (Var' t) (Var' locL))]
                            (locR,(p3,ic3,(t2:t3:ts2),b3,fl3)) = compileE r (p,ic2,(ts),b,fl)
                            ic4 = ic3++[(Assign' (Var' t2) (Var' locR))]
                            instruction = (Mod' (Var' t3) (Var' t) (Var' t2))
                        in  (t3,(p3,(ic4++[instruction]),ts2,b,fl))  
                        
compileE (Lt l r) s =   let (locL,s1) = compileE l s
                            (locR,(p,ic,(t:ts),(tList,fList,x,y),fl)) = compileE r s1
                            ic2 = ic++[(Lt' (Var' t) (Var' locL) (Var' locR)),(Bzero' (Var' t) 0),(Jump' 0)]
                            (fList2) = ((length ic2)-2):fList
                            (tList2) = ((length ic2)-1):tList
                        in  ("",(p,ic2,ts,(tList2,fList2,x,y),fl))
compileE (Le l r) s =   let (locL,s1) = compileE l s
                            (locR,(p,ic,(t:ts),(tList,fList,x,y),fl)) = compileE r s1
                            ic2 = ic++[(Le' (Var' t) (Var' locL) (Var' locR)),(Bzero' (Var' t) 0),(Jump' 0)]
                            (fList2) = ((length ic2)-2):fList
                            (tList2) = ((length ic2)-1):tList
                        in  ("",(p,ic2,ts,(tList2,fList2,x,y),fl))
compileE (Gt l r) s =   let (locL,s1) = compileE l s
                            (locR,(p,ic,(t:ts),(tList,fList,x,y),fl)) = compileE r s1
                            ic2 = ic++[(Gt' (Var' t) (Var' locL) (Var' locR)),(Bzero' (Var' t) 0),(Jump' 0)]
                            (fList2) = ((length ic2)-2):fList
                            (tList2) = ((length ic2)-1):tList
                        in  ("",(p,ic2,ts,(tList2,fList2,x,y),fl))
compileE (Ge l r) s =   let (locL,s1) = compileE l s
                            (locR,(p,ic,(t:ts),(tList,fList,x,y),fl)) = compileE r s1
                            ic2 = ic++[(Ge' (Var' t) (Var' locL) (Var' locR)),(Bzero' (Var' t) 0),(Jump' 0)]
                            (fList2) = ((length ic2)-2):fList
                            (tList2) = ((length ic2)-1):tList
                        in  ("",(p,ic2,ts,(tList2,fList2,x,y),fl))
compileE (Eq l r) s =   let (locL,s1) = compileE l s
                            (locR,(p,ic,(t:ts),(tList,fList,x,y),fl)) = compileE r s1
                            ic2 = ic++[(Equal' (Var' t) (Var' locL) (Var' locR)),(Bzero' (Var' t) 0),(Jump' 0)]
                            (fList2) = ((length ic2)-2):fList
                            (tList2) = ((length ic2)-1):tList
                        in  ("",(p,ic2,ts,(tList2,fList2,x,y),fl))
compileE (Ne l r) s =   let (locL,s1) = compileE l s
                            (locR,(p,ic,(t:ts),(tList,fList,x,y),fl)) = compileE r s1
                            ic2 = ic++[(NotEq' (Var' t) (Var' locL) (Var' locR)),(Bzero' (Var' t) 0),(Jump' 0)]
                            (fList2) = ((length ic2)-2):fList
                            (tList2) = ((length ic2)-1):tList
                        in  ("",(p,ic2,ts,(tList2,fList2,x,y),fl))                        
                        
compileE (And l r) s =  let (_,(p,ic,t,(tList,fList,a,b),fl)) = compileE l s
                            tV = length ic
                            ic2 = backPatchTF tList tV [] 0 ic 0
                            (_,(p3,ic3,t3,b3,fl3)) = compileE r (p,ic2,t,([],fList,a,b),fl)
                            ic4 = backPatchTF [] 0 fList (length ic3) ic3 0
                        in  ("",(p3,ic4,t3,b3,fl3))
                        
compileE (Or l r) s =   let (_,(p,ic,t,(tList,fList,a,b),fl)) = compileE l s
                            fV = length ic
                            ic2 = backPatchTF [] 0 fList fV ic 0
                            (_,(p3,ic3,t3,b3,fl3)) = compileE r (p,ic2,t,([],fList,a,b),fl)
                            ic4 = backPatchTF tList (length ic3) [] 0 ic3 0
                        in  ("",(p3,ic4,t3,b3,fl3))
compileE (Not e)  s =   let (_,(p,ic,t,(tList,fList,a,b),fl)) = compileE e s
                        in  ("",(p,ic,t,(fList,tList,a,b),fl))
--
--
compileSs :: [Stmnt] -> State -> State
compileSs (x:xs) s =  let s1 = compileS x s
                      in  compileSs xs s1
compileSs [] s = s
--
--
findFuncs :: Program -> Map String (Int,[String])
findFuncs [] = empty
findFuncs ((Def n a _):fs) = insert n (-1,a) (findFuncs fs)
--
--
setupArgs :: [Expr] -> State -> ([String],State)
setupArgs [] s = ([],s)
setupArgs (x:xs) s =  let (loc,(p,ic,t,b,fl)) = compileE x s
                          (params,state) = setupArgs xs (p,ic,t,b,fl)
                      in  (loc:params,state)
--
--
setupArgs2 :: [String] -> [String] -> State -> State
setupArgs2 [] [] s =  s
setupArgs2 (x:xs) (y:ys) (p,ic,t,b,fl) = setupArgs2 xs ys (p,(ic++[(Assign' (Var' x) (Var' y))]),t,b,fl)
--
--
backPatchBC :: [Int] -> Int -> [Int] -> Int -> IC_Program -> Int -> IC_Program
backPatchBC _ _ _ _ [] _ = []
backPatchBC bI bV cI cV (x:xs) index = if (index `elem` bI)
                                       then (Jump' bV):(backPatchBC bI bV cI cV xs (index+1))
                                       else if (index `elem` cI)
                                            then (Jump' cV):(backPatchBC bI bV cI cV xs (index+1))
                                            else x:(backPatchBC bI bV cI cV xs (index+1))
backPatchTF :: [Int] -> Int -> [Int] -> Int -> IC_Program -> Int -> IC_Program
backPatchTF _ _ _ _ [] _ = []
backPatchTF tI tV fI fV (x:xs) index  = if((index `elem` tI))
                                        then case x of (Jump' _) -> (Jump' tV):(backPatchTF tI tV fI fV xs (index+1))
                                                       (Bzero' y z) -> (Bzero' y tV):(backPatchTF tI tV fI fV xs (index+1))
                                        else if(index `elem` fI)
                                             then case x of (Bzero' y z) -> (Bzero' y fV):(backPatchTF tI tV fI fV xs (index+1))
                                                            (Jump' z) -> (Jump' fV):(backPatchTF tI tV fI fV xs (index+1))
                                             else x:(backPatchTF tI tV fI fV xs (index+1))
backPatch :: State -> State -> State
backPatch (fs,[],t,b,fl) (a,bb,c,d,e) = (a,bb,c,d,e)
backPatch (fs,(x:xs),t,b,fl) (p,ic,t2,b2,fl2)= case x of (Call'' y) -> let (Just (n,_)) = lookup y fl
                                                                       in backPatch (fs,xs,t,b,fl) (p,(ic++[(Call' n)]),t2,b2,fl2)
                                                         z          -> backPatch (fs,xs,t,b,fl) (p,(ic++[z]),t2,b2,fl2)
                                                         
optimize :: IC_Program -> IC_Program
optimize ic = case (optimize' ic 0 ic) of (1,x) -> optimize x
                                          (0,x) -> x
                                          
optimize' ::IC_Program -> Int -> IC_Program -> (Int,IC_Program)
optimize' [] _ a = (0,a)
--get rid of redundant jumps
optimize' ((Jump' dest):xs) (index) ic | (dest==(index+1)) = (1,(optimizeJ ic (index) 0))
optimize' (x:xs) (index) ic = optimize' xs (index+1) ic

optimizeJ :: IC_Program -> Int -> Int -> IC_Program
optimizeJ [] _ _ = []
optimizeJ (x:xs) i i2 | i==i2 = optimizeJ xs i (i2+1) 
optimizeJ ((Jump' x):xs) i i2| x>i = ((Jump' (x-1)):(optimizeJ xs i (i2+1)))
optimizeJ ((Bzero' a x):xs) i i2| x>i = ((Bzero' a (x-1)):(optimizeJ xs i (i2+1)))
optimizeJ ((Call' x):xs) i i2| x>i = ((Call' (x-1)):(optimizeJ xs i (i2+1)))
optimizeJ (x:xs) i i2 = (x:(optimizeJ xs (i) (i2+1)))








