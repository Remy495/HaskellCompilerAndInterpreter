module Ast where

type Program = [Stmnt]
--example1 = (\(Just (x,y)) -> x) $ parse parser test1
data Stmnt = Def String [String] [Stmnt]
           | Block [Stmnt]
           | Print String
           | Debug String
		   | Continue
		   | Break
		   | Assign String Expr
           | Return Expr
           | While Expr Stmnt
           | DoWhile Stmnt Expr
		   | If_Else Expr Stmnt Stmnt
		   | If Expr Stmnt
		   deriving (Eq,Show)
		   
data Expr = And Expr Expr | Or Expr Expr | Not Expr
          | Lt Expr Expr | Le Expr Expr 
		  | Gt Expr Expr | Ge Expr Expr 
		  | Eq Expr Expr | Ne Expr Expr 
		  
          | ValInt Integer
          | Plus Expr Expr | Minus Expr Expr | Mult Expr Expr | Div Expr Expr
		  | Mod Expr Expr
		  | Call String [Expr]
          | Var String
		  deriving (Eq,Show)

prettyShow :: Program -> String
prettyShow [] = ""
prettyShow (st:sts) = (ps' st)++(prettyShow sts) 

showArgs :: [String] -> String
showArgs [] = ""
showArgs [x] = id x
showArgs (x:xs) = (id x) ++ "," ++(showArgs xs)

showArgs' :: [Expr] -> String
showArgs' [] = ""
showArgs' [x] = ps''' x
showArgs' (x:xs) = (ps''' x) ++ "," ++(showArgs' xs)

ps' :: Stmnt -> String
ps' (Def s a sts)= "def "++s++"("++(showArgs a)++"){"++(ps'' sts)++"}"
ps' (Block sts) = "{"++(ps'' sts)++"}"
ps' (Print st) = "print "++st++";"
ps' (Continue) = "continue;"
ps' (Break) = "break;"
ps' (Assign st ex) = st++"="++(ps''' ex)++";"
ps' (Return ex) = "return "++(ps''' ex)++";"
ps' (While ex st) = "while("++(ps''' ex)++")"++(ps' st)
ps' (DoWhile st ex) = "do"++(ps' st)++"while("++(ps''' ex)++");"
ps' (If_Else ex st1 st2) = "if("++(ps''' ex)++")"++(ps' st1)++"else"++(ps' st2)
ps' (If ex st) = "if("++(ps''' ex)++")"++(ps' st)

ps''' :: Expr -> String
ps''' (And ex1 ex2) = "("++(ps''' ex1)++")"++"&&"++"("++(ps''' ex2)++")"
ps''' (Or ex1 ex2) = "("++(ps''' ex1)++")"++"||"++"("++(ps''' ex2)++")"
ps''' (Not ex) = "!"++"("++(ps''' ex)++")"
ps''' (Lt ex1 ex2) = "("++(ps''' ex1)++")"++"<"++"("++(ps''' ex2)++")"
ps''' (Le ex1 ex2) = "("++(ps''' ex1)++")"++"<="++"("++(ps''' ex2)++")"
ps''' (Gt ex1 ex2) = "("++(ps''' ex1)++")"++">"++"("++(ps''' ex2)++")"
ps''' (Ge ex1 ex2) = "("++(ps''' ex1)++")"++">="++"("++(ps''' ex2)++")"
ps''' (Eq ex1 ex2) = "("++(ps''' ex1)++")"++"=="++"("++(ps''' ex2)++")"
ps''' (Ne ex1 ex2) = "("++(ps''' ex1)++")"++"!="++"("++(ps''' ex2)++")"
ps''' (ValInt i) = show i
ps''' (Plus ex1 ex2) = "("++(ps''' ex1)++")"++"+"++"("++(ps''' ex2)++")"
ps''' (Minus ex1 ex2) = "("++(ps''' ex1)++")"++"-"++"("++(ps''' ex2)++")"
ps''' (Mult ex1 ex2) = "("++(ps''' ex1)++")"++"*"++"("++(ps''' ex2)++")"
ps''' (Div ex1 ex2) = "("++(ps''' ex1)++")"++"/"++"("++(ps''' ex2)++")"
ps''' (Mod ex1 ex2) = "("++(ps''' ex1)++")"++"%"++"("++(ps''' ex2)++")"
ps''' (Call s a) = s++"("++(showArgs' a)++")"
ps''' (Var st) = id st

ps'' :: [Stmnt] -> String
ps'' [] = ""
ps'' (x:xs) = (ps' x)++(ps'' xs)