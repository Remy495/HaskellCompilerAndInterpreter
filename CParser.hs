module CParser where

import Ast
import ParserMonad
import TestsProject
oneOf :: [Parser a] -> Parser a
oneOf [] = failParse
oneOf (pa:rest) = pa <||> oneOf rest
--
-- -- *LangParser> parse (oneOf [ints,bools]) "-78"
-- -- Just (-78,"")
-- -- *LangParser> parse (oneOf [ints,bools]) " true"
-- -- Just (true,"")
-- -- *LangParser> parse (oneOf [ints,bools]) " tr ue"
-- -- Nothing
--
--
-- -- we saw before the midterm that there were issues when there are multiple operators with the same precedence, this is a helper function to handle those
-- -- this generalizes the helper function posted on piaza from last time
-- -- it is left associative
withInfix :: Parser a -> [(String, a -> a -> a)] -> Parser a
withInfix pa ls = let operators = fmap fst ls
                      opParsers = fmap (\ s -> token $ literal s) operators
                      innerParser left = do s <- oneOf opParsers
                                            next <- pa
                                            case lookup s ls of
                                              Nothing -> failParse
                                              Just f ->  let out = f left next
                                                         in (innerParser out) <||> return out
                   in do l <- pa
                         (innerParser l) <||> (return l)

keywords = ["if","then","else", "let", "in", "true","false"]
--
--
vars :: Parser Expr
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s
--
ints :: Parser Expr
ints = ints' <||> ints''
ints' = do int <- natParser
           return $ ValInt int
ints''= do token $ literal "-"
           int <- natParser
           return $ ValInt (int*(-1))
           
addSubExpr :: Parser Expr
addSubExpr = (withInfix multDivExpr [("+", (Plus)), ("-", (Minus))])

multDivExpr :: Parser Expr
multDivExpr = (withInfix parseFactor [("*", (Mult)), ("/", (Div)),("%",(Mod))])

orBExpr :: Parser Expr
orBExpr = (withInfix andBExpr [("||", (Or))])

andBExpr :: Parser Expr
andBExpr = (withInfix parseBFactor [("&&", (And))])
--

parseFunc :: Parser Stmnt
parseFunc = (do token $ literal "def"                -- 4
                (Var f) <- token $ vars
                args <- token $ parens $ parseArgs2
                token $ literal "{"
                x <- token $ parseStmts
                token $ literal "}"
                return (Def f args x))

parseStmts :: Parser [Stmnt]
parseStmts = (do x <- token $ parseStmt              -- 5
                 token $ literal ";"
                 y <- token $ parseStmts
                 return (x:y))<||>
             (do x <- token $ parseStmt              -- 6
                 token $ literal ";"
                 return [x])<||>
             (do x <- token $ parseBlock             -- 7
                 y <- token $ parseStmts
                 return (x:y))<||>  
             (do x <- token $ parseBlock             -- 8
                 return [x])
                 
parseBlock :: Parser Stmnt
parseBlock = (do token $ literal "if"                -- 12
                 x <- token $ parens $ parseBExpr
                 y <- token $ parseBlock
                 token $ literal "else"
                 z <- token $ parseBlock
                 return (If_Else x y z))<||>
             (do token $ literal "if"                -- 11       
                 x <- token $ parens $ parseBExpr
                 y <- token $ parseBlock 
                 return (If x y))<||>
             (do token $ literal "do"             -- 10
                 y <- token $ parseBlock
                 token $ literal "while"
                 x <- token $ parens $ parseBExpr
                 token $ literal ";"
                 return (DoWhile y x))<||>
             (do token $ literal "while"             -- 10
                 x <- token $ parens $ parseBExpr
                 y <- token $ parseBlock
                 return (While x y))<||>
             
             (do token $ literal "{"                 -- 9
                 x <- parseStmts
                 token $ literal "}"
                 return $ Block x)
                 
parseStmt :: Parser Stmnt
parseStmt = 
            (do token $ literal "continue"           -- 17
                return Continue)<||>
            (do token $ literal "break"              -- 16
                return Break) <||>
            (do token $ literal "print"              -- 15
                (Var id) <- token $ vars
                return $ Print id)<||>
            (do token $ literal "debug"              
                (Var id) <- token $ vars
                return $ Debug id)<||>
            (do token $ literal "return"             -- 14
                x <- token $ parseExpr
                return $ Return x)<||>
            (do (Var id) <- token $ vars             -- 13
                token $ literal "="
                x <- token $ parseExpr
                return (Assign id x))

parseBExpr :: Parser Expr
parseBExpr = orBExpr                                 -- 21,20,19,18  

parseBFactor :: Parser Expr
parseBFactor = (do x <- token $ parens $ parseBExpr  -- 24
                   return x)<||>
               (parseCond)<||>                       -- 23
               (do token $ literal "!"               -- 22
                   r <- parseBFactor
                   return $ Not r)

parseCond :: Parser Expr
parseCond = (do x <- parseExpr                       -- 30
                token $ literal ">="
                y <- parseExpr
                return (Ge x y))<||>
            (do x <- parseExpr                       -- 29
                token $ literal ">"
                y <- parseExpr
                return (Gt x y))<||>
            (do x <- parseExpr                       -- 28
                token $ literal "<="
                y <- parseExpr
                return (Le x y))<||>
            (do x <- parseExpr                       -- 27
                token $ literal "<"
                y <- parseExpr
                return (Lt x y))<||>
            (do x <- parseExpr                       -- 26
                token $ literal "!="
                y <- parseExpr
                return (Ne x y))<||>
            (do x <- parseExpr                       -- 25
                token $ literal "=="
                y <- parseExpr
                return (Eq x y))

parseExpr :: Parser Expr
parseExpr = token $ addSubExpr                       -- 35,34,33,32,31

parseFactor :: Parser Expr
parseFactor =
             (do token $ literal "-"                 -- 38
                 r <- token $ parens $ parseExpr
                 return $ Minus (ValInt 0) (r))<||>
             (do token $ literal "-"                 -- 38
                 r <- token $ vars
                 return $ Minus (ValInt 0) (r))<||>
             (do token $ literal "-"                 -- 38
                 r <- token $ ints
                 return $ Minus (ValInt 0) (r))<||>
             (do (Var f) <- token $ vars             -- 40,39
                 e <- token $ parens $ parseArgs
                 return $ Call f e)<||>
             (do r <- token $ vars                   -- 41
                 return r)<||>
             (do r <- token $ ints                   -- 42
                 return r)<||>
             (do r <- token $ parens $ parseExpr     -- 43
                 return r)

-- Helper funcs
parseArgs2 :: Parser [String]
parseArgs2 = do r <- rep $ token $ parseArgs2'
                return r
parseArgs2' :: Parser String
parseArgs2' =(do token $ literal ","
                 (Var x) <- token $ vars
                 return x)<||>
             (do (Var x) <- token $ vars
                 return x)
                 
parseArgs :: Parser [Expr]
parseArgs = do r <- rep $ token $ parseArgs'
               return r
parseArgs' :: Parser Expr    
parseArgs' = (do token $ literal ","
                 r <- token $ parseExpr
                 return r)<||>
             (do r <- token $ parseExpr
                 return r)


parens :: Parser a -> Parser a 
parens pa = do token $ literal "("
               r <- token $ pa
               token $ literal ")"
               return r

 -- Actual parser
parser :: Parser Program
parser = token $ rep $ token $ parseFunc

