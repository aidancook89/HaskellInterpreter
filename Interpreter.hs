module Interpreter where
    import Data.Char 
    import Combinators
    -------------------------------------------------------------------------------
--Interpreter
    type Variable = String

    type Val = Int

    type Store = Variable -> Val

    data Expr = Const Val | Var Variable | Minus Expr Expr | Times Expr Expr 
        | Greater Expr Expr
        deriving Show

    data Command = Assign Variable Expr | Seq Command Command| 
        Cond Expr Command Command | While Expr Command
        deriving Show
{-
    Assign :: Variable -> Expr -> Command
    Seq :: Command -> Command -> Command
    Cond :: Expr -> Command -> Command -> Command
    While :: Expr -> Command -> Command
-}

--Store functions
--fetch gets the value associated with a variable in a specific store
--update updates the value associated with a variable in a specific store
--initial sets all associated values to 0
    fetch :: Store -> Variable -> Val
    update :: Store -> Variable -> Val -> Store
    initial :: Store

    fetch = id 
    --fix style
    update s v val x = if x == v then val else s x
    initial _ = 0

--eval function that evaluates the expressions passed to it based on pattern
--matching
    eval :: Expr -> Store -> Val
    eval (Const x) _ = x
    eval (Var v) s = fetch s v
    eval (Minus m1 m2) s = eval m1 s - eval m2 s
    eval (Times t1 t2) s = eval t1 s * eval t2 s
    eval (Greater g1 g2) s = if eval g1 s > eval g2 s then 1 else 0

--switch function that chooses one of two functions to apply to a store based
--on the value provided to it
    switch :: Val -> (Store -> Store) -> (Store -> Store) -> Store -> Store
    switch 0 _ g = g
    switch 1 f _ = f

--interpret function that interprets the commands passed to it with 
--pattern matching
    interpret :: Command -> Store -> Store
    interpret (Assign v e) s = update s v (eval e s)
    interpret (Seq c1 c2) s = interpret c2 (interpret c1 s)
    interpret (Cond e c1 c2) s = switch (eval e s) (interpret c1) (interpret c2) s
    interpret (While e c) s = switch (eval e s) (interpret (While e c).interpret c) 
        fetch s

    givenTest :: Command
    givenTest = (While (Greater (Var "X") (Var "Y"))
        (Seq (Assign "X" (Minus (Var "X") (Const 1)))
            (Assign "Z" (Times (Var "Z") (Var "Z"))))) 


--Interpreter Tests
    
--test 1 checks the assign command
    store1 :: Store 
    store1 = update initial "Y" 5

    intTest1 :: Command 
    intTest1 = Assign "X" (Times (Var "Y") (Var "Y"))

--test 2 checks the seq command
    store2 :: Store
    store2 = update (update initial "Y" 3) "Z" 4

    intTest2 :: Command
    intTest2 = Seq (Assign "X" (Times (Var "Y") (Var "Y"))) 
        (Assign "X" (Minus (Var "Z") (Var "Y")))

--test 3 checks the cond command
    store3 :: Store
    store3 = update (update (update initial "Y" 3) "X" 5) "Z" 2

    intTest3 :: Command 
    intTest3 = Cond (Greater (Var "X") (Var "Y")) (Assign "Y" (Times (Var "X") 
        (Var "X"))) (Assign "X" (Minus (Var "Y") (Var "Z")))

--test 4 checks the while command
    store4 :: Store
    store4 = update (update initial "Z" 2) "X" 7

    intTest4 :: Command
    intTest4 = While (Greater (Var "X") (Var "Z")) 
        (Assign "Z" (Times (Var "Z") (Var "Z")))

--test 5 combines some element of all the previous tests
    store5 :: Store
    store5 = update (update initial "X" 24) "Z" 2
    
    intTest5 :: Command
    intTest5 = While (Greater (Var "X") (Var "Z")) (Seq (Assign "X" (Minus (Var "X") 
        (Const 1))) (Assign "Z" (Times (Var "X") (Var "Z"))))
        
-----------------------------------------------------------------------------------
--Parser

    data Token = Ident String | Number Int | Symbol String
        deriving Show
    type Parser a = [Token] -> Maybe (a, [Token])

--number parser that takes a number n in a list of tokens and returns a Just (n, s)
    number :: Parser Int
    number (Number n : s) = Just (n,s)
    number _ = Nothing

--variable parser that takes a number n in a list of tokens and returns a Just (n, s)
    variable :: Parser Variable
    variable (Ident v : s) = Just (v,s)
    variable _ = Nothing

--literal parser that takes a string s and a Symbol sy in a list of tokens 
--and returns a Just (s, ss) if the string s is the same as what the Symbol string is
    literal :: String -> Parser String
    literal s (Symbol sy : ss) | s == sy = Just (s, ss)
    literal _ _ = Nothing

--expr parser that returns a Greater expression based on the input token list
    expr :: Parser Expr
    expr = (aexp <&> optional (literal ">" <&> aexp)) `modify` optGreater 
        where optGreater (e1, []) = e1
              optGreater (e1, [(_, e2)]) = Greater e1 e2
              optGreater _ = error "impossible"

--aexp parser that returns a Minus expression based on the input token list
    aexp :: Parser Expr
    aexp = (bexp <&> optional (literal "-" <&> aexp)) `modify` optSub
        where optSub (e1, []) = e1
              optSub (e1, [(_, e2)]) = Minus e1 e2
              optSub _ = error "impossible"

--bexp parser that returns a Times expression based on the input token list
    bexp :: Parser Expr
    bexp = (cexp <&> optional (literal "*" <&> bexp)) `modify` optMul
        where optMul (e1, []) = e1
              optMul (e1, [(_, e2)]) = Times e1 e2
              optMul _ = error "impossible"

--cexp parser that returns an unparenthesized expression or a number or a variable
    cexp :: Parser Expr
    cexp = (literal "(" <&> expr <&> literal ")") `modify` unparenth 
        <|> (number `modify` Const) <|> (variable `modify` Var)
            where unparenth ((_, e1), _) = e1

--command parser that returns a Sequence command based on the input token list
    command :: Parser Command
    command = unitcom <&> optional (literal ";" <&> command) `modify` optSeq
        where unitcom = whilecom <|> ifcom <|> assign
              optSeq (c1, []) = c1
              optSeq (c1, [(_, c2)]) = Seq c1 c2
              optSeq _ = error "impossible"

--whilecom parser that returns a While command based on the input token list
    whilecom :: Parser Command
    whilecom = (literal "WHILE" <&> expr <&> literal "DO" <&> command <&> 
        literal "END") `modify` mkWhileNode
            where mkWhileNode ((((_, e1) ,_), c1),_) = While e1 c1

--ifcom parser that returns a Conditional command based on the input token list
    ifcom :: Parser Command
    ifcom = (literal "IF" <&> expr <&> literal "THEN" <&> command <&> literal "ELSE" 
        <&> command <&> literal "ENDIF") `modify` mkIfNode
            where mkIfNode ((((((_, e1), _), c1), _), c2), _) = Cond e1 c1 c2

--assign parser that returns an Assign command based on the input token list
    assign:: Parser Command
    assign = (variable <&> literal ":=" <&> expr) `modify` mkAssignNode
        where mkAssignNode ((v, _ ), e) = Assign v e
    
--report function that performs error handling
    report :: Maybe (a, [Token]) -> a
    report Nothing = error "Parse error"
    report (Just(c,[])) = c
    report (Just(c,xs)) = error (stringwith
        ("Syntax error \n Unparsed:-\n",
        " ",
        "\n")
        (map lit xs))
        where 
            stringwith (front, sep, back) ls =
                let sepback [] = back
                    sepback [a] = a ++ back
                    sepback (a:xs) = a ++ sep ++ sepback xs
                in
                    front ++ sepback ls

            lit (Ident s) = s ++ " "
            lit (Symbol s) = s ++ " "
            lit (Number s) = show s ++ " "

--mainParser function that composes the report function and command parser to implement a complete
--parser for the L language
    mainParser :: [Token] -> Command
    mainParser = report . command

--Parser Tests
    parTest1 :: [Token]
    parTest1 = [Ident "X",Symbol ":=",Ident "Y",Symbol "*",Ident "Y"]

    parTest2 :: [Token]
    parTest2 = [Ident "X",Symbol ":=",Ident "Y",Symbol "*",Ident "Y",Symbol ";",
        Ident "X",Symbol ":=",Ident "Z",Symbol "-",Ident "Y"]

    parTest3 :: [Token]
    parTest3 = [Symbol "IF",Ident "X",Symbol ">",Ident "Y",Symbol "THEN",Ident "Y",
        Symbol ":=",Ident "X",Symbol "*",Ident "X",Symbol "ELSE",Ident "X",
        Symbol ":=",Ident "Y",Symbol "-",Ident "Z",Symbol "ENDIF"]

    parTest4 :: [Token]
    parTest4 = [Symbol "WHILE",Ident "X",Symbol ">",Ident "Z",Symbol "DO",Ident "Z",
        Symbol ":=",Ident "Z",Symbol "*",Ident "Z",Symbol "END"]

    parTest5 :: [Token]
    parTest5 = [Symbol "WHILE",Ident "X",Symbol ">",Ident "Z",Symbol "DO",Ident "X",
        Symbol ":=",Ident "X",Symbol "-",Number 1,Symbol ";",Ident "Z",Symbol ":=",
        Ident "X",Symbol "*",Ident "Z",Symbol "END"]


-----------------------------------------------------------------------------------
--Lexical Analyzer

--keyword function that returns a Bool based on if the input string s is present
--in the list of L language keywords
    keyword :: String -> Bool
    keyword s = s `elem` ["IF", "THEN", "ELSE", "ENDIF", "WHILE", "DO", "END"]

--keycheck function that uses the keyword function to return either a Symbol s if
--the string is a keyword or an Ident s otherwise
    keycheck :: String -> Token
    keycheck s | keyword s = Symbol s
        | otherwise = Ident s

--letdigetc function that returns a Bool based on if the input char c is a letter,
--a number, a single quote, or an underscore
    letdigetc :: Char -> Bool
    letdigetc c = isLetter c || isDigit c || c `elem` "\'_"

--layout function that returns a Bool based on if the input char is a whitespace char
    layout :: Char -> Bool
    layout c = c `elem` " \n\t"

--symbolchar function that returns a Bool based on if the input char is
--present in a list of L language symbols
    symbolchar :: Char -> Bool
    symbolchar c = c `elem` "*->:=;"

--lexer function that analyzes a string a builds a token list based off of that 
    lexer :: String -> [Token]
    lexer [] = []
    lexer (a:x) = if layout a then lexer x else
        if a == '(' then Symbol "(" : (lexer x) else
            if a == ')' then Symbol ")" : (lexer x) else
                if isLetter a then getword [a] x else
                    if isDigit a then getnum (digitToInt a) x else
                        if symbolchar a then getsymbol [a] x else
                            error ("Lexical error : unrecognized token " ++ (a:x))

--getword function that gets a word of the input string, char by char, until 
--a character is seen that ends the function call
    getword :: String -> String -> [Token]
    getword l [] = [keycheck (reverse l)]
    getword l (a:x) = if letdigetc a
        then getword (a:l) x
            else (keycheck (reverse l)) : (lexer (a:x))

--getsymbol function that gets a symbol of the input string, char by char, until 
--a character is seen that ends the function call
    getsymbol :: String -> String -> [Token]
    getsymbol l [] = [Symbol (reverse l)]
    getsymbol l (a:x) = if symbolchar a
        then getsymbol (a:l) x
            else (Symbol (reverse l)) : (lexer (a:x))

--getnum function that gets a number out of the input string, char by char, until 
--a character is seen that ends the function call
    getnum :: Int -> String -> [Token]
    getnum l [] = [Number l]
    getnum l (a:x) = if isDigit a
        then getnum (l * 10 + (digitToInt a)) x
            else (Number l) : (lexer (a:x))

--run function that composes all three implementation functions to implement a 
--complete interpreter for the L language
    run :: String -> Store -> Store
    run s = interpret (mainParser (lexer s))

--Lexer Tests

    lexTest1 :: String
    lexTest1 = "X := Y * Y"

    lexTest2 :: String
    lexTest2 = "X := Y * Y; X := Z - Y"

    lexTest3 :: String
    lexTest3 = "IF X > Y THEN Y := X * X ELSE X := Y - Z ENDIF"

    lexTest4 :: String
    lexTest4 = "WHILE X > Z DO Z := Z * Z END"

    lexTest5 :: String
    lexTest5 = "WHILE X > Z DO X := X - 1; Z := X * Z END"

--Complete Tests
--same as lexer tests, but run all the way through the interpreter
    fullTest1 :: String
    fullTest1 = "X := Y * Y"

    fullTest2 :: String
    fullTest2 = "X := Y * Y; X := Z - Y"

    fullTest3 :: String
    fullTest3 = "IF X > Y THEN Y := X * X ELSE X := Y - Z ENDIF"

    fullTest4 :: String
    fullTest4 = "WHILE X > Z DO Z := Z * Z END"

    fullTest5 :: String
    fullTest5 = "WHILE X > Z DO X := X - 1; Z := X * Z END"
