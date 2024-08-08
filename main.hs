import Data.List
import Data.Char
import Control.Applicative 


-- Definições dos dados e dos tipos

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

data Param =
  Int Integer | TT | FF
  deriving (Show, Eq)

data Aexp = 
    IntExp Integer| VarExp String | AddExp Aexp Aexp | MulExp Aexp Aexp | SubExp Aexp Aexp | EmptyAexp
    deriving (Show, Eq)

data Bexp =
   TrueExp | FalseExp | EquExp Aexp Aexp | EqExp Bexp Bexp | LeExp Aexp Aexp | NegExp Bexp | AndExp Bexp Bexp | EmptyBexp
   deriving (Show, Eq)

data Stm = 
   If Bexp Stm Stm| While Bexp Stm | Seq Stm Stm | Assign String Aexp
   deriving(Show, Eq)  

data Token = 
  PlusTok | MinusTok | TimesTok | OpenTok | CloseTok | IntTok Integer | VarTok String | LessTok | EqualTok  
  | DotTok | CommaTok | EqTok | NotTok | AndTok | IfTok | ThenTok | ElseTok | WhileTok | DoTok | TrueTok | FalseTok | AssignTok
  deriving (Show, Eq)


type Code = [Inst]
type Stack = [Param]
type State = [(String, Param)]
type Program = [Stm] 

-- retorna uma stack da máquina vazia 
createEmptyStack :: Stack
createEmptyStack = []

-- retorna um estado da máquina vazio
createEmptyState :: State
createEmptyState = []

-- conversão da stack para uma string
stack2Str :: Stack -> String
stack2Str = intercalate "," . map stackToString
  where
    stackToString FF = "False"
    stackToString TT = "True"
    stackToString (Int n) = show n 

-- conversão do estado da máquina para string
state2Str :: State -> String
state2Str = intercalate "," . map(\(string, param) -> string ++ "=" ++ stateToString param) . sortOn fst
  where
    stateToString FF = "False"
    stateToString TT = "True"
    stateToString (Int n) = show n 

-- interpretador que executa uma lista de instruções
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack , state) = ([], stack , state)
run (code:next, stack, state) = 
  case code of 

    -- coloca o valor constante x na stack
    Push x -> run(next, Int x:stack , state) 

    -- adiciona os dois primeiros valores inteiros da stack e coloca-os no topo da stack
    Add -> if length stack >= 2
           then let (Int a : Int b : stack') = stack
                in run (next, Int (a + b) : stack', state)
           else error "Run-time error"

    -- multiplica os dois primeiros valores inteiros da stack e coloca-os no topo da stack
    Mult -> if length stack >= 2
           then let (Int a : Int b : stack') = stack
                in run (next, Int (a * b) : stack', state)
           else error "Run-time error"

    -- subtrai os dois primeiros valores inteiros da stack e coloca-os no topo da stack
    Sub -> if length stack >= 2
           then let (Int a : Int b : stack') = stack
                in run (next, Int (a - b) : stack', state)
           else error "Run-time error"

    -- coloca o valor tt na stack
    Tru -> run(next, TT:stack, state)

    -- coloca o valor ff na stack
    Fals -> run(next, FF:stack, state)

    -- compara os dois primeiros valores inteiros ou booleanos da stack para verificar igualdade
    Equ -> if length stack >= 2
           then case stack of
              (Int a : Int b : stack') -> run(next, if a == b then TT:stack' else FF:stack', state)
              (a : b : stack') -> run (next, if a == b then TT:stack' else FF:stack', state)
           else error "Run-time error"  

    -- compara os dois primeiros valores inteiros da stack para verificar se o primeiro é menor ou igual que o segundo
    Le  -> if length stack >= 2
           then let (Int a : Int b : stack') = stack
             in run (next, if a <= b then TT:stack' else FF:stack', state)
           else error "Run-time error"

    -- se os dois primeiros valores da stack forem verdadeiros coloca-se tt no topo da stack senão coloca-se ff. Caso os valores nao sejam boolean dá erro 
    And -> let (a : b : stack') = stack
           in run (next, if a == TT && b == TT then TT:stack' 
           else if a==TT  && b==FF then FF:stack'
           else if a==FF && b==FF then FF:stack'
           else if a==FF && b==TT then FF:stack'
           else error "Run-time error", state)
    
    --  nega o valor lógico do primeiro valor da stack
    Neg -> let (a : stack') = stack
            in run (next, if a == TT then FF:stack' 
            else if a==FF then TT:stack' 
            else error "Run-time error", state)

    -- coloca o valor associado a x na stack
    Fetch s -> case lookup s state of
      Just param-> run(next, param:stack, state)
      Nothing -> error "Run-time error"

    -- elimina o primeiro valor da stack e atualiza o valor associado a x com o valor eliminado
    Store s -> if not (null stack)
               then let (param : stack') = stack
                 in run (next, stack', (s, param):filter ((/=s).fst) state)
               else error "Run-time error"

    -- retorna a stack e o estado da máquina
    Noop -> run(next, stack, state)

    -- se o valor do primeiro elemento da stack for tt, esse é eliminado e o c1 é executado. Se o valor do primeiro elemento da stack for ff, esse é eliminado e o c2 é executado
    Branch code1 code2 -> if not (null stack)
                          then case head stack of
                            TT -> run (code1 ++ next, tail stack, state)
                            FF -> run (code2 ++ next, tail stack, state)
                          else error "Run-time error: Branch with empty stack"

    -- simula um while loop
    Loop code1 code2 -> run(code1 ++ [Branch(code2 ++ [Loop code1 code2]) [Noop]] ++ next, stack, state)

-- teste do assembler da parte 1
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)


-- compilador auxiliar para expressões aritméticas
compA :: Aexp -> Code
compA (IntExp x) = [Push x]
compA (VarExp s) = [Fetch s]
compA (AddExp a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (MulExp a1 a2) = compA a1 ++ compA a2 ++ [Mult]
compA (SubExp a1 a2) = compA a1 ++ compA a2 ++ [Sub]

-- compilador auxiliar para expressões booleanas
compB :: Bexp -> Code
compB TrueExp = [Tru]
compB FalseExp = [Fals]
compB (EquExp a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (EqExp b1 b2) = compB b1 ++ compB b2 ++ [Equ]
compB (LeExp a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (AndExp b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (NegExp b) = compB b ++ [Neg]

-- compilador para statements
compStm :: Stm -> Code
compStm (If b s1 s2) = compB b ++ [Branch (compStm s1) (compStm s2)]
compStm (While b s) = [Loop (compB b) (compStm s)]
compStm (Seq s1 s2) = compStm s1 ++ compStm s2
compStm (Assign s a) = compA a ++ [Store s]

-- compilador principal
compile :: Program -> Code
compile = concatMap compStm


-- parser principal nao funcional
parse :: String -> Program
parse tokens = parseLoop (lexer tokens) []


-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- funções auxiliares do Parser

-- converte a string numa lista de strings
lexer :: String -> [Token]
lexer [] = []
lexer ('+' : xs) = PlusTok : lexer xs
lexer ('-' : xs) = MinusTok : lexer xs
lexer ('*' : xs) = TimesTok : lexer xs
lexer ('(' : xs) = OpenTok : lexer xs
lexer (')' : xs) = CloseTok : lexer xs
lexer ('<' : '=' : xs) = LessTok : lexer xs
lexer ('=' : '=' : xs) = EqualTok : lexer xs
lexer (':' : '=' : xs) = DotTok : lexer xs
lexer (';': xs) = CommaTok : lexer xs
lexer ('=' : xs) = EqTok : lexer xs
lexer ('w': 'h': 'i': 'l': 'e': xs) = WhileTok : lexer xs
lexer ('d': 'o': xs) = DoTok : lexer xs
lexer ('i': 'f': xs) = IfTok : lexer xs
lexer ('t': 'h': 'e': 'n': xs) = ThenTok : lexer xs
lexer ('e': 'l': 's': 'e': xs) = ElseTok : lexer xs
lexer ('a': 'n': 'd': xs) = AndTok : lexer xs
lexer ('n': 'o': 't': xs) = NotTok : lexer xs
lexer ('T': 'r': 'u': 'e': xs) = TrueTok : lexer xs
lexer ('F': 'a': 'l': 's' : 'e': xs) = FalseTok : lexer xs
lexer (x : xs) | isSpace x = lexer xs
lexer s@(c : xs)
  | isDigit c = IntTok (read n) : lexer ns
  where
    (n, ns) = span isDigit s
lexer s@(c : xs)
  | isLetter c = VarTok v : lexer vs
  where
    (v, vs) = span isLetter s

-- Parsers auxiliares para expressões aritméticas

-- parser principal para expressoes aritméticas
parseA :: [Token] -> Maybe (Aexp, [Token])
parseA tokens =
            parseInt tokens
          <|> parseMul tokens
          <|> parseAdd tokens
          <|> parseSub tokens
          <|> parseParenA tokens
          <|> parseMulParen tokens
          <|> parseSubParen tokens
          <|> parseAddParen tokens

-- parser para inteiros
parseInt :: [Token] -> Maybe (Aexp, [Token])
parseInt (IntTok x : rest) = Just (IntExp x, rest)
parseInt tokens = Nothing

-- parser para multiplicações 
parseMul :: [Token] -> Maybe (Aexp, [Token])
parseMul tokens = case parseInt tokens of
  Just (exp1, (TimesTok : rest1)) -> case parseMul rest1 of
      Just (exp2, rest2) -> Just (MulExp exp1 exp2, rest2)
      Nothing -> Nothing
  result -> result

-- parser para adições
parseAdd :: [Token] -> Maybe (Aexp, [Token])
parseAdd tokens = case parseMul tokens of
  Just (exp1, (PlusTok : rest1)) -> case parseAdd rest1 of
      Just (exp2, rest2) -> Just (AddExp exp1 exp2, rest2)
      Nothing -> Nothing
  result -> result

-- parser para subtrações
parseSub :: [Token] -> Maybe (Aexp, [Token])
parseSub tokens = case parseAdd tokens of
  Just (exp1, (MinusTok : rest1)) -> case parseSub rest1 of
      Just (exp2, rest2) -> Just (SubExp exp1 exp2, rest2)
      Nothing -> Nothing
  result -> result 

-- parser para expresões aritméticas dentro de parentesis
parseParenA :: [Token] -> Maybe (Aexp, [Token])
parseParenA (IntTok n : rest) = Just (IntExp n, rest)
parseParenA (OpenTok : rest1) = case parseSub rest1 of
    Just (exp, (CloseTok : rest2)) -> Just (exp, rest2)
    Just _ -> Nothing
    Nothing -> Nothing
parseParenA tokens = Nothing

-- parser para multiplicações dentro de parentesis
parseMulParen :: [Token] -> Maybe (Aexp, [Token])
parseMulParen tokens = case parseParenA tokens of
  Just (exp1, (TimesTok : rest1)) -> case parseMulParen rest1 of
      Just (exp2, rest2) -> Just (MulExp exp1 exp2, rest2)
      Nothing -> Nothing
  result -> result

-- parser para subtrações dentro de parentesis
parseSubParen :: [Token] -> Maybe (Aexp, [Token])
parseSubParen tokens = case parseMulParen tokens of
  Just (exp1, (MinusTok : rest1)) -> case parseSubParen rest1 of
      Just (exp2, rest2) -> Just (SubExp exp1 exp2, rest2)
      Nothing -> Nothing
  result -> result  

-- parser para adições dentro de parentesis
parseAddParen :: [Token] -> Maybe (Aexp, [Token])
parseAddParen tokens = case parseSubParen tokens of
  Just (exp1, (PlusTok : rest1)) -> case parseAddParen rest1 of
      Just (exp2, rest2) -> Just (AddExp exp1 exp2, rest2)
      Nothing -> Nothing
  result -> result  

-- Parser para expressões booleanas

-- parser principal das expressoes booleanas
parseB :: [Token] -> Maybe (Bexp, [Token])
parseB tokens = 
        parseTrue tokens
      <|> parseFalse tokens
      <|> parseLess tokens
      <|> parseEqual tokens
      <|> parseNeg tokens
      <|> parseAnd tokens
      <|> parseParenB tokens

-- parser para expressões booleanas dentro de parentesis
parseParenB :: [Token] -> Maybe (Bexp, [Token])
parseParenB (OpenTok : rest) =
  case parseB rest of
    Just (b, CloseTok : rest') -> Just (b, rest')
    Nothing -> Nothing
parseParenB tokens = Nothing

-- parser para valores verdadeiros
parseTrue :: [Token] -> Maybe (Bexp, [Token])
parseTrue (TrueTok : rest) = Just (TrueExp, rest)
parseTrue tokens = Nothing

-- parser para valores falsos
parseFalse :: [Token] -> Maybe (Bexp, [Token])
parseFalse (FalseTok : rest) = Just (FalseExp, rest)
parseFalse tokens = Nothing

-- parser para valores menor ou igual que outro valor
parseLess :: [Token] -> Maybe (Bexp, [Token])
parseLess tokens = case parseParenA tokens of
    Just (exp1, LessTok : rest1) -> case parseParenA rest1 of
        Just (exp2, rest2) -> Just (LeExp exp1 exp2, rest2)
        Nothing -> Nothing

-- parser para igualdades 
parseEqual :: [Token] -> Maybe (Bexp, [Token])
parseEqual tokens = case parseParenA tokens of
    Just (exp1, EqualTok : rest1) -> case parseParenA rest1 of
        Just (exp2, rest2) -> Just (EquExp exp1 exp2, rest2)
        Nothing -> Nothing      

-- parser para negar o valor lógico de um valor
parseNeg :: [Token] -> Maybe (Bexp, [Token])
parseNeg (NotTok : rest) = case parseB rest of
    Just (b, rest') -> Just (NegExp b, rest')
    Nothing -> Nothing
parseNeg tokens = Nothing

-- parser para a conjunção logica de dois valores
parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens = case parseB tokens of
    Just (b1, AndTok : rest) -> case parseB rest of
        Just (b2, rest') -> Just (AndExp b1 b2, rest')
        Nothing -> Nothing
    result -> result

-- Parser para statements

-- parser principal dos statements
parseS :: [Token] -> Maybe (Stm, [Token])
parseS tokens =
        parseIf tokens
      <|> parseWhile tokens
      <|> parseAssign tokens
      <|> parseAexp tokens

-- parser auxiliar que converte o output do parseS para uma lista de Statements
parseLoop :: [Token] -> Program -> Program
parseLoop [] s = s
parseLoop tokens s = case parseS tokens of
    Just (s1, rest1) -> parseLoop rest1 (s ++ [s1])
    Nothing -> error "Parse Error"        

-- parser para a condição if
parseIf :: [Token] -> Maybe (Stm, [Token])
parseIf tokens = case parseB tokens of
    Just (b, IfTok : rest1) -> case parseS rest1 of
        Just (s1, ElseTok : rest2) -> case parseS rest2 of
            Just (s2, rest3) -> Just (If b s1 s2, rest3)
            Nothing -> Nothing
        result -> result

-- parser para o ciclo while
parseWhile :: [Token] -> Maybe (Stm, [Token])
parseWhile tokens = case parseB tokens of
    Just (b, WhileTok : rest1) -> case parseS rest1 of
        Just (s, rest2) -> Just (While b s, rest2)
        Nothing -> Nothing

-- parser para o atribuidor assign
parseAssign :: [Token] -> Maybe (Stm, [Token])
parseAssign tokens = case parseVar tokens of
    Just (var, AssignTok : rest1) -> case parseA rest1 of
        Just (a, rest2) -> Just (Assign var a, rest2)
        Nothing -> Nothing   

-- parser para expressoes aritmeticas relacionadas com statements
parseAexp :: [Token] -> Maybe (Stm, [Token])
parseAexp tokens = case parseVar tokens of
    Just (var, AssignTok : rest1) -> case parseA rest1 of
        Just (a, rest2) -> Just (Assign var a, rest2)
        Nothing -> Nothing

-- parser para as variáveis 
parseVar :: [Token] -> Maybe (String, [Token])
parseVar (VarTok var : rest) = Just (var, rest)
parseVar tokens = Nothing        