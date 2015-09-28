-- | Interpreter for the __Intuitive Language__.
module Main where

import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Text.Parsec         hiding (State)

-- | Type definition for the token parser.
type TokenParser = Parsec String ()

-- | Type definition for the action parser.
type ActionParser = State [Token]

-- | Definitions of lexical units.
data Token = Var String  -- ^ Variable name.
           | Number NumT -- ^ Number as fractional value.
           | KIs         -- ^ Keyword @is@.
           | KFunc       -- ^ Keywords @function of@.
           | KDo         -- ^ Keyword @do@.
           | KTo         -- ^ Keyword @to@.
           | KAssign     -- ^ Keyword @assign@.
           | KWhat       -- ^ Keyword @what@.
           | KAnd        -- ^ Keyword @and@.
           | SDot        -- ^ Symbol \'@.@\'.
           | SComma      -- ^ Symbol \'@,@\'.
           | SColon      -- ^ Symbol \'@:@\'.
           | SExcl       -- ^ Symbol \'@!@\'.
           | SQuestion   -- ^ Symbol \'@?@\'.
           | SBraceL     -- ^ Symbol \'@{@\'.
           | SBraceR     -- ^ Symbol \'@}@\'.
           | SBracketL   -- ^ Symbol \'@[@\'.
           | SBracketR   -- ^ Symbol \'@]@\'.
           | SParenL     -- ^ Symbol \'@(@\'.
           | SParenR     -- ^ Symbol \'@)@\'.
           | OpAdd       -- ^ Operator \'@+@\'.
           | OpSub       -- ^ Operator \'@-@\'.
           | OpMult      -- ^ Operator \'@*@\'.
           | OpDiv       -- ^ Operator \'@/@\'. Doubles as fractional bar.
           | UPlus       -- ^ Unary \'@+@\'.
           | UMinus      -- ^ Unary \'@-@\'.
           deriving (Show, Eq)

-- | Numeric type for the interpreter: a fraction.
data NumT = NumT Int Int
    deriving (Show, Eq)

-- | TODO: Definition of an expresssion.
data Expr = Expr

-- | Definitions of actions.
data Action = Decl String [Expr]
            | Assign
            | Loop
            | Ask

-- | Converts token to symbolic representation.
tToS :: Token -> String
tToS t = case t of
    KIs       -> "is"
    KDo       -> "do"
    KAssign   -> "assign"
    KWhat     -> "what"
    KAnd      -> "and"
    KTo       -> "to"
    SDot      -> "."
    SComma    -> ","
    SColon    -> ":"
    SExcl     -> "!"
    SQuestion -> "?"
    SBraceL   -> "{"
    SBraceR   -> "}"
    SBracketL -> "["
    SBracketR -> "]"
    SParenL   -> "("
    SParenR   -> ")"
    OpAdd     -> "+"
    OpSub     -> "-"
    OpMult    -> "*"
    OpDiv     -> "/"
    UPlus     -> "+"
    UMinus    -> "-"
    _         -> error "Unmatched token"

-- | Parses a variable name.
parseVariable :: TokenParser Token
parseVariable = Var <$> ((:) <$> letter <*> many (letter <|> digit))

-- | Parses an arbitrary token from its symbolic representation.
parseToken :: Token -> TokenParser Token
parseToken t = string (tToS t) >> return t

-- | Parses keywords \'@function of@\'.
parseFunction :: TokenParser Token
parseFunction = string "function" *> spaces *> string "of" *> return KFunc

-- | Parses a fractional value. Denominator can be omitted if equal to 1.
parseNumber :: TokenParser NumT
parseNumber = NumT <$> parseSignedNumber <*> parseDenominator where
    parseDenominator = option 1 (spaces >> parseToken OpDiv >> spaces >> parseSignedNumber)

-- | Parse value of signed number.
parseSignedNumber :: TokenParser Int
parseSignedNumber = read <$> (mplus <$> parseSign <*> many1 digit) where
    parseSign = option "" (string (tToS UMinus) <|> return "" <* string (tToS UPlus))

-- | Parse a function or a variable declaration.
-- | Variables can be declared using 'function of 0' (like constant function),
-- | or without keywords 'function of' (like regular variable).
parseDeclaration :: ActionParser [Action]
parseDeclaration = do
    [Var i, KIs] <- fetch 2
    f <- peek
    n <- case f of
        KFunc -> parseFuncN
        _     -> return 1
    exprs <- concat <$> sequence (intersperse (ignoreToken SComma) (replicate n (return <$> parseExpr :: ActionParser [Expr])))
    _ <- ignoreToken SDot :: ActionParser [a]
    return [Decl i exprs]

-- | Ignores a token from source. Returns an empty monoid if token is found, and @undefined@ otherwise. 
ignoreToken :: Monoid a => Token -> State [Token] a
ignoreToken t = do
    t' <- head <$> fetch 1
    return $ if t' == t then mempty else undefined

-- | TODO: Parses an expression.
parseExpr :: ActionParser Expr
parseExpr = undefined

-- | TODO: Evaluates an expression.
evalExpr :: Expr -> Int
evalExpr = undefined

-- | Parses the number of arguments from a function declaration.
parseFuncN :: ActionParser Int
parseFuncN = do
    [KFunc, Number (NumT n _), SColon] <- fetch 3
    return n

-- | @fetch n@ fetches first n elements from state, and removes them from it.
fetch :: Int -> State [s] [s]
fetch n = do
    (f, rest) <- splitAt n <$> get
    put rest
    return f

-- | Peeks into state.
peek :: State [s] s
peek = head <$> get

-- | Main program.
main :: IO ()
main = undefined

