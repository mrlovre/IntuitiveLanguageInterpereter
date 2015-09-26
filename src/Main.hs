-- | Interpreter for the __Intuitive Language__.
module Main where

import           Text.Parsec

-- | Parsec type constraints.
type ParserType = Parsec String () Token

-- | Definitions of lexical units.
data Token = Var String -- ^ Variable name.
           | Number Int Int -- ^ Number as fractional value.
           | KIs -- ^ Keyword @is@.
           | KFunc -- ^ Keywords @function of@.
           | KDo -- ^ Keyword @do@.
           | KAssign -- ^ Keyword @assign@.
           | KWhat -- ^ Keyword @what@.
           | KAnd -- ^ Keyword @and@.
           | SDot -- ^ Symbol \'@.@\'.
           | SComma -- ^ Symbol \'@,@\'.
           | SColon -- ^ Symbol \'@:@\'.
           | SExcl -- ^ Symbol \'@!@\'.
           | SQuestion -- ^ Symbol \'@?@\'.
           | SBraceL -- ^ Symbol \'@{@\'.
           | SBraceR -- ^ Symbol \'@}@\'.
           | SBracketL -- ^ Symbol \'@[@\'.
           | SBracketR -- ^ Symbol \'@]@\'.
           | SParenL -- ^ Symbol \'@(@\'.
           | SParenR -- ^ Symbol \'@)@\'.
           | OpAdd -- ^ Operator \'@+@\'.
           | OpSub -- ^ Operator \'@-@\'.
           | OpMult -- ^ Operator \'@*@\'.
           | OpDiv -- ^ Operator \'@/@\'. Doubles as fractional bar.
           | UPlus -- ^ Unary \'@+@\'.
           | UMinus -- ^ Unary \'@-@\'.
           deriving (Show, Eq)

-- | Converts token to symbolic representation.
tToS :: Token -> String
tToS t = case t of
    KIs       -> "is"
    KDo       -> "do"
    KAssign   -> "assign"
    KWhat     -> "what"
    KAnd      -> "and"
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
parseVariable :: ParserType
parseVariable = Var <$> ((:) <$> letter <*> many (letter <|> digit))

-- | Parses an arbitrary token from its symbolic representation.
parseToken :: Token -> ParserType
parseToken t = string (tToS t) >> return t

-- | Parses keywords \'@function of@\'.
parseFunction :: ParserType
parseFunction = string "function" >> spaces >> string "of" >> return KFunc

-- | Parses a fractional value. Denominator can be omitted if equal to 1.
parseNumber :: ParserType
parseNumber = Number <$> parseSignedNumber <*> parseDenominator where
    parseDenominator = option 1 (spaces >> parseToken OpDiv >> spaces >> parseSignedNumber)

-- | Parse value of signed number.
parseSignedNumber :: Parsec String () Int
parseSignedNumber = read <$> ((++) <$> parseSign <*> many1 digit) where
    parseSign = option "" (string (tToS UMinus) <|> return "" <* string (tToS UPlus))

-- | Main program.
main :: IO ()
main = undefined
