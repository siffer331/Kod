module Parser(parser) where
import Prelude hiding (any, sequence, concat)
import Data.Text (replace)
import ParserGenerator
import Data.Char

any = anyParser
many = manyParser
sequence = sequenceParser
choice = choiceParser
concat = concatResult
compact = compactResult


ignore :: Parser -> Parser
ignore = setType "ignore"

fil :: String -> Parser
fil = ignore . string

whitespace :: Parser
whitespace = ignore $ many $ char isSpace

letter :: Parser
letter = char isLetter

convert :: String -> String
convert [] = []
convert (x:xs) = if x == '-' then '_':(convert xs) else x:(convert xs)

word :: Parser
word = transformLeaf convert . (setType "word" $ concat . sequence [letter, any $ choice [sequence [string "-", letter], letter]])

sep :: Parser -> Parser -> Parser
sep p1 p2 = compact "sep" 2 . (sequence [pack . pack . p1, any $ (sequence [p2,p1])])

liste :: Parser -> Parser
liste p = choice [compact "liste" 2 . sequence [sep p (fil ", "), sequence [fil " og ", p]], setType "liste" $ sequence [p]]

genType :: String -> Parser
genType s = setType s $ sequence [fil (s++" "), word]

typer :: [Parser]
typer = map genType ["heltallet", "sannhetsværdien", "teksten"]

typer' :: [Parser]
typer' = map string ["et heltall", "en sannhetsværdi", "en tekst", "ingenting"]


---- Math ----

comp :: (String, String) -> Parser
comp (dataType, text) = setType dataType $ sequence [summ, fil text, summ]

calc :: Parser
calc = choice $ [run] ++ map comp
    [ ("lik", " er lik ")
    , ("ulik", " ikke er lik ")
    , ("mindre"," er mindre enn ")
    , ("større", " er større enn ")
    , ("størreEL", " er større enn eller lik ")
    , ("mindreEL", " er mindre enn eller lik ")
    ] ++ [summ]

parentes :: Parser -> Parser
parentes p = setType "parentes" $ sequence[fil "(", p, fil ")"]

tal :: Parser
tal = setType "num" $ concat . (many $ char isDigit)

element :: Parser
element = choice [tal, word, parentes calc]

mult :: Parser
mult = setType "mult" $ sequence [fil " ganger ", element]

divi :: Parser
divi = setType "div" $ sequence [fil " delt på ", element]

prod :: Parser
prod = compact "prod" 1 . sequence [pack . element, any $ choice [mult, divi]]

add :: Parser
add = setType "add" $ sequence [fil " pluss ", prod]

sub :: Parser
sub = setType "sub" $ sequence [fil " minus ", prod]

summ :: Parser
summ = compact "sum" 1 . sequence [pack . prod, any $ choice [add, sub]]

--------------

define :: Parser
define = setType "define" $ sequence [fil "La ", choice typer, fil " være ", calc, fil "."]

setV :: Parser
setV = setType "set" $ sequence [fil "Sett ", word, fil " til ", calc, fil "."]

run :: Parser
run = setType "run" $ sequence [word, fil " på ", liste calc]

runStatement :: Parser
runStatement = compact "run void" 1 . sequence [fil "Kjør ", run, fil "."]

statements :: Parser
statements = setType "statements" $ sep (choice [runStatement, define, hvis, mens, setV, returner]) whitespace

statements' :: Parser
statements' = setType "statements" $ sep (choice [runStatement, define, hvis, mens, setV, proc]) whitespace

statementBlock :: Parser
statementBlock = compact "statements" 1 . choice [sequence [statements, whitespace, fil "Og ikke mer."], pack . sequence [fil "Og ikke mer."]]

returner :: Parser
returner = setType "return" $ sequence [fil "Returner ", calc, fil "."]

hvis :: Parser
hvis = setType "hvis" $ sequence [fil "Hvis ", calc, fil " gjør følgende:", whitespace, statementBlock, choice [ellers, setType "statements" $ sequence []]]

ellers :: Parser
ellers = compact "statements" 1 . sequence [whitespace, fil "Ellers gjør følgende:", whitespace, statementBlock]

mens :: Parser
mens = setType "mens" $ sequence [fil "Mens ", calc, fil " gjør følgende:", whitespace, statementBlock]

proc :: Parser
proc = setType "proc" $ sequence [
        fil "La prosedyren "
        , word
        , fil " som gir "
        , choice typer'
        , fil " fra "
        , liste $ choice (typer++[fil "ingenting"])
        , fil " være følgende:"
        , whitespace
        , statementBlock
        ]


parser = compact "statements" 1 . sequence [statements', ignore $ any whitespace]
