module Parser(parser) where
import Prelude hiding (any, sequence, concat)
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

word :: Parser
word = setType "word" $ concat . sequence [letter, any $ choice [sequence [string "-", letter], letter]]

sep :: Parser -> Parser -> Parser
sep p1 p2 = compact "sep" 2 . (sequence [pack . pack . p1, any $ (sequence [p2,p1])])

liste :: Parser -> Parser
liste p = choice [compact "liste" 2 . sequence [sep p (fil ", "), sequence [fil " og ", p]], setType "liste" $ sequence [p]]

genType :: String -> Parser
genType s = setType s $ sequence [fil (s++" "), word]

typer :: [Parser]
typer = map genType ["heltallet", "sandhedsværdien", "teksten"]

typer' :: [Parser]
typer' = map string ["et heltal", "en sandhedsværdi", "en tekst", "intet"]


---- Math ----

comp :: (String, String) -> Parser
comp (dataType, text) = setType dataType $ sequence [summ, fil text, summ]

calc :: Parser
calc = choice $ map comp
    [ ("lig", " er lig med ")
    , ("iLig", " ikke er lig med ")
    , ("mindre"," er mindre end ")
    , ("større", " er større end ")
    , ("størreEL", " er større eller lig med ")
    , ("mindreEL", " er mindre eller lig med ")
    ] ++ [summ]

parentes :: Parser -> Parser
parentes p = setType "parentes" $ sequence[fil "(", p, fil ")"]

tal :: Parser
tal = setType "num" $ concat . (many $ char isDigit)

element :: Parser
element = choice [tal, word, run, parentes calc]

mult :: Parser
mult = setType "mult" $ sequence [fil " gange ", element]

divi :: Parser
divi = setType "div" $ sequence [fil " divideret med ", element]

prod :: Parser
prod = compact "prod" 1 . sequence [pack . element, any $ choice [mult, divi]]

add :: Parser
add = setType "add" $ sequence [fil " plus ", prod]

sub :: Parser
sub = setType "sub" $ sequence [fil " minus ", prod]

summ :: Parser
summ = compact "summ" 1 . sequence [pack . prod, any $ choice [add, sub]]

--------------

define :: Parser
define = setType "define" $ sequence [fil "Lad ", choice typer, fil " være ", calc, fil "."]

setV :: Parser
setV = setType "set" $ sequence [fil "Sæt ", word, fil " til at være ", calc, fil "."]

run :: Parser
run = setType "run" $ sequence [word, fil " på ", liste calc]

runStatement :: Parser
runStatement = compact "run void" 1 . sequence [fil "Udfør ", run, fil "."]

statements :: Parser
statements = setType "statements" $ sep (choice [runStatement, define, hvis, mens, setV, returner]) whitespace

statements' :: Parser
statements' = setType "statements" $ sep (choice [runStatement, define, hvis, mens, setV, proc]) whitespace

statementBlock :: Parser
statementBlock = compact "statements" 1 . choice [sequence [statements, whitespace, fil "Og ikke mere."], pack . sequence [fil "Og ikke mere."]]

returner :: Parser
returner = setType "return" $ sequence [fil "Retuner ", calc, fil "."]

hvis :: Parser
hvis = setType "hvis" $ sequence [fil "Hvis ", calc, fil " gør følgende:", whitespace, statementBlock, choice [ellers, setType "statements" $ sequence []]]

ellers :: Parser
ellers = compact "statements" 1 . sequence [whitespace, fil "Ellers gør følgende:", whitespace, statementBlock]

mens :: Parser
mens = setType "mens" $ sequence [fil "Mens ", calc, fil " gør følgende:", whitespace, statementBlock]

proc :: Parser
proc = setType "proc" $ sequence [
        fil "Lad procedyren "
        , word
        , fil " som giver "
        , choice typer'
        , fil " fra "
        , liste $ choice (typer++[fil "intet"])
        , fil " være følgende:"
        , whitespace
        , statementBlock
        ]


parser = compact "statements" 1 . sequence [statements', ignore $ any whitespace]
