module ParserGenerator
( Body, AST, ParserData, ParserResult, Parser, getParent, getLeaf, dataType, body, tree, rest
, json, jsonAst, isError, setType, concatResult, compactResult, pack
, char, string
, anyParser, manyParser, sequenceParser, choiceParser
) where

fix :: (a -> a) -> a
fix f = f (fix f)

class Tree a where
    getText :: a -> String

data Body = Leaf String | Parent [AST] deriving (Show)
data AST = AST {dataType :: String, body :: Body} deriving (Show)
data ParserData = Tree {tree :: AST, rest :: String} deriving (Show)

type ParserResult = Either String ParserData
type Parser = String -> ParserResult

getLeaf :: Body -> String
getLeaf (Leaf a) = a

getParent :: Body -> [AST]
getParent (Parent a) = a

basicAST :: String -> String -> AST
basicAST dataType text = AST {dataType = dataType, body = Leaf text}

basic :: String -> String -> String -> ParserResult
basic dataType text rest = Right $ basicData dataType text rest

basicData :: String -> String -> String -> ParserData
basicData dataType text rest = Tree {tree = basicAST dataType text, rest = rest}

parentAST :: String -> [AST] -> AST
parentAST dataType asts = AST {dataType = dataType, body = Parent asts}

parentData :: String -> [AST] -> String -> ParserData
parentData dataType asts rest = Tree {tree = parentAST dataType asts, rest = rest}

parent :: String -> [AST] -> String -> ParserResult
parent dataType asts rest = Right $ parentData dataType asts rest

instance Tree Body where
    getText (Leaf text) = text
    getText (Parent []) = ""
    getText (Parent xs) = foldl1 (++) (map getText xs)

instance Tree AST where
    getText AST{body = body} = getText body

jsonBody:: Body -> String
jsonBody (Leaf text) = "\"" ++ text ++ "\""
jsonBody (Parent []) = "[]"
jsonBody (Parent xs) = "[" ++ foldr1 ((++) . (++ ", ")) (map jsonAst xs) ++ "]"

jsonAst:: AST -> String
jsonAst AST{dataType = dataType, body = body} =
    "{\"type\": \"" ++ dataType ++ "\", \"body\": " ++ jsonBody body ++ "}"

json:: ParserResult -> String
json (Left text) = "{\"error\": \"" ++ text ++ "\"}"
json (Right (Tree {tree = tree, rest = rest})) =
    "{\"tree\": " ++ jsonAst tree ++ ", \"rest\": \"" ++ rest ++ "\"}"

getChildren' :: Int -> [AST] -> Either String [AST]
getChildren' _ [] = Right []
getChildren' n (ast:xs) = case getChildren (n-1) ast of
    (Left msg) -> Left msg
    Right res  -> fmap (res++) $ getChildren' n xs

getChildren :: Int -> AST -> Either String [AST]
getChildren 0 ast = Right [ast]
getChildren _ AST{body=(Leaf t)} = Left "Cant get children of Leaf"
getChildren n AST{body=(Parent asts)} = getChildren' n asts

repeatData :: Parser -> String -> ([AST], String)
repeatData parser s = run s
    where run = fix (\f text -> case parser text of
            (Left s)  -> ([], text)
            (Right (Tree t r)) -> let (asts, rest) = f $ r in (t:asts, rest)
            )

sequenceData :: [Parser] -> String -> Either String ([AST], String)
sequenceData [] s = Right ([], s)
sequenceData (parser:xs) s = case parser s of
    (Left s)    -> Left s
    (Right res) -> (case sequenceData xs $ rest res of
            (Left s)             -> Left s
            (Right (asts, rest)) -> if (dataType $ tree res) == "ignore" then Right (asts, rest) else Right ((tree res):asts, rest)
            )

isError :: ParserResult -> Bool
isError (Left s) = True
isError _ = False

changeType :: String -> ParserData -> ParserData
changeType newType (Tree {tree = (AST {body = body}), rest = rest}) =
    Tree {tree = (AST {dataType = newType, body = body}), rest = rest}

concatResultData :: ParserData -> ParserData
concatResultData (Tree tree rest) =
    basicData (dataType tree) (getText $ body tree) $ rest

concatResult :: ParserResult -> ParserResult
concatResult = fmap concatResultData

compactLayer :: String -> Int -> ParserData -> ParserResult
compactLayer dataType n res = fmap ((flip $ parentData dataType) $ rest res) (getChildren n $ tree res)

compactResult :: String -> Int -> ParserResult -> ParserResult
compactResult _ _ (Left s) = Left s
compactResult dataType n (Right res) = compactLayer dataType (n+1) res

packData :: ParserData -> ParserData
packData da = parentData "pack" [tree da] $ rest da

pack :: ParserResult -> ParserResult
pack = fmap packData

char :: (Char -> Bool) -> Parser
char _ "" = Left "Cant get char of empty string"
char f text
    | f $ head text = basic "Char" (head text : []) $ tail text
    | otherwise     = Left $ "Cant recognize the first char in '" ++ text ++ "'"

string :: String -> Parser
string word text
    | length word > length text       = Left $ "'" ++ text ++ "' does not start with '" ++ word ++ "'"
    | take (length word) text == word = basic "Word" word $ drop (length word) text
    | otherwise                       = Left $ "'" ++ text ++ "' does not start with '" ++ word ++ "'"

setType :: String -> Parser -> Parser
setType newType parser = fmap (changeType newType) . parser

anyParser :: Parser -> Parser
anyParser parser s = parent "any" asts rest
    where (asts, rest) = repeatData parser s

manyParser :: Parser -> Parser
manyParser parser s = if length asts == 0 then Left $ "Parser could not parse " ++ s else parent "many" asts rest
    where (asts, rest) = repeatData parser s

sequenceParser :: [Parser] -> Parser
sequenceParser parsers = fmap (\(asts, rest) -> parentData "sequence" asts rest) . sequenceData parsers

choiceParser :: [Parser] -> Parser
choiceParser [] s = Left $ "Could not parse any parsers in choice on '" ++ s ++ "'"
choiceParser (parser:xs) s = case parser s of
    (Left msg)    -> choiceParser xs s
    res -> res


a = concatResult . (manyParser $ char ('a' ==))
b = concatResult . (manyParser $ char ('b' ==))
c = concatResult . (manyParser $ char ('c' ==))
d = concatResult . (manyParser $ char ('d' ==))

ab = compactResult "Yay" 2 . (manyParser $ choiceParser [sequenceParser [a,b], sequenceParser [c,d]])





