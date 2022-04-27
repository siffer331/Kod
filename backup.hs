import Parser
import ParserGenerator
import Control.Monad.State
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

-- State (functionSet,varMap,varStack)
type CState = (Set.Set String, Map.Map String Int, [[String]])

getP = getParent . body
getL = getLeaf . body

base :: String
base = ".data:\n  global _start\n\n_start:\n"

baseEnd :: String
baseEnd = "  mov eax, 1\n  mov ebx, 0\n  int 0x80\n"

summ :: AST -> State CState String
summ ast = let x:xs = getP ast in
    prod x ++ (concat $ map summCal xs)

summCal :: AST -> State CState String
summCal ast = (element $ (getP ast) !! 0) ++ (case dataType ast of
    "add" -> "  add esp-1 [esp]\n"
    "sub" -> "  sub esp-1 [esp]\n"
    s -> s
    ) ++ "  pop\n"


prod :: AST -> State CState String
prod ast = let x:xs = getP ast in
    element x ++ (concat $ map prodCal xs)

prodCal :: AST -> State CState String
prodCal ast = "; calc prdo\n"


element :: AST -> State CState String
element ast = case dataType ast of
    "num" -> num ast
    "word" -> word ast
    s -> s

num :: AST -> State CState String
num ast = return $ "  push " ++ (getL ast) ++ "\n"

word :: AST -> State CState String
word ast = let w = getK ast in \(fs, vm, vs) -> if lookup w  vm == Nothing
    then (" push [ ; push word later", 
    else error $ "Variablen " ++ w ++ " er ikke defineret"

statement :: AST -> State CState String
statement ast = do
    case dataType ast of
        "define" -> summCal $ ((getP ast) !! 1)

main = do
    content <- getContents
    let res = parser content
    if isError res then putStrLn $ json res else do
        let ast = tree $ (\(Right a) -> a) res
        let lines = getP ast
        putStrLn $ json res
        print funcs
	let initState = (Set.empty, Map.empty, [])
        putStrLn $ statement $ lines !! 0


        let out = base ++ baseEnd
        writeFile "out.asm" out
