import Parser
import ParserGenerator
import Control.Monad.State
import qualified Data.Map.Lazy as Map

-- State (functionMap,varMap,varStack,stackSize)
type CState = (Map.Map String Int, Map.Map String Int, [[String]], Int, Int)

getP = getParent . body
getL = getLeaf . body

base :: String
base = "section .data\n  .buf db '0000000000',10,0\n\nsection .text\n  global _start\n\n.skrivut:\n  mov eax,[esp+4]\n  mov edi,10\n  mov ecx,10\n  mov ebx,.buf+9\n.ploop:\n  mov edx,0\n  idiv edi\n  add edx,48\n  mov [ebx],dl\n  dec ebx\n  loop .ploop\n  mov eax,4\n  mov ebx,1\n  mov ecx,.buf\n  mov edx,11\n  int 0x80\n  ret\n\n"

baseEnd :: String
baseEnd = "  mov eax, 1\n  mov ebx, 0\n  int 0x80\n"

pushStack :: State CState ()
pushStack = do
    (fm, vm, vs, s, l) <- get
    put (fm, vm, vs, s+1, l)

popStack :: Int -> State CState String
popStack n = do
    (fm, vm, vs, s, l) <- get
    put (fm, vm, vs, s-n, l)
    return $ "  add esp," ++ (show $ 4*n) ++ "\n"

comp :: String -> AST -> State CState String
comp ins ast = do
    let [a, b] = getP ast
    codeA <- calc a
    codeB <- calc b
    (fm, vm, vs, s, l) <- get
    put (fm, vm, vs, s-1, l+1)
    let num = show l
    return $ codeA++codeB++"  pop eax\n  cmp [esp],eax\n  "++ins++" .test"++num++
        "\n  mov [esp],dword 0\n  jmp .end"++ num++"\n.test"++num++":\n  mov [esp],dword 1\n.end"++num++":\n"

summ :: AST -> State CState String
summ ast = do
    let x:xs = getP ast
    p <- prod x
    rest <- sequence $ map summCal xs
    return $ p ++ concat rest

summCal :: AST -> State CState String
summCal ast = do
    start <- prod $ head $ getP ast
    let cal = (case dataType ast of
         "add" -> "  add [esp],eax\n"
         "sub" -> "  sub [esp],eax\n"
         s -> "; damn: " ++ s ++ "\n"
         )
    end <- popStack 1
    return $ start ++ "  mov eax,[esp]\n" ++ end ++ cal


prod :: AST -> State CState String
prod ast = do
    let x:xs = getP ast
    el <- element x
    rest <- sequence $ map prodCal xs
    return $ el ++ concat rest

prodCal :: AST -> State CState String
prodCal ast = do
    start <- element $ head $ getP ast
    let cal = (case dataType ast of
         "mult" -> "  imul ebx\n"
         "div" -> "  idiv ebx\n"
         s -> "; damn: " ++ s ++ "\n"
         )
    end <- popStack 1
    return $ start ++ "  mov eax,[esp+4]\n  mov ebx,[esp]\n" ++ cal ++ end ++ "  mov [esp],eax\n"

calc :: AST -> State CState String
calc ast = case dataType ast of
    "sum" -> summ ast
    "lik" -> comp "jz" ast
    "ulik" -> comp "jnz" ast
    "mindre" -> comp "jl" ast
    "større" -> comp "jg" ast
    "mindreEL" -> comp "jle" ast
    "størreEL" -> comp "jge" ast
    "run" -> runProc 0 ast
    s -> return $ "; Damn " ++ s ++ "\n"

hvis :: AST -> State CState String
hvis ast = do
    let [testAst, ifAst, elseAst] = getP ast
    test <- calc testAst
    end <- popStack 1
    (fm, vm, vs, s, l) <- get
    put (fm, vm, []:vs, s, l+1)
    elseCode <- sequence $ map statement $ getP elseAst
    cleanup1 <- exitScope
    put (fm, vm, []:vs, s, l+1)
    ifCode <- sequence $ map statement $ getP ifAst
    cleanup2 <- exitScope
    let num = show l
    return $ test ++ "  cmp [esp],dword 0\n  pop eax\n  jne .if" ++ num ++ "\n" ++ (concat elseCode) ++ cleanup1 ++
        "  jmp .end" ++ num ++ "\n.if" ++ num ++ ":\n" ++ (concat ifCode) ++ cleanup2 ++ ".end" ++ num ++ ":\n"

mens :: AST -> State CState String
mens ast = do
    let [testAst, codeAst] = getP ast
    test <- calc testAst
    end <- popStack 1
    (fm, vm, vs, s, l) <- get
    put (fm, vm, []:vs, s, l+1)
    code <- sequence $ map statement $ getP codeAst
    cleanup <- exitScope
    let num = show l
    return $ " .begin" ++ num ++":\n" ++ test ++ "  cmp [esp],dword 0\n  pop eax\n  je .mens" ++ num ++ "\n" ++
        (concat code) ++ cleanup ++ "  jmp .begin" ++ num ++ "\n.mens" ++ num ++ ":\n"

element :: AST -> State CState String
element ast = case dataType ast of
    "num" -> num ast
    "word" -> word ast
    "parentes" -> calc $ head $ getP ast
    s -> return $ "; Damn " ++ s ++ "\n"

num :: AST -> State CState String
num ast = do
    pushStack
    return $ "  push " ++ (getL ast) ++ "\n"

word :: AST -> State CState String
word ast = do
    let varName = getL ast
    (_, vm, _, s, _) <- get
    pushStack
    case vm Map.!? varName of
        Just a -> return $ "  push dword [esp+"++(show $ 4*(s-a))++"]\n"
        Nothing -> error $ "Variabelen " ++ varName ++ " er ikke definert"

define :: AST -> State CState String
define ast = do
    let [wordAst, calcAst] = getP ast
    let varName = getL $ head $ getP wordAst
    (fm, vm, vs, s, l) <- get
    let _ = case vm Map.!? varName of
            Just a -> error $ "Variabelen " ++ varName ++ " er allerede definert."
            Nothing -> 2
    put (fm, Map.insert varName (s+1) vm, (varName:(head vs)):(tail vs), s, l)
    calc calcAst

setV :: AST -> State CState String
setV ast = do
    let [wordAst, calcAst] = getP ast
    let varName = getL wordAst
    (_, vm, _, s, _) <- get
    let point = case vm Map.!? varName of
            Nothing -> error $ "Variabelen " ++ varName ++ " er ikke definert."
            Just a -> a
    code <- calc calcAst
    end <- popStack 1
    return $ code ++ "  mov eax,[esp]\n" ++ end ++ "  mov [esp+"++(show $ 4*(s-point)) ++ "],eax\n"

runProc :: Int -> AST -> State CState String
runProc extra ast = do
    let [wordAst, inputAsts] = getP ast
    let funcName = getL wordAst
    let inputs = getP inputAsts
    (fm, _, _, _, _) <- get
    pushStack
    let params = case fm Map.!? funcName of
            Just x -> x
            Nothing -> error $ "Prosedyren " ++ funcName ++ " er ikke definert"
    if params == length inputs then do
        paramCode <- sequence $ map calc inputs
        end <- popStack $ params+extra
        return $ "  push 0\n" ++ concat paramCode ++ "  call ." ++ funcName ++ "\n" ++ end
        else error $ "Fikk ikke riktig antall inputs til " ++ funcName

exitScope :: State CState String
exitScope = do
    (fm, vm, vs, s, l) <- get
    let remove = (length $ head vs)
    let nvm = foldl (\a f -> f a) vm $ map Map.delete $ head vs
    put (fm, nvm, tail vs, s-remove, l)
    return $ "  add esp," ++ (show $ 4 * remove) ++ "\n"

exitAll :: State CState String
exitAll = do
    (fm, vm, vs, s, l) <- get
    return $ "  add esp," ++ (show $ 4*(sum $ map length $ init vs)) ++ "\n"

returner :: AST -> State CState String
returner ast = do
    code <- calc $ head $ getP ast
    popStack 1
    (_, _, _, s, _) <- get
    cleanup <- exitAll
    return $ code ++ "  pop eax\n  mov [esp+" ++ (show $ 4*s) ++ "], eax\n" ++ cleanup ++ "  ret\n"

proc :: AST -> State CState String
proc ast = do
    let [nameAst, typeAst, inputAsts, statementAsts] = getP ast
    let name = getL nameAst
    let inputs = map (getL . head . getP) $ getP inputAsts
    (fm, vm, vs, s, l) <- get
    put (Map.insert name (length inputs) fm, Map.fromList $ zip inputs [1..], [[],inputs], length inputs + 1, l)
    code <- sequence $ map statement $ getP statementAsts
    cleanup <- exitAll
    (_, _, _, _, nl) <- get
    put (Map.insert name (length inputs) fm, vm, vs, s, nl)
    return $ "." ++ name ++ ":\n" ++ (concat code) ++ cleanup ++ "  ret\n\n"

statement :: AST -> State CState String
statement ast = case dataType ast of
    "define" -> define ast
    "run void" -> runProc 1 ast
    "proc" -> proc ast
    "return" -> returner ast
    "hvis" -> hvis ast
    "mens" -> mens ast
    "set" -> setV ast
    s -> return $ "; Damned " ++ s ++ "\n"

sortStatements :: [String] -> (String, String)
sortStatements [] = ("","")
sortStatements (x:xs) = let (f,l) = sortStatements xs in if head x == '.' then (x++f, l) else (f, x++l)

main = do
    content <- getContents
    let res = parser content
    if isError res then putStrLn $ json res else do
        let ast = tree $ (\(Right a) -> a) res
        let programLines = getP ast
        putStrLn $ json res ++ "\n"

        let initState = (Map.fromList [("input", 0),("skrivut", 1)], Map.empty, [[]], 0, 1) :: CState
        let (res, endState) = (runState (sequence $ map statement programLines) initState) :: ([String], CState)
        let (funcs, code) = sortStatements res
        putStrLn $ funcs ++ "_start:\n" ++ code
        print endState


        let out = base ++ funcs ++ "\n\n_start:" ++ code ++ baseEnd
        writeFile "out.asm" out

{-
section .data\n  .buf db '0000000000',10,0\n\nsection .text\n  global _start\n\n.print:\n  mov eax,[esp+4]\n  mov edi,10\n  mov ecx,10\n  mov ebx,.buf+9\n.ploop:\n  mov edx,0\n  div edi\n  add edx,48\n  mov [ebx],dl\n  dec ebx\n  loop .ploop\n  mov eax,4\n  mov ebx,1\n  mov ecx,.buf\n  mov edx,11\n  int 0x80\n  ret
_start:
    push 20
    call .print
    call .print
    pop eax

    mov eax,1
    int 0x80
-}
