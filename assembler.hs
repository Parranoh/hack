{-# LANGUAGE BinaryLiterals #-}
import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (foldl')
import Data.Bits (testBit,(.|.),shiftL)
import Data.Maybe (fromJust)

main :: IO ()
main = interact $ unlines . map toBinary . assemble . removeComments . lines

removeComments :: [String] -> [String]
removeComments = filter (not . null) . map (trim . rmcomm) where
    trim = filter (not . isWhitespace)
    rmcomm l = case break (== '/') l of
        (l','/':'/':_) -> l'
        _              -> l

type SymbolTable = Map String Word

lookup :: String -> SymbolTable -> Word
lookup s tbl = case M.lookup s tbl of
    Just v -> v
    _      -> read s

assemble :: [String] -> [Word]
assemble prgm = machineCode (symbolTable prgm) prgm

symbolTable :: [String] -> SymbolTable
symbolTable prgm = vars . labels $ predefinedSymbols where
    labels :: SymbolTable -> SymbolTable
    labels s = snd . foldl' f (0,s) $ prgm where
        -- error if already exists
        f old@(ln,table) line@('(':_:_) = if all isDigit lbl then old else (ln,M.insertWith (error $ "multiply defined label" ++ lbl) lbl ln table)
            where lbl = init . tail $ line
        f (ln,table)     _              = (succ ln,table)
    vars :: SymbolTable -> SymbolTable
    vars s = snd . foldl' f (16,s) $ prgm where
        -- nop if already exists
        f (next,table) ('@':var) = (succ next,M.insertWith (flip const) var next table)
        f other        _         = other

predefinedSymbols :: SymbolTable
predefinedSymbols = M.fromList $
    [ ("SP"    ,0    )
    , ("LCL"   ,1    )
    , ("ARG"   ,2    )
    , ("THIS"  ,3    )
    , ("THAT"  ,4    )
    , ("SCREEN",16384)
    , ("KBD"   ,24576)
    ]

machineCode :: SymbolTable -> [String] -> [Word]
machineCode table = concatMap $ \line -> case line of
    '(':_   -> []
    '@':sym -> [lookup sym table]
    instr   -> case break (== '=') instr of
        (dest@(_:_),'=':expr) -> [encodeDest dest .|. encodeExpr expr]
        (expr,"")             -> [encodeExpr expr]
        _                     -> error $ "invalid instruction" ++ instr
    where
        encodeDest, encodeExpr, encodeJmp :: String -> Word
        encodeDest d = flip shiftL 3 $ case d of
            "M"   -> 0b001
            "D"   -> 0b010
            "MD"  -> 0b011
            "A"   -> 0b100
            "AM"  -> 0b101
            "AD"  -> 0b110
            "AMD" -> 0b101
            _     -> error $ "invalid destination mnemonic " ++ d
        encodeExpr e = case break (== ';') e of
            (expr,(';':jmp)) -> encodeExpr expr .|. encodeJmp jmp
            (expr,"")        -> shiftL 0b111 13 .|. (flip shiftL 12 . toEnum . fromEnum $ 'M' `elem` expr) .|. flip shiftL 6 (case replace 'M' 'A' expr of
                "0"   -> 0b101010
                "1"   -> 0b111111
                "-1"  -> 0b111010
                "D"   -> 0b001100
                "A"   -> 0b110000
                "~D"  -> 0b001101
                "!D"  -> 0b001101
                "~A"  -> 0b110001
                "!A"  -> 0b110001
                "-D"  -> 0b001111
                "-A"  -> 0b110011
                "D+1" -> 0b011111
                "A+1" -> 0b110111
                "D-1" -> 0b001110
                "A-1" -> 0b110010
                "D+A" -> 0b000010
                "A+D" -> 0b000010
                "D-A" -> 0b010011
                "A-D" -> 0b000111
                "D&A" -> 0b000000
                "A&D" -> 0b000000
                "D|A" -> 0b010101
                "A|D" -> 0b010101
                _ -> error $ "invalid expression mnemonic " ++ expr
                )
        encodeJmp  j = case j of
            "JGT" -> 0b001
            "JEQ" -> 0b010
            "JGE" -> 0b011
            "JLT" -> 0b100
            "JNE" -> 0b101
            "JLE" -> 0b110
            "JMP" -> 0b111
            _     -> error $ "invalid jump mnemonic " ++ j

toBinary :: Word -> String
toBinary x = concatMap (show . fromEnum . testBit x) [15,14..0]

replace :: Eq a => a -> a -> [a] -> [a]
replace from to = map (\x -> if x == from then to else x)

isDigit, isWhitespace :: Char -> Bool
isDigit = flip elem ['0'..'9']
isWhitespace = flip elem " \n\t\r" -- >:(
