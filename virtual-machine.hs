import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.Trans.State.Lazy (State,state)

main :: IO ()
main = interact
    $ ($ "")
    . compose
    . flip S.evalState (error "code must start with function")
    . traverse genCode
    . map (takeWhile (/= '/'))
    . lines
    where compose = foldr (.) id

type FuncName = ShowS

genCode :: String -> State (FuncName,Word) ShowS
genCode l = fmap ((.) $ showString "// " . showString l . showChar '\n') $ case words l of
    [                     ] -> return $ showString ""
    ["add"                ] -> return $ showString "\
        \@SP\n\
        \AM=M-1\n\
        \D=M\n\
        \A=A-1\n\
        \M=D+M\n"
    ["sub"                ] -> return $ showString "\
        \@SP\n\
        \AM=M-1\n\
        \D=M\n\
        \A=A-1\n\
        \M=M-D\n"
    ["neg"                ] -> return $ showString "\
        \@SP\n\
        \A=M-1\n\
        \M=-M\n"
    ["eq"                 ] -> state $ \(fn,nLbl) -> (let lbl = fn . showString "$$" . shows nLbl in showString "\
            \@SP\n\
            \AM=M-1\n\
            \D=M\n\
            \A=A-1\n\
            \D=M-D\n\
            \M=0\n\
            \@" . lbl . showString "\n\
            \D;JNE\n\
            \@SP\n\
            \A=M-1\n\
            \M=-1\n\
            \(" . lbl . showString ")\n"
        ,(fn,succ nLbl))
    ["gt"                 ] -> state $ \(fn,nLbl) -> (let lbl = fn . showString "$$" . shows nLbl in showString "\
            \@SP\n\
            \AM=M-1\n\
            \D=M\n\
            \A=A-1\n\
            \D=M-D\n\
            \M=0\n\
            \@" . lbl . showString "\n\
            \D;JLE\n\
            \@SP\n\
            \A=M-1\n\
            \M=-1\n\
            \(" . lbl . showString ")\n"
        ,(fn,succ nLbl))
    ["lt"                 ] -> state $ \(fn,nLbl) -> (let lbl = fn . showString "$$" . shows nLbl in showString "\
            \@SP\n\
            \AM=M-1\n\
            \D=M\n\
            \A=A-1\n\
            \D=M-D\n\
            \M=0\n\
            \@" . lbl . showString "\n\
            \D;JGE\n\
            \@SP\n\
            \A=M-1\n\
            \M=-1\n\
            \(" . lbl . showString ")\n"
        ,(fn,succ nLbl))
    ["and"                ] -> return $ showString "\
        \@SP\n\
        \AM=M-1\n\
        \D=M\n\
        \A=A-1\n\
        \M=D&M\n"
    ["or"                 ] -> return $ showString "\
        \@SP\n\
        \AM=M-1\n\
        \D=M\n\
        \A=A-1\n\
        \M=D|M\n"
    ["not"                ] -> return $ showString "\
        \@SP\n\
        \A=M-1\n\
        \M=~M\n"
    ["push","constant","0"] -> return $ showString "\
        \@SP\n\
        \M=M+1\n\
        \A=M-1\n\
        \M=0\n"
    ["push","constant","1"] -> return $ showString "\
        \@SP\n\
        \M=M+1\n\
        \A=M-1\n\
        \M=1\n"
    ["push","constant",ix ] -> return $ showString "\
        \@" . showString ix . showString "\n\
        \D=A\n\
        \@SP\n\
        \M=M+1\n\
        \A=M-1\n\
        \M=D\n"
    ["push"    ,seg   ,ix ] -> let i = read ix :: Int in case globalSegment seg of
        Just s -> return $ showString "\
            \@" . shows (i + s) . showString "\n\
            \D=M\n\
            \@SP\n\
            \M=M+1\n\
            \A=M-1\n\
            \M=D\n"
        _      -> case derefSegment seg of
            Just s
                | i <= 3 -> return $ showString "\
                    \@" . showString s . showString "\n\
                    \A=M" . (if i > 0 then showString "+1" else id) . showChar '\n' .
                    (nTimes (i - 1) $ showString "A=A+1\n") . showString
                    "D=M\n\
                    \@SP\n\
                    \M=M+1\n\
                    \A=M-1\n\
                    \M=D\n"
                | otherwise -> return $ showString "\
                    \@" . showString ix . showString "\n\
                    \D=A\n\
                    \@" . showString s . showString "\n\
                    \A=D+M\n\
                    \D=M\n\
                    \@SP\n\
                    \M=M+1\n\
                    \A=M-1\n\
                    \M=D\n"
            _ -> error $ "unknown segment " ++ seg
    ["pop"     ,seg ,ix   ] -> let i = read ix :: Int in case globalSegment seg of
        Just s -> return $ showString "\
            \@SP\n\
            \AM=M-1\n\
            \D=M\n\
            \@" . shows (i + s) . showString "\n\
            \M=D\n"
        _      -> case derefSegment seg of
            Just s
                | i <= 7 -> return $ showString "\
                    \@SP\n\
                    \AM=M-1\n\
                    \D=M\n\
                    \@" . showString s . showString "\n\
                    \A=M" . (if i > 0 then showString "+1" else id) . showChar '\n' .
                    (nTimes (i - 1) $ showString "A=A+1\n") . showString
                    "D=M\n"
                | otherwise -> return $ showString "\
                    \@" . showString ix . showString "\n\
                    \D=A\n\
                    \@" . showString s . showString "\n\
                    \D=D+M\n\
                    \@R15\n\
                    \M=D\n\
                    \@SP\n\
                    \AM=M-1\n\
                    \D=M\n\
                    \@R15\n\
                    \A=M\n\
                    \M=D\n"
            _ -> error $ "unknown segment " ++ seg
    ["label"   ,lbl       ] -> state $ \s@(fn,_) -> (showChar '(' . fn . showChar '$' . showString lbl . showString ")\n",s)
    ["goto"    ,lbl       ] -> state $ \s@(fn,_) -> (showChar '@' . fn . showChar '$' . showString lbl . showString "\n0;JMP\n",s)
    ["if-goto" ,lbl       ] -> state $ \s@(fn,_) -> (showString "\
            \@SP\n\
            \AM=M-1\n\
            \D=M\n\
            \@" . fn . showChar '$' . showString lbl . showString "\n\
            \D;JNE\n",s)
    ["function",name,nLcls] -> let showName = showString name in state . const $ (
            showChar '(' . showName . showString ")\n\
            \@SP\n\
            \D=M\n\
            \@LCL\n\
            \AM=D\n" .
            (nTimes (read nLcls) $ showString "M=0\nA=A+1\n") . showString "\
            \D=A\n\
            \@SP\n\
            \M=D\n"
        ,(showName,0))
    ["call"    ,name,nArgs] -> state $ \(fn,nLbl) -> let lbl = fn . showString "$$" . shows nLbl in (
            showChar '@' . lbl . showString "\n\
            \D=A\n\
            \@SP\n\
            \A=M\n\
            \M=D\n\
            \@LCL\n\
            \D=M\n\
            \@SP\n\
            \AM=M+1\n\
            \M=D\n\
            \@ARG\n\
            \D=M\n\
            \@SP\n\
            \AM=M+1\n\
            \M=D\n\
            \@THIS\n\
            \D=M\n\
            \@SP\n\
            \AM=M+1\n\
            \M=D\n\
            \@THAT\n\
            \D=M\n\
            \@SP\n\
            \AM=M+1\n\
            \M=D\n\
            \@SP\n\
            \MD=M+1\n\
            \@" . shows (5 + read nArgs :: Word) . showString "\n\
            \D=D-A\n\
            \@ARG\n\
            \M=D\n\
            \@" . showString name . showString "\n\
            \0;JMP\n\
            \(" . lbl . showString ")\n"
        ,(fn,succ nLbl))
    ["return"             ] -> return $ showString "\
        \@SP\n\
        \A=M-1\n\
        \D=M\n\
        \@ARG\n\
        \A=M\n\
        \M=D\n\
        \@ARG\n\
        \D=M\n\
        \@SP\n\
        \M=D+1\n\
        \@LCL\n\
        \D=M\n\
        \@R15\n\
        \AM=D-1\n\
        \D=M\n\
        \@THAT\n\
        \M=D\n\
        \@R15\n\
        \AM=M-1\n\
        \D=M\n\
        \@THIS\n\
        \M=D\n\
        \@R15\n\
        \AM=M-1\n\
        \D=M\n\
        \@ARG\n\
        \M=D\n\
        \@R15\n\
        \AM=M-1\n\
        \D=M\n\
        \@LCL\n\
        \M=D\n\
        \@R15\n\
        \A=M-1\n\
        \A=M;JMP\n"
    _                       -> error $ "invalid command " ++ l
    where
        globalSegment s = case s of
            "pointer"  -> Just (3 :: Int)
            "temp"     -> Just 5
            "static"   -> Just 16
            _          -> Nothing
        derefSegment s = case s of
            "local"    -> Just "LCL"
            "argument" -> Just "ARG"
            "this"     -> Just "THIS"
            "that"     -> Just "THAT"
            _          -> Nothing

nTimes :: Int -> (a -> a) -> (a -> a)
nTimes n f = go n where
    go m
        | m > 0     = go (pred m) . f
        | otherwise = id
