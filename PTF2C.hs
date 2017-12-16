module Main (main) where

import System.FilePath
import System.Environment
import Control.Monad
import Data.List
import Data.List.Split
import Data.Either
import Numeric

type Byte = Int

main :: IO ()
main = do
    [fn] <- getArgs
    let progname = takeBaseName fn
    ss <- loadPaperTape fn
    writeFile (progname <.> "h") $ unlines $ formatHeader progname ss
    writeFile (progname <.> "c") $ unlines $ formatOutput progname ss

loadPaperTape :: FilePath -> IO [(Int, [Byte])]
loadPaperTape fn = do
    ([n], ss) <- fmap (partitionEithers . map (uncurry processLine) . zip [1..] . lines) $ readFile fn
    unless (length ss == n) $ error $ "record count mismatch: " ++ show (length ss) ++ "/=" ++ show n
    return ss
 
processLine :: Int -> String -> Either Int (Int, [Byte])
processLine i s
    | (';':'0':'0':c3:c2:c1:c0:xs) <- s = Left $ hex2int [c3,c2,c1,c0] -- ignoring checksum on count (hex2int xs)
    | (';':lh:ll:a3:a2:a1:a0:xs) <- s = Right $ let cksum = hex2int [lh,ll] + hex2int [a3,a2] + hex2int [a1,a0]
                                                 in processRecord i (hex2int [lh,ll]) (hex2int [a3,a2,a1,a0]) cksum xs
    | otherwise = error $ "bad input line: '" ++ s ++ "'"

processRecord :: Int -> Int -> Int -> Int -> String -> (Int, [Byte])
processRecord i l a cksum xs = (a, processBytes i (hex2int (drop (l*2) xs) - cksum) (take (l*2) xs))

processBytes :: Int -> Int -> String -> [Byte]
processBytes i cksum xs
    | cksum' == cksum = bs
    | otherwise = error $ "checksum error: " ++ show cksum' ++ "/=" ++ show cksum ++ " on line " ++ show i
    where bs = map hex2int $ chunksOf 2 xs
          cksum' = sum bs

formatHeader :: String -> [(Int, [Byte])] -> [String]
formatHeader progname xs@((start,_):_) =
    [ "#pragma once"
    , ""
    , "struct record { unsigned addr; unsigned size; unsigned char data[24]; };"
    , ""
    , "const unsigned " ++ progname ++ "_num_records = " ++ int2hex (length xs) ++ ";"
    , ""
    , "extern const struct record " ++ progname ++ "_records[];"
    , ""
    ] 
    where size = sum $ map (length . snd) xs

formatOutput :: String -> [(Int, [Byte])] -> [String]
formatOutput progname xs@((start,_):_) =
    [ "#include \"" ++ progname ++ ".h\""
    , ""
    , "const struct record " ++ progname ++ "_records[] = "
    ]  ++
    [ "    " ++ (if i > 0 then ", " else "{ ") ++ record a bs
    | (i, (a, bs)) <- zip [0..] xs
    ] ++
    [ "    };" ]

record :: Int -> [Byte] -> String
record a bs = "{ " ++ int2hex a ++ ", " ++ int2hex (length bs) ++ ", { " ++ intercalate ", " (map int2hex bs) ++ " } }"

hex2int :: String -> Int
hex2int = read . ("0x"++)

int2hex :: Int -> String
int2hex = ("0x"++) . (`showHex` "")

