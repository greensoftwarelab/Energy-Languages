-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- Contributed by Branimir Maksimovic

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment
import Data.IORef
import Data.List
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

lineLen = 60
blockFactor = 10
block = lineLen * blockFactor

main = do
    n <- getArgs >>= readIO.head
    rnd <- newIORef 42
    repeatFasta ">ONE Homo sapiens alu" (2*n) alu
    dta <- startThreads
    randomFasta ">TWO IUB ambiguity codes" (3*n) iub rnd dta
    randomFasta ">THREE Homo sapiens frequency" (5*n) homosapiens rnd dta
    where startThreads = do
            out <- newEmptyMVar
            workChan <- newChan
            mapM_ (\_-> forkIO $ do
                  let loop = do
                       (i,work,line) <- takeMVar out
                       let !ln = line work
                       writeChan workChan (i,ln)
                       loop
                  loop) [0..2]
            return (out,workChan)

repeatFasta :: String -> Int -> BL.ByteString -> IO ()
repeatFasta s n nuc = do
    putStrLn s
    breakString $ BL.toStrict $ BL.take (fromIntegral n) $ BL.cycle nuc

randomFasta :: String -> Int -> [(Float,Char)]-> IORef Int -> 
               (MVar (Int,[Float],[Float]->B.ByteString),Chan (Int,B.ByteString))-> IO ()
randomFasta s n nuc rnd (out,workChan) = do
    putStrLn s
    let tbl = scanl1 (\(a,_) (b,c) ->(a+b,c)) nuc
        genRndLst n = replicateM n (genRandom 1.0 rnd)
        find ((a,b):xs) p
            | p < a = b
            | otherwise = find xs p
        unf [] = Nothing
        unf (x:xs) = Just (find tbl x,xs)
        line = B.unfoldr unf
    (sum,i) <- foldM (\(x,i) _->do
                         lst <- genRndLst block
                         putMVar out (i,lst,line)
                         return (x+block,i+1) ) (0,0) [block,2*block..n-1]
    mp <- foldM (\m _-> do
                    dta <- readChan workChan
                    return (dta:m) ) [] [0..i-1]
    mapM_ (\(_,line)-> breakString line) $ sort mp
    lst <- genRndLst (n-sum)
    breakString $ line lst
    
alu = BL.pack "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGG\
    \TCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGG\
    \CGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGC\
    \GGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iub = [(0.27,'a'),(0.12,'c'),(0.12,'g'),(0.27,'t'),(0.02,'B')
      ,(0.02,'D'),(0.02,'H'),(0.02,'K'),(0.02,'M'),(0.02,'N')
      ,(0.02,'R'),(0.02,'S'),(0.02,'V'),(0.02,'W'),(0.02,'Y')]

homosapiens = [(0.3029549426680,'a'),(0.1979883004921,'c')
              ,(0.1975473066391,'g'),(0.3015094502008,'t')]

genRandom :: Float -> IORef Int -> IO Float
genRandom max rnd = do
    let im = 139968
        ia = 3877
        ic = 29573
        form x = ((x*ia+ic)`rem`im)
    modifyIORef' rnd form
    last <- readIORef rnd
    return (max * (fromIntegral last) / fromIntegral im)

breakString s = break' $ B.splitAt (fromIntegral lineLen) s
     where break' (l,r)
            | B.null l && B.null r  = return ()
            | B.null r = B.putStrLn l
            | otherwise = do
                           B.putStrLn l
                           break' $ B.splitAt (fromIntegral lineLen) r