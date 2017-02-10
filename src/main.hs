import qualified Data.List as L
import qualified Data.List.Split as S
import qualified System.Environment as E (getArgs)
import qualified System.Exit as Ex
import qualified System.IO as I
import qualified System.Posix.Time as T
import qualified System.Posix.Types as TT
import Control.Applicative
import Control.Monad

-- 問題番号, 解答にかかった秒数
-- 後で、解答したUNIX-TIMEも引数に加える予定
newtype Problem = Problem (Int, Int)

getAnsSec :: Problem -> Int
getAnsSec (Problem (_, ansSec)) = ansSec

getProbNum :: Problem -> Int
getProbNum (Problem (n, _)) = n

readTimeLst :: FilePath -> IO [Int]
readTimeLst path = do
  str <- readFile path
  let timeStrs = lines str
  let timeLines = map (\x -> read x :: Int) timeStrs
  return timeLines


writeTimeLst :: FilePath -> [Int] -> IO ()
writeTimeLst path intLst = do
  let ansStr = unlines $ map show intLst
  writeFile path ansStr
  return ()

epochTime2Int :: TT.EpochTime -> Int
epochTime2Int eT = read $ show $ eT


-- forM :: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
-- (問題番号と元々かかった時間)のタプルのリストを引数として、(問題番号と元々かかった時間と今回かかった秒数)のタプルのリストを返す
presentProblems :: [Problem] -> IO [(Int, Int, Int)]
presentProblems problems = do
  let probNums  = map getProbNum problems
  let origTimes = map getAnsSec problems
  counted <- forM probNums presentProblem
  let ans = zipWith (\a p -> ((getProbNum p), a, (getAnsSec p))) origTimes counted
  return ans

-- 引数で与えた問題を提示し、秒数をカウントする
presentProblem :: Int -> IO Problem
presentProblem probNum = do
  putStrLn $ (show probNum) ++ "問目を見てください。Ready?"
  inputStr <- getLine
  putStrLn "Go. [解けたらEnterを押してください]"
  nowInt <- epochTime2Int <$> T.epochTime
  inputStr <- getLine
  nowInt2 <- (+ 1) <$> epochTime2Int <$> T.epochTime --切り上げのようなもの。1秒多くカウントする
  let thisTime = if inputStr == "" then (nowInt2 - nowInt) else 120
  putStrLn $ (show thisTime) ++ "秒"
  return $ Problem (probNum, thisTime)


updateTimeLines :: [Problem] -> [Problem] -> [Problem]
updateTimeLines origTimeLines [] = origTimeLines
updateTimeLines origTimeLines (x:xs) = updateTimeLines upd xs
  where probNum  = getProbNum x
        thisTime = getAnsSec  x
        upd = (map (\(Problem (num, t)) -> if num == probNum then (Problem (num, thisTime)) else (Problem (num, t))) origTimeLines)

avg :: [Int] -> Double
avg [] = 0.0
avg lst = 1.0 * (fromIntegral $ sum $ lst) / (fromIntegral $ length lst)

getEnumTimeLines :: [Int] -> [Problem]
getEnumTimeLines ansSecs = zipWith (\a b -> Problem (a,b)) [1..] ansSecs

main2 :: [Int] -> Int -> Int -> IO [Problem]
main2 timeLines chunk_unit_num solve_num = do
  let all_avg_sec = avg $ timeLines
  let enumTimeLines = getEnumTimeLines timeLines
  let chunks = S.chunksOf chunk_unit_num enumTimeLines
  let problems = L.sortBy (\p1 p2 -> compare (getProbNum p1) (getProbNum p2)) $ concat $ take solve_num $ L.sortBy (\g1 g2 -> compare (avg $ map getAnsSec g2) (avg $ map getAnsSec g1)) $ chunks
  let sum_sec = sum $ map getAnsSec problems
  let avg_time = avg $ map getAnsSec problems

  putStrLn $ show all_avg_sec
  putStrLn $ show avg_time

  countedResult <- map (\(a, b, c) -> Problem (a,c)) <$> presentProblems problems
  let new_sum = sum $ map getAnsSec countedResult
  let new_avg = avg $ map getAnsSec countedResult
  let sec_diff = new_sum - sum_sec
  putStrLn $ "平均" ++ (show avg_time) ++ "秒から平均" ++ (show new_avg) ++ "秒に更新しました。(計" ++ (show sec_diff) ++ "秒)"

  let updatedTimeLines = updateTimeLines enumTimeLines countedResult
  return updatedTimeLines

main = do
  args <- E.getArgs
  when ((length args) /= 3) $ do
    I.hPutStrLn I.stderr "Argument Error:<file_name> <chunk_unit_num> <solve_num>"
    Ex.exitFailure

  let fileName = args !! 0 -- "time_data/5_moves_handbook_1.txt"
  --何問を1セットにするか (もし4なら見開き1ページの問題を一度に解く)
  -- ただし、時間の比較は和ではなく平均で比較する
  -- これは、全問題数が4の倍数でなかった時に、問題数の異なるセットと正当に比較できるようにするため
  let chunk_unit_num = read (args !! 1) :: Int
  let solve_num = read (args !! 2) :: Int -- 一回に解く問題数

  timeLines <- readTimeLst fileName
  updatedTimeLines <- map getAnsSec <$> main2 timeLines chunk_unit_num solve_num
  writeTimeLst fileName updatedTimeLines
