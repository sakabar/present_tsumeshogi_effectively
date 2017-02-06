import qualified Data.List as L
import qualified Data.List.Split as S
import qualified System.Environment as E (getArgs)
import qualified System.Exit as Ex
import qualified System.IO as I
import qualified System.Posix.Time as T
import qualified System.Posix.Types as TT
import Control.Applicative
import Control.Monad

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
presentProblems :: [(Int, Int)] -> IO [(Int, Int, Int)]
presentProblems problems = do
  let probNums  = map fst problems
  let origTimes = map snd problems
  counted <- forM probNums presentProblem
  let ans = zipWith (\a b -> ((fst b), a, (snd b))) origTimes counted
  return ans

-- 引数で与えた問題を提示し、秒数をカウントする
presentProblem :: Int -> IO (Int, Int)
presentProblem probNum = do
  putStrLn $ (show probNum) ++ "問目を見てください。Ready?"
  inputStr <- getLine
  putStrLn "Go. [解けたらEnterを押してください]"
  nowInt <- fmap epochTime2Int T.epochTime
  inputStr <- getLine
  nowInt2 <- fmap (+ 1) $ fmap epochTime2Int T.epochTime --切り上げのようなもの。1秒多くカウントする
  let thisTime = if inputStr == "" then (nowInt2 - nowInt) else 120
  putStrLn $ (show thisTime) ++ "秒"
  return (probNum, thisTime)


-- 末尾再帰になっていない FIXME
updateTimeLines :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
updateTimeLines origTimeLines [] = origTimeLines
updateTimeLines origTimeLines (x:xs) = updateTimeLines upd xs
  where probNum  = fst x
        thisTime = snd x
        upd = (map (\(num, t) -> if num == probNum then (num, thisTime) else (num, t)) origTimeLines)

avg :: [Int] -> Double
avg [] = 0.0
avg lst = 1.0 * (fromIntegral $ sum $ lst) / (fromIntegral $ length lst)

main = do
  args <- E.getArgs
  when ((length args) /= 1) $ do
    I.hPutStrLn I.stderr "Argument Error:<file_name>"
    Ex.exitFailure

  let fileName = args !! 0 -- "time_data/5_moves_handbook_1.txt"
  -- let howManyProblems = 4
  timeLines <- readTimeLst fileName
  let all_avg_sec = avg $ timeLines
  putStrLn $ show all_avg_sec
  let enumTimeLines = zip [1..] timeLines
  let chunks = S.chunksOf 4 enumTimeLines

  -- 見開きで一番時間がかかっている問題4問をセットで出題
  -- ただし、時間の比較は和ではなく平均で比較する
  -- これは、全問題数が4の倍数でなかった時に、問題数の異なるセットと正当に比較できるようにするため
  let problems = head $ L.sortBy (\g1 g2 -> compare (avg $ map snd g2) (avg $ map snd g1)) $ chunks
  let len = length problems
  let sum_sec = sum $ map snd problems
  let avg_time = avg $ map snd problems
  putStrLn $ show avg_time
  -- let sortedTimeLines = L.sortBy (\tpl1 tpl2 -> compare (snd $ tpl2) (snd $ tpl1)) $ enumTimeLines -- 逆順ソート
  -- let problems = L.sortBy (\tpl1 tpl2 -> compare (fst $ tpl1) (fst $ tpl2)) $ take howManyProblems sortedTimeLines

  countedResult <- fmap (\lst -> map (\(a, b, c) -> (a,c)) lst) $ presentProblems problems
  let updatedTimeLines = map snd $ updateTimeLines enumTimeLines countedResult

  let new_sum = sum $ map snd countedResult
  let sec_diff = new_sum - sum_sec
  putStrLn $ (show sum_sec) ++ "秒から" ++ (show new_sum) ++ "秒に更新しました。(" ++ (show sec_diff) ++ "秒)"
  writeTimeLst fileName updatedTimeLines

  -- putStr $ unlines $ map show $ problems


