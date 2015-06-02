
import System.Environment (getArgs)
import Data.List
import Data.List.Split (splitOn)
import Debug.Trace

------------------------------------------------------------------------------

data Path = Path
    { pName :: String
    , pWhole :: String
    , pFrequency :: Int
    } deriving (Show,Eq)

data ScoredPath = ScoredPath
    { spPath :: Path
    , spScore :: Int
    } deriving (Show,Eq)

instance Ord Path where
    (Path _ _ f1) `compare` (Path _ _ f2) = f2 `compare` f1

instance Ord ScoredPath where
    (ScoredPath p1 s1) `compare` (ScoredPath p2 s2) = (s2, p2) `compare` (s1, p2)

------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    content <- readFile "/Users/rene/.cd_history"
    let query = args !! 0
    print $ scoreAllPaths query $ loadAllPaths $ lines content

loadAllPaths :: [String] -> [Path]
loadAllPaths lines = map loadPath $ frequency lines

frequency :: Eq a => [a] -> [(a, Int)]
frequency l = map (\x -> (head x, length x)) $ group l

loadPath :: (String, Int) -> Path
loadPath (line, freq) =
    Path { pName = n
         , pWhole = p
         , pFrequency = freq
         }
  where
    n = head $ reverse $ splitOn "/" line
    p = line

scoreAllPaths :: String -> [Path] -> [ScoredPath]
scoreAllPaths query paths = sort $ map (scorePath query) paths

scorePath :: String -> Path -> ScoredPath
scorePath query path = ScoredPath path $ getPathScore query $ pName path

getPathScore :: String -> String -> Int
getPathScore (q:query) (p:path)
    | q == p    = getPathScore query path
    | otherwise = getPathScore (q:query) path
getPathScore [] _ = 10
getPathScore _ [] = 0

