module Main where

import System.Console.GetOpt
import System.Environment

{-
simulate FILENAME.CSV [ -b 2.0,3.0    -- beta distribution
                      | -n            -- normal distribution
                      | -e            -- "expected value" ]
                      [ -i NUM        -- iterations (default: 1000) ]

What should I keep? One column per node, early finish-late finish, plus last finish overall?
Should I output the critical path?


-- All we need to keep are the last finish values
-- We could also just display percent finished by deadlines (40, 50, etc.)

-}

data Method = ExpectedValue
            | NormalDistribution
            | BetaDistribution { alpha :: Double, beta :: Double }
            deriving (Show, Eq)

data Options = Opts
               { optIterations :: Int
               , optMethod     :: Method
               } deriving (Show, Eq)

defaultOptions = Opts { optIterations = 1000, optMethod = NormalDistribution }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "n" ["normal"] (NoArg (\o -> o { optMethod = NormalDistribution })) "use normal distribution method"
  , Option "e" ["expected"] (NoArg (\o -> o { optMethod = ExpectedValue, optIterations = 1 })) "use expected value calculation (sets iteration count to one)"
  , Option "i" ["iterations"] (ReqArg (\c opts -> opts { optIterations = read c }) "ITERATIONS") "number of iterations (default: 1000)" 
  ]
  -- Option ['b'] ["beta"] (NoArg (\o -> o { optMethod = BetaDistribution }))

getOptions :: IO (Options, [String])
getOptions = do
  argv <- getArgs
  case getOpt Permute options argv of
    (o, n, [])   -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: sim [OPTION...] files..."

process :: Options -> FilePath -> IO ()
process opts fp = putStrLn $ "processing " ++ fp ++ "..."

main = do
  (opt, args) <- getOptions
  mapM_ (process opt) args


