{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Data.List.Split
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Csv

parseFile :: FilePath -> IO [(String, Double, [String])]
parseFile fp = do
  csvData <- BL.readFile fp
  case decode False csvData of
    Left err -> fail $ "Parse error: " ++ show err
    Right v -> return $ map convert (V.toList v)
      where
        convert (name, time :: Double, deps) =
          (name, time, split (dropBlanks $ oneOf ",") deps)

