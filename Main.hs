module Main (main) where

import qualified Data.ByteString.UTF8 as UTF8
import Data.Digest.Murmur3


main :: IO ()
main = do
  mapM_ (\string -> do
           putStrLn $ show $ asByteString $ hash $ UTF8.fromString string)
        ["Int8", "Int16", "Int32", "Int64", "Word8", "Word16", "Word32",
         "Word64", "Float", "Double", "UTF8", "Blob",
         "Structure(name\000,value\000)"]
