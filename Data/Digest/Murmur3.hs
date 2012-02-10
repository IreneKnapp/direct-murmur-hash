module Data.Digest.Murmur3
  (Hash,
   asByteString,
   hash)
  where

import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word


data Hash = Hash Word64 Word64
          deriving (Eq, Ord)


newtype Identity a = MakeIdentity { identityAction :: a }
instance Monad Identity where
  return a = MakeIdentity a
  (>>=) x f = f $ identityAction x


asByteString :: Hash -> ByteString
asByteString (Hash h1 h2) =
  BS.pack [fromIntegral $ shiftR h1 0 .&. 0xFF,
           fromIntegral $ shiftR h1 8 .&. 0xFF,
           fromIntegral $ shiftR h1 16 .&. 0xFF,
           fromIntegral $ shiftR h1 24 .&. 0xFF,
           fromIntegral $ shiftR h1 32 .&. 0xFF,
           fromIntegral $ shiftR h1 40 .&. 0xFF,
           fromIntegral $ shiftR h1 48 .&. 0xFF,
           fromIntegral $ shiftR h1 56 .&. 0xFF,
           fromIntegral $ shiftR h2 0 .&. 0xFF,
           fromIntegral $ shiftR h2 8 .&. 0xFF,
           fromIntegral $ shiftR h2 16 .&. 0xFF,
           fromIntegral $ shiftR h2 24 .&. 0xFF,
           fromIntegral $ shiftR h2 32 .&. 0xFF,
           fromIntegral $ shiftR h2 40 .&. 0xFF,
           fromIntegral $ shiftR h2 48 .&. 0xFF,
           fromIntegral $ shiftR h2 56 .&. 0xFF]


hash :: ByteString -> Hash
hash input = identityAction $ do
  let c1 = 0x87c37b91114253d5
  let c2 = 0x4cf5ad432745937f
      seed = 0
      totalLength = fromIntegral $ BS.length input
  let step :: Word64 -> Word64
           -> Word64 -> Word64
           -> Identity (Word64, Word64)
      step h1 h2 k1 k2 = do
        -- First line
        k1 <- return $ k1 * c1
        k1 <- return $ rotateL k1 31
        k1 <- return $ k1 * c2
        h1 <- return $ xor h1 k1
        -- Second line
        h1 <- return $ rotateL h1 27
        h1 <- return $ h1 + h2
        h1 <- return $ h1 * 5 + 0x52dce729
        -- Third line
        k2 <- return $ k2 * c2
        k2 <- return $ rotateL k2 33
        k2 <- return $ k2 * c1
        h2 <- return $ xor h2 k2
        -- Fourth line
        h2 <- return $ rotateL h2 31
        h2 <- return $ h2 + h1
        h2 <- return $ h2 * 5 + 0x38495ab5
        return (h1, h2)
      finish :: Word64 -> Word64
             -> Word64 -> Word64
             -> Identity Hash
      finish h1 h2 k1 k2 = do
        -- First line
        k1 <- return $ k1 * c1
        k1 <- return $ rotateL k1 31
        k1 <- return $ k1 * c2
        h1 <- return $ xor h1 k1
        -- Third line
        k2 <- return $ k2 * c2
        k2 <- return $ rotateL k2 33
        k2 <- return $ k2 * c1
        h2 <- return $ xor h2 k2
        -- Finalization
        h1 <- return $ xor h1 totalLength
        h2 <- return $ xor h2 totalLength
        h1 <- mix h1
        h2 <- mix h2
        h1 <- return $ h1 + h2
        h2 <- return $ h2 + h1
        return $ Hash h1 h2
      mix :: Word64 -> Identity Word64
      mix k = do
        k <- return $ xor k (shiftR k 33)
        k <- return $ k * 0xff51afd7ed558ccd
        k <- return $ xor k (shiftR k 33)
        k <- return $ k * 0xc4ceb9fe1a85ec53
        k <- return $ xor k (shiftR k 33)
        return k
      loop :: Word64 -> Word64 -> ByteString -> Identity Hash
      loop h1 h2 input = do
        (k1, input) <- takeWord64 input
        (k2, input) <- takeWord64 input
        if BS.null input
          then finish h1 h2 k1 k2
          else do
            (h1, h2) <- step h1 h2 k1 k2
            loop h1 h2 input
      takeWord64 :: ByteString -> Identity (Word64, ByteString)
      takeWord64 input = do
        let (front, rest) = BS.splitAt 8 input
        word <- foldM (\sum (byte, offset) -> do
                         return $ sum + (shiftL (fromIntegral byte) offset))
                      0
                      (zip (BS.unpack front) [0, 8 .. 56])
        return (word, rest)
  loop seed seed input
