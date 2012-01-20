module Data.Digest.Murmur3
  (Hash,
   asByteString,
   hash)
  where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word


data Hash = Hash Word64 Word64


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
hash input =
  let c1 = 0x87c37b91114253d5
      c2 = 0x4cf5ad432745937f
      seed = 0
      loop :: (Word64, Word64, Word64, Word64, Int)
           -> Word8
           -> (Word64, Word64, Word64, Word64, Int)
      loop (h1, h2, k1, k2, fillCount) byte =
        let shiftAmount = mod (8 * fillCount) 64
        in case fillCount of
             _ | fillCount < 8 ->
                  (h1, h2,
                   k1 .|. (shiftL (fromIntegral byte) shiftAmount), k2,
                   fillCount + 1)
               | fillCount == 15 ->
                  let k1'0 = k1
                      k2'0 = k2 .|. (shiftL (fromIntegral byte) shiftAmount)
                      (h1'1, k1'1) = stepA1 h1 k1'0
                      h1'2 = stepB1 h1'2 h2
                      (h2'1, k2'1) = stepA2 h2 k1'0
                      h2'2 = stepB2 h2'1 h1'2
                  in (h1'2, h2'2, 0, 0, 0)
               | otherwise ->
                  (h1, h2,
                   k1, k2 .|. (shiftL (fromIntegral byte) shiftAmount),
                   fillCount + 1)
      stepA1 h1 k1 =
        let k1'1 = k1 * c1
            k1'2 = rotateL k1'1 31
            k1'3 = k1'2 * c2
            h1'1 = xor h1 k1'3
        in (h1'1, k1'3)
      stepB1 h1 h2 =
        let h1'1 = rotateL h1 27
            h1'2 = h1'1 + h2
            h1'3 = h1'2 * 5 + 0x52dce729
        in h1'3
      stepA2 h2 k2 =
        let k2'1 = k2'0 * c2
            k2'2 = rotateL k2'1 33
            k2'3 = k2'2 * c1
            h2'0 = h2
            h2'1 = xor h2'0 k2'3
        in (h2'1, k2'3)
      stepB2 h2 h1 =
        let h2'1 = rotateL h2 31
            h2'2 = h2'1 + h1
            h2'3 = h2'2 * 5 + 0x38495ab5
        in h2'3
      finish h1 h2 =
        let h1'1 = xor h1 $ fromIntegral $ BS.length input
            h2'1 = xor h2 $ fromIntegral $ BS.length input
            h1'2 = h1'1 + h2'1
            h2'2 = h2'1 + h1'2
            h1'3 = fmix h1'2
            h2'3 = fmix h2'2
            h1'4 = h1'3 + h2'3
            h2'4 = h2'3 + h1'4
        in (h1'4, h2'4)
      fmix h =
        let h'1 = xor h (shiftR h 16)
            h'2 = h'1 * 0x85ebca6b
            h'3 = xor h'2 (shiftR h'2 13)
            h'4 = h'3 * 0xc2b2ae35
            h'5 = xor h'3 (shiftR h'4 16)
        in h'5
      (h1'0, h2'0, k1'0, k2'0, fillCount) =
        BS.foldl loop (seed, seed, 0, 0, 0) input
      (h1'1, k1'1) =
        if fillCount > 8
          then stepA1 h1'0 k1'0
          else (h1'0, k1'0)
      (h2'1, k2'1) =
        if fillCount > 0
          then stepA2 h2'0 k2'0
          else (h2'0, k2'0)
      (h1'2, h2'2) = finish h1'1 h2'1
      result = Hash h1'2 h2'2
 in result
