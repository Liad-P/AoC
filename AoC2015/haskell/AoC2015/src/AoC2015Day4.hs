{-# LANGUAGE ImportQualifiedPost #-}

module AoC2015Day4 where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 qualified as BS (pack, unpack)
import Data.List (isPrefixOf)

day4part1 :: Int
day4part1 = length $ takeWhile (\md5 -> not $ "00000" `isPrefixOf` md5) inputList
  where
    inputList = map (\x -> BS.unpack $ encode $ hash $ BS.pack $ "ckczppom" ++ show x) [0 ..]

day4part2 :: Int
day4part2 = length $ takeWhile (\md5 -> not $ "000000" `isPrefixOf` md5) inputList
  where
    inputList = map (\x -> BS.unpack $ encode $ hash $ BS.pack $ "ckczppom" ++ show x) [0 ..]