{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Taiji.Pipeline.Fusion.Functions where

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Binary (decodeFile)

import Taiji.Prelude
import Taiji.Utils
import Taiji.Pipeline.SC.ATACSeq.Types (SCATACSeq)
import Taiji.Pipeline.SC.RNASeq.Types (SCRNASeq)
import qualified Data.IntMap.Strict            as IM
import qualified Taiji.Utils.DataFrame as DF

getMatchedExp :: (Experiment e1, Experiment e2)
              => [e1 N f1] -> [e2 N f2]
              -> [(B.ByteString, B.ByteString)]
getMatchedExp exp1 exp2 = getIds exp1 <> getIds exp2
  where
    matchedGroups = S.fromList (map (^.groupName) exp1) `S.intersection` S.fromList (map (^.groupName) exp2)
    getIds :: Experiment e => [e N f] -> [(B.ByteString, B.ByteString)]
    getIds = concatMap f . filter (\x -> (x^.groupName) `S.member` matchedGroups)
    f x = flip map (IM.keys $ x^.replicates) $ \i ->
        ( B.pack $ printf "%s_%d" (T.unpack $ x^.eid) i
        , B.pack $ printf "%s_%d" (T.unpack $ fromJust $ x^.groupName) i)

mkConfusionTable :: FilePath
                 -> [(B.ByteString, B.ByteString)]
                 -> Maybe (SCATACSeq S (File '[] 'Other))
                 -> Maybe (SCRNASeq S (File '[] 'Other))
                 -> IO ()
mkConfusionTable output prefix' (Just atac) (Just rna) = do
    atacCl <- decodeFile $ atac^.replicates._2.files.location
    rnaCl <- decodeFile $ rna^.replicates._2.files.location
    B.writeFile "1.txt" $ B.unlines $ concat $ getBarcodes atacCl
    B.writeFile "2.txt" $ B.unlines $ concat $ getBarcodes rnaCl
    let df = DF.mkDataFrame (map (T.pack . B.unpack . _cluster_name) atacCl)
            (map (T.pack . B.unpack . _cluster_name) rnaCl) $ confusionTable
            (getBarcodes atacCl) (getBarcodes rnaCl)
    DF.writeTable output (T.pack . show) df
  where
    prefix = M.fromList prefix'
    getBarcodes = map (mapMaybe (f . _cell_barcode) . _cluster_member)
      where
        f x = let (a,b) = B.breakEnd (=='+') x
              in case M.lookup (B.init a) prefix of
                  Nothing -> Nothing
                  Just a' -> Just $ a' <> "+" <> b
mkConfusionTable _ _ _ _ = return ()