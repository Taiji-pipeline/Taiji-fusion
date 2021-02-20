{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase #-}
module Taiji.Pipeline.Fusion where

import           Control.Workflow

import Taiji.Pipeline.Fusion.Functions

builder :: Builder ()
builder = do
    uNode "Get_Matched_Groups" [| \(a,b) -> return $ getMatchedExp a b |]
    ["SCATAC_Read_Input", "SCRNA_Read_Input"] ~> "Get_Matched_Groups"

    node "Confusion_Table" [| \(prefix,atac,rna) ->
        liftIO $ mkConfusionTable "1.tsv" prefix atac rna
        |] $ return ()
    ["Get_Matched_Groups", "SCATAC_Merged_Cluster", "SCRNA_Merged_Cluster"] ~> "Confusion_Table"
