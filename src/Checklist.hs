{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Checklist where

import qualified Data.Map as M
import qualified Data.Text as T

type Checked = Bool
newtype Item = Item { unItem :: (T.Text, Checked) }
newtype Checklist = Checklist { unChecklist :: M.Map Int Item }

instance Show Checklist where
  show (Checklist checklist) = M.foldrWithKey (\k a b -> ppItem k a ++ b) "" checklist
    where
      -- helper to pretty print an item in the checklist
      ppItem :: Int -> Item -> String
      ppItem n (Item item) =
        show n ++ ": " ++ (T.unpack $ fst item) ++ " - " ++ (check $ snd item) ++ "\n"

      -- helper to convert boolean value to "Done" or "Not Done"
      check True = "Done"
      check False = "Not Done"

addItem :: Int -> Checklist -> Item -> Either String Checklist
addItem n (Checklist checklist) item =
  case M.lookup n checklist of
    Nothing -> Right $ Checklist $ M.insert n item checklist
    Just _ -> Left "Item already in this position"

checkItem :: Int -> Checklist -> Checklist
checkItem n =
  Checklist . M.adjust (Item . fmap (const True) . unItem) n . unChecklist

unCheckItem :: Int -> Checklist -> Checklist
unCheckItem n =
  Checklist . M.adjust (Item . fmap (const False) . unItem) n . unChecklist

buildChecklist :: [(Int, T.Text)] -> Checklist
buildChecklist = Checklist . M.fromList . map (fmap mkItem)

mkItem :: T.Text -> Item
mkItem = Item . flip (,) False -- assume the item is not done when making new one
