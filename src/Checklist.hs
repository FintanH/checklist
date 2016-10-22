{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Checklist ( Checklist
                 , mkItem
                 , buildChecklist
                 , checkItem
                 , unCheckItem
                 , addItem ) where

import           Control.Applicative
import qualified Data.Map            as M
import           Data.Monoid         (mconcat)
import qualified Data.Text           as T

data Checked = Done | NotDone
data Item = Item T.Text Checked
newtype Checklist = Checklist { unChecklist :: M.Map Int Item }

instance Show Checked where
  show Done = "Done"
  show NotDone = "Not Done"

instance Show Item where
  show (Item txt checked) = T.unpack txt, " - ", show checked

instance Show Checklist where
  show (Checklist checklist) =
    M.foldrWithKey (\k a b -> ppItem k a ++ b) "" checklist
    where
      -- helper to pretty print an item in the checklist
      ppItem :: Int -> Item -> String
      ppItem n item =
        mconcat [show n, ": ", show item, "\n"]

{--
 -- Add an item to the checklist at a certain position
 -- If there is already an item exisiting in this position return an error
--}
addItem :: Int -> Checklist -> Item -> Either String Checklist
addItem n (Checklist checklist) item =
  case M.lookup n checklist of
    Nothing -> Right $ Checklist $ M.insert n item checklist
    Just _ -> Left "Item already in this position"

{--
 -- Select an item in the checklist and mark it as Done
--}
checkItem :: Int -> Checklist -> Checklist
checkItem n =
  Checklist . M.adjust (Item . fmap (const Done) . unItem) n . unChecklist

{--
 -- Select an item in the checklist and mark it as Not Done
--}
unCheckItem :: Int -> Checklist -> Checklist
unCheckItem n =
  Checklist . M.adjust (Item . fmap (const NotDone) . unItem) n . unChecklist

{--
 -- Take a list of items and turn them into a checklist
--}
buildChecklist :: [(Int, T.Text)] -> Checklist
buildChecklist = Checklist . M.fromList . map (fmap mkItem)

{--
 -- Make an Item from a Text value
 -- Assume the item is not done when making new one
--}
mkItem :: T.Text -> Item
mkItem = Item . flip (,) NotDone
