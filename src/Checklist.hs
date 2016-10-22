{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Checklist ( Checklist
                 , mkItem
                 , buildChecklist
                 , checkItem
                 , unCheckItem
                 , addItem ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import           Data.Monoid         (mconcat)
import qualified Data.Text           as T
import           Data.Traversable
import           Text.Read           (readMaybe)

data Checked = Done | NotDone
data Item = Item { text :: T.Text, checked :: Checked }
newtype Checklist = Checklist { unChecklist :: M.Map Int Item }

{--
The following are Instances declatations for Checked including:
  * Show
  * FromJSON
  * ToJSON
--}

-- Trivial, just adding the space in "Not Done"
instance Show Checked where
  show Done = "Done"
  show NotDone = "Not Done"

instance FromJSON Checked where
  parseJSON = withObject "checked" $ \o -> do
    c <- o .:? "checked" .!= False  -- if field isn't present assume it's false
    return $ if c then Done else NotDone  -- convert the Boolean to Checked

instance ToJSON Checked where
  toJSON c = toJSON $ toBool c  -- convert to Bool and then JSON
    where
      toBool Done = True
      toBool NotDone = False

{--
The following are Instances declatations for Item including:
  * Show
  * FromJSON
  * ToJSON
--}

instance Show Item where
  -- Convert the fields to string and concatenate
  show Item{..} = mconcat [T.unpack text, " - ", show checked]

instance FromJSON Item where
  parseJSON = withObject "item" $ \o -> do
    -- need to tell compiler we're inspecting an object when parsing Checked
    checked <- parseJSON (Object o)
    text <- o .: "text"
    return $ Item{..}

instance ToJSON Item where
  toJSON Item{..} =
    object ["text" .= text, "checked" .= (toJSON checked)]

instance Show Checklist where
  show (Checklist checklist) =
    M.foldrWithKey (\k a b -> ppItem k a ++ b) "" checklist
    where
      -- helper to pretty print an item in the checklist
      ppItem :: Int -> Item -> String
      ppItem n item =
        mconcat [show n, ": ", show item, "\n"]

{--
The following are Instances declatations for Checklist including:
  * Show
  * FromJSON
  * ToJSON
--}
instance FromJSON Checklist where
  parseJSON = withObject "checklist" $ \o ->
    -- Parse the list of items and turn it into a Map and then Checklist
    Checklist <$> M.fromList <$> parseItems (Object o)
    where
      parseItems:: Value -> Parser [(Int, Item)]
      parseItems =
        withObject "items" $ \o ->
          -- traverse of key, value pairs i.e. (1, Item), (2, Item), etc
          for (HM.toList o) $ \(itemNumber, itemJSON) -> do
            item <- parseJSON itemJSON  -- already defined how to parse this

            -- Need to check if the key is in fact a number
            i <- case readMaybe $ T.unpack itemNumber of
                    Just x -> return x
                    Nothing -> fail "Key was not a number"

            -- Return as a tuple
            return (i, item)

instance ToJSON Checklist where
  toJSON (Checklist checklist) =
    -- Turn keys into String and use Map instance of toJSON
    toJSON $ M.mapKeys show checklist

{--
The following are functions for building and manipulating checklists
--}

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
  Checklist . M.adjust check n . unChecklist
  where
    check (Item t _) = Item t Done

{--
 -- Select an item in the checklist and mark it as Not Done
--}
unCheckItem :: Int -> Checklist -> Checklist
unCheckItem n =
  Checklist . M.adjust unCheck n . unChecklist
  where
    unCheck (Item t _) = Item t NotDone

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
mkItem t = Item t NotDone
