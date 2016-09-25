{-# LANGUAGE OverloadedStrings #-}
module Main where

import Checklist


main :: IO ()
main = do
  let checklist = buildChecklist [(1, "Do first thing"), (2, "Do second thing")]
  print checklist
  let checklist' = checkItem 1 checklist
  print checklist'
  let checklist'' = checkItem 2 checklist'
  print checklist''
