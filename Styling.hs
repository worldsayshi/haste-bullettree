{-# LANGUAGE  CPP, OverloadedStrings #-}
module Styling (styleToFile) where

#ifndef __HASTE__
import Prelude hiding (div,writeFile)
import Clay
import Clay.Stylesheet (key)
import Data.Text.Lazy (toStrict)
import Data.Text.IO (writeFile)
import Data.Text

styleToFile = writeFile "style.css" (toStrict styling)

styling = render $ do
  star ? do
    fontSize $ px 16
    fontFamily ["Helvetica"] [sansSerif]
  h1 # ".header" ? do
    let headerHeight = px 100
    height headerHeight
    whiteSpace nowrap
    lineHeight headerHeight
    paddingLeft $ px 50
    -- key "text-align" ("center" :: Text)
  div # ".container" ? do
    width $ px 1000
    key "margin" ("0 auto" :: Text)
  div # "#editorRoot" ? do
    backgroundColor (grayish 245) 
  div # ".tree-container" ? do
    marginLeft $ px 20
  Clay.span # ".bullet" ? do
    display tableCell
{-  Clay.span # ".input-wrapper" # ":before" ? do
    content $ stringContent "›"
    paddingRight $ px 10-}
  Clay.span # ".input-wrapper" ? do
    paddingLeft $ px 10
    display tableCell
    width $ pct 100
    overflow hidden
  Clay.span # ".input-wrapper" # ":hover" ? do
      backgroundColor (grayish 255) 
  input ? do
    backgroundColor transparent
    width $ pct 100
    lineHeight $ px 25
    key "border" ("none" :: Text)
  
#endif
#ifdef __HASTE__
styleToFile = undefined
#endif
