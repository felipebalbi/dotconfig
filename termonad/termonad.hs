{-# LANGUAGE OverloadedStrings #-}
module Main where

import Termonad
  ( CursorBlinkMode(CursorBlinkModeOn)
  , Option(Set)
  , ShowScrollbar(ShowScrollbarNever)
  , TMConfig
  , confirmExit
  , cursorBlinkMode
  , defaultConfigOptions
  , defaultTMConfig
  , options
  , start
  , FontConfig
  , FontSize(FontSizePoints)
  , defaultFontConfig
  , fontConfig
  , fontFamily
  , fontSize
  , showMenu
  , showScrollbar
  )
import Termonad.Config.Colour
  ( AlphaColour
  , ColourConfig
  , Palette(ExtendedPalette)
  , addColourExtension
  , createColour
  , createColourExtension
  , defaultColourConfig
  , backgroundColour
  , foregroundColour
  , palette
  )
import Termonad.Config.Vec (Vec((:*), EmptyVec), N8)

-- This is our main 'TMConfig'.  It holds all of the non-colour settings
-- for Termonad.
--
-- This shows how a few settings can be changed.
myTMConfig :: TMConfig
myTMConfig =
  defaultTMConfig
    { options =
        defaultConfigOptions
          { showScrollbar = ShowScrollbarNever
          , confirmExit = False
          , showMenu = False
          , cursorBlinkMode = CursorBlinkModeOn
          , fontConfig = fontConf
          }
    }

-- This is our Dracula dark 'ColourConfig'.  It holds all of our dark-related settings.
draculaDark :: ColourConfig (AlphaColour Double)
draculaDark =
  defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
    { foregroundColour = Set (createColour 248 248 242) -- base0
    , backgroundColour = Set (createColour  40  42  54) -- base03
    -- Set the extended palette that has 2 Vecs of 8 Dracula palette colours
    , palette = ExtendedPalette draculaDark1 draculaDark2
    }
  where
    draculaDark1 :: Vec N8 (AlphaColour Double)
    draculaDark1 =
         createColour  55  56  68 -- base02
      :* createColour 255 184 108 -- orange
      :* createColour  90 247 142 -- bright green
      :* createColour 244 249 157 -- bright yellow
      :* createColour 202 169 250 -- bright blue
      :* createColour 255 146 208 -- bright magenta
      :* createColour 154 237 254 -- bright cyan
      :* createColour 226 226 220 -- base2
      :* EmptyVec

    draculaDark2 :: Vec N8 (AlphaColour Double)
    draculaDark2 =
         createColour  70  71  82 -- base03
      :* createColour 255  55  55 -- red
      :* createColour  80 250 123 -- green
      :* createColour 241 250 140 -- yellow
      :* createColour  98 114 164 -- blue
      :* createColour 255 121 198 -- magenta
      :* createColour 139 233 253 -- cyan
      :* createColour 226 226 220 -- base2
      :* EmptyVec

-- This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "Fira Code"
    , fontSize = FontSizePoints 12
    }

main :: IO ()
main = do
  -- First, create the colour extension based on either Dracula modules.
  myColourExt <- createColourExtension draculaDark

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
