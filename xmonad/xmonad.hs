------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

-- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

-- Data
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, docks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

------------------------------------------------------------------------
-- Variables
------------------------------------------------------------------------

myTerminal :: String
myTerminal      = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth   = 3

myModMask :: KeyMask
myModMask       = mod4Mask

myWorkspaces :: [[Char]]
myWorkspaces    = ["1:web","2:mail","3:code","4:chat","5","6","7","8","9"]

myNormalBorderColor :: String
myNormalBorderColor  = "#4d4d4d"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff5555"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' .
              W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- Autostart
------------------------------------------------------------------------
myStartupHook :: X()
myStartupHook = do
  spawnOnce "xscreensaver -no-splash &"
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom -b &"
  spawnOnce "volumeicon &"

------------------------------------------------------------------------
-- Xprompt
------------------------------------------------------------------------
xpKeymap :: M.Map (KeyMask,KeySym) (XP ())
xpKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line fowards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) mod1Mask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]


xpConfig :: XPConfig
xpConfig = def
      { font                = "xft:Fira Code:size=12:style=bold"
      , bgColor             = "#292d3e"
      , fgColor             = "#d0d0d0"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
      , promptKeymap        = xpKeymap
      , position            = Top
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = isPrefixOf
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }

-- The same config minus the autocomplete feature which is annoying on
-- certain Xprompts, like the search engine prompts.
xpConfig' :: XPConfig
xpConfig' = xpConfig
      { autoComplete = Nothing
      }

-----------------------------------------------------------------------
-- Key Bindings
-----------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
  [
    -- XMonad
    ("M-C-r", spawn "xmonad --recompile && xmonad --restart")
  , ("M-C-q", io exitSuccess)

    -- Spawn terminal
  , ("M-<Return>", spawn (myTerminal))

    -- DMenu
  , ("M-d", spawn "dmenu_run -fn 'Fira Code:size=12:style=bold'")

    -- Windows
  , ("M-S-c", kill1)
  , ("M-S-a", killAll)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-S-<Space>", sendMessage ToggleStruts)

    -- Screen Lock
  , ("M-C-l", spawn "xscreensaver-command -lock")

    -- Suspend
  , ("M-C-h", spawn "xscreensaver-command -lock && systemctl suspend")

    -- Navigation
  , ("M-f",   windows W.focusUp)
  , ("M-b",   windows W.focusDown)
  , ("M-m",   windows W.focusMaster)
  , ("M-C-f", windows W.swapUp)
  , ("M-C-b", windows W.swapDown)
  , ("M-<Backspace>", promote)
  , ("M-C-<Tab>", rotSlavesDown)

    -- Push window back into tiling
  , ("M-t", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
  , ("M-,", sendMessage (IncMasterN 1))

    -- Decrement the number of windows in the master area
  , ("M-.", sendMessage (IncMasterN (-1)))

    -- Shrink the master area
  , ("M-h", sendMessage Shrink)

    -- Expand the master area
  , ("M-l", sendMessage Expand)

    -- Emacs
  , ("M-e", spawn "emacsclient -c -a ''")

    -- Scrot
  , ("M-C-s", spawn "sleep 0.2 && scrot -s /tmp/%Y-%m-%d_%H-%M-%S_%w-%h.png")

    -- Multimedia Keys
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
  ]
  -- Append search engines
  ++ [("M-s " ++ k, S.promptSearch xpConfig' f) | (k,f) <- searchList ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Search Engines
------------------------------------------------------------------------
archwiki :: S.SearchEngine
archwiki = S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/?search="

ebay :: S.SearchEngine
ebay = S.searchEngine "eBay" "https://www.ebay.com/sch/i.html?_nkw="

linuxUsb :: S.SearchEngine
linuxUsb = S.searchEngine "Linux-USB" "https://lore.kernel.org/linux-usb/?q="

lkml :: S.SearchEngine
lkml = S.searchEngine "LKML" "https://lore.kernel.org/linux-kernel/?q="

searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("d", S.duckduckgo)
             , ("e", ebay)
             , ("h", S.hoogle)
             , ("l", lkml)
             , ("u", linuxUsb)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("z", S.amazon)
             ]

------------------------------------------------------------------------
-- Layouts:
------------------------------------------------------------------------

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayout = avoidStruts . (mySpacing 6) $
      tiled
  ||| Mirror tiled
  ||| twopane
  ||| Mirror twopane
  ||| Full
  ||| three
  ||| grid
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- two panes
     twopane = TwoPane delta ratio

     --- ThreeColumns layout
     three   = ThreeCol nmaster delta ratio

     --- Grid layout
     grid    = GridRatio (4/3)

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "Gimp"                      --> doFloat
    , className =? "MPlayer"                   --> doFloat
    , className =? "Microsoft Teams - Preview" --> doFloat
    , className =? "Signal"                    --> doFloat
    , className =? "Zenity"                    --> doFloat
    , className =? "obs"                       --> doFloat
    , className =? "transmission"              --> doFloat
    , title     =? "OtoDecks"                  --> doFloat
    , resource  =? "desktop_window"            --> doIgnore
    , resource  =? "kdesktop"                  --> doIgnore
    , isFullscreen                             --> doFullFloat
    ]

------------------------------------------------------------------------
-- Event handling
------------------------------------------------------------------------

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging
------------------------------------------------------------------------
myLogHook :: X ()
myLogHook = return ()

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ docks def
    {
      -- simple stuff
      terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

    , mouseBindings      = myMouseBindings

      -- hooks, layouts
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = myEventHook
    , logHook            = dynamicLogWithPP xmobarPP
                           { ppOutput = \x -> hPutStrLn xmproc x
                           , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]"
                           , ppVisible = xmobarColor "#c3e88d" ""
                           , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""
                           , ppHiddenNoWindows = xmobarColor "#F07178" ""
                           , ppTitle = xmobarColor "#d0d0d0" "" . shorten 60
                           , ppSep =  "<fc=#666666> | </fc>"
                           , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
                           , ppExtras  = [windowCount]
                           , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                           }
    , startupHook        = myStartupHook
    } `additionalKeysP` myKeys
