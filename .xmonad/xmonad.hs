import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-WindowBringer.html
import XMonad.Actions.WindowBringer
import qualified XMonad.StackSet as W

myManageHook = composeAll [
    (className =? "Pidgin" <&&> (title =? "Pidgin" <||> title =? "Accounts")) --> doCenterFloat
-- , (className =? "Pidgin") --> doShift "3"
  , (className =? "Gnome-panel" <&&> title =? "Run Application") --> doCenterFloat
  , (className =? "Gcr-prompter") --> doCenterFloat
  ]

avoidFocusStealingManageHook = fmap not isDialog --> doF avoidMaster

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) -> W.Stack r [] (t:rs)
     otherwise           -> c

main = xmonad $ gnomeConfig
        { modMask = mod4Mask
        , terminal = "x-terminal-emulator-default"
        , layoutHook = smartBorders (layoutHook gnomeConfig)
        , manageHook = myManageHook <+> avoidFocusStealingManageHook <+> manageHook gnomeConfig
        }
        `additionalKeysP`
                 [ ("M-d", spawn "e")
                 , ("M-<Return>", spawn "x-terminal-emulator")
                 , ("M-f", gnomeRun)
                 , ("M-S-q", spawn "gnome-session-quit")
                 , ("M-<Left>",    prevWS )
                 , ("M-<Right>",   nextWS )
                 , ("M-S-<Left>",  shiftToPrev )
                 , ("M-S-<Right>", shiftToNext )
                 , ("M-h", sendMessage ToggleStruts)
                 , ("M-S-h", sendMessage $ ToggleStrut U)
                 , ("M-C-h", sendMessage $ ToggleStrut D)
                 , ("M-g", gotoMenu)
                 , ("M-b", bringMenu)
                 ]
