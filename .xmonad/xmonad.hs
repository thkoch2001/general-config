import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS

myManageHook = composeAll [
    (className =? "Pidgin" <&&> (title =? "Pidgin" <||> title =? "Accounts")) --> doCenterFloat
-- , (className =? "Pidgin") --> doShift "3"
  , (className =? "Gnome-panel" <&&> title =? "Run Application") --> doCenterFloat
  , (className =? "Gcr-prompter") --> doCenterFloat
  ]

main = xmonad $ gnomeConfig
        { modMask = mod4Mask
        , terminal = "x-terminal-emulator"
        , layoutHook = smartBorders (layoutHook gnomeConfig)
        , manageHook = myManageHook <+> manageHook gnomeConfig
        }
        `additionalKeysP`
                 [ ("M-e", spawn "e")
                 , ("M-r", gnomeRun)
                 , ("M-S-q", spawn "gnome-session-quit")
                 , ("M-<Left>",    prevWS )
                 , ("M-<Right>",   nextWS )
                 , ("M-S-<Left>",  shiftToPrev )
                 , ("M-S-<Right>", shiftToNext )
                 , ("M-h", sendMessage ToggleStruts)
                 , ("M-S-h", sendMessage $ ToggleStrut U)
                 , ("M-C-h", sendMessage $ ToggleStrut D)
                 ]