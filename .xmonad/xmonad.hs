{-# LANGUAGE OverloadedStrings #-}

import DBus
import DBus.Client

import System.Taffybar.XMonadLog

import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-WindowBringer.html
import XMonad.Actions.WindowBringer
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowMenu

myManageHook = composeAll [
--    (className =? "Pidgin" <&&> (title =? "Pidgin" <||> title =? "Accounts")) --> doCenterFloat
  ]

avoidFocusStealingManageHook
  = (
           className /=? "Gmrun"
      <&&> className /=? "Termit"
      <&&> className /=? "X-terminal-emulator"
      <&&> fmap not isDialog
    )
    --> doF avoidFocusStealing

avoidFocusStealing :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidFocusStealing = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) -> W.Stack r [] (t:rs)
     otherwise           -> c

logHookPP :: PP
logHookPP = taffybarPP {
    ppCurrent = taffybarColor "green" "#333333"
  , ppVisible = taffybarColor "yellow" "#333333"
  , ppTitle = taffybarColor "green"  "" . shorten 90
  , ppHiddenNoWindows = taffybarColor "#bbbbbb" ""
}

main = do
  dbusClient <- connectSession
  xmonad $ gnomeConfig
        { modMask = mod4Mask
        , terminal = "x-terminal-emulator-default"
        , layoutHook = smartBorders (layoutHook gnomeConfig)
        , manageHook = myManageHook <+> avoidFocusStealingManageHook <+> manageHook gnomeConfig
        , logHook = dbusLogWithPP dbusClient logHookPP >> logHook gnomeConfig
        }
        `additionalKeysP`
                 [ ("M-d", spawn "e")
                 , ("M-<Return>", spawn "x-terminal-emulator")
                 , ("M-S-q", spawn "gnome-session-quit")
                 , ("M-<Left>",    prevWS )
                 , ("M-<Right>",   nextWS )
                 , ("M-S-<Left>",  shiftToPrev )
                 , ("M-S-<Right>", shiftToNext )

                 , ("M-f", sendMessage ToggleStruts)
                 , ("M-S-f", sendMessage $ ToggleStrut U)
                 , ("M-C-f", sendMessage $ ToggleStrut D)
                 , ("M-g", gotoMenu)
                 , ("M-b", bringMenu)
                 , ("M-o", windowMenu)
                 , ("M1-C-l", spawn "gnome-screensaver-command -l")
                 , ("M1-S-l", spawn "cinnamon-session-quit")
                 , ("M1-S-q", spawn "cinnamon-session-quit --power-off")
                 , ("M1-S-r", spawn "cinnamon-session-quit --reboot")
                 ]

