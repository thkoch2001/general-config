{-# LANGUAGE OverloadedStrings #-}

import           System.Taffybar.Hooks.PagerHints (pagerHints)

import           XMonad
import           XMonad.Actions.CycleWS
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-WindowBringer.html
import           XMonad.Actions.WindowBringer
import           XMonad.Actions.WindowMenu
import           XMonad.Config.Gnome
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops        (ewmh)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
-- https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.NoBorders
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedWindows
import           XMonad.Util.Run



import qualified XMonad.StackSet                  as W

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- fmap (W.findTag w) $ gets windowset

    safeSpawn "notify-send" [show name, "workspace " ++ idx]

myManageHook = composeAll [
--    (className =? "Pidgin" <&&> (title =? "Pidgin" <||> title =? "Accounts")) --> doCenterFloat
      title =? "gcert" -->doCenterFloat
    , className =? "gnubby_ssh_prompt" --> doCenterFloat
    , className =? "Action_prodcertstatus.py" --> doCenterFloat
    , className =? "Lxsession-logout" --> doCenterFloat
  ]

avoidFocusStealingManageHook
  = (
           className /=? "Gmrun"
      <&&> className /=? "Termit"
      <&&> className /=? "X-terminal-emulator"
      <&&> className /=? "Lxsession-logout"
      <&&> fmap not isDialog
    )
    --> doF avoidFocusStealing

avoidFocusStealing :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidFocusStealing = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) -> W.Stack r [] (t:rs)
     otherwise           -> c

main = do
  xmonad
    $ ewmh $ pagerHints -- see System.Taffybar.TaffyPager
    $ withUrgencyHook LibNotifyUrgencyHook
    $ gnomeConfig
        { modMask = mod4Mask
        , terminal = "x-terminal-emulator-default"
        , layoutHook = smartBorders (layoutHook gnomeConfig)
        , manageHook = myManageHook <+> avoidFocusStealingManageHook <+> manageHook gnomeConfig
        }
        `additionalKeysP`
                 [ ("M-d", spawn "emacsclient -c")
                 , ("M-<Return>", spawn "x-terminal-emulator")
                 , ("M-S-<Return>", spawn "x-terminal-emulator -e 'tmux new-session -A -s default'")
                 , ("M-S-q", spawn "lxsession-logout")
                 , ("M-<Left>",    prevWS )
                 , ("M-<Right>",   nextWS )
                 , ("M-S-<Left>",  shiftToPrev )
                 , ("M-S-<Right>", shiftToNext )

                 , ("M-<Backspace>", focusUrgent)
                 , ("M-S-<Backspace>", clearUrgents)

                 , ("M-f", sendMessage ToggleStruts)
                 , ("M-S-f", sendMessage $ ToggleStrut U)
                 , ("M-C-f", sendMessage $ ToggleStrut D)
                 , ("M-g", gotoMenu)
                 , ("M-b", bringMenu)
                 , ("M-o", windowMenu)
                 , ("M1-C-l", spawn "cinnamon-screensaver-command -l")
                 ]

