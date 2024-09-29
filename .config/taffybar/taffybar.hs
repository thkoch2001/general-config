{-# LANGUAGE OverloadedStrings #-}

import System.Taffybar
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Battery
--import System.Taffybar.Widget.FreedesktopNotifications
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Windows
import System.Taffybar.Widget.XDGMenu.MenuWidget

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [ totalLoad, systemLoad ]

main = do
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Just "mem"
                                  }

      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
                                  , graphLabel = Just "cpu"
                                  }
      clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      textBattery = textBatteryNew "$percentage$% ($time$) $status$"
      workspaces = workspacesNew defaultWorkspacesConfig
      menu = menuWidgetNew Nothing
      windows = windowsNew $ WindowsConfig {
          getMenuLabel = truncatedGetMenuLabel 120
        , getActiveLabel = truncatedGetActiveLabel 60
                                           }
--      notify = notifyAreaNew defaultNotificationConfig
      simpleConfig = defaultSimpleTaffyConfig
                       { startWidgets = [ menu, workspaces, windows ]
                       , endWidgets = [ {-notify,-} sniTrayNew, clock, textBattery, mem, cpu ]
                       }
  simpleTaffybar simpleConfig
