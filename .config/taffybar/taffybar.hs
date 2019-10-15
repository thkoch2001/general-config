{-# LANGUAGE CPP #-}

import           System.Taffybar

import           System.Taffybar.Battery
import           System.Taffybar.Menu.MenuWidget
import           System.Taffybar.SimpleClock
import           System.Taffybar.Systray
import           System.Taffybar.Pager

import           System.Taffybar.Widgets.PollingBar
import           System.Taffybar.Widgets.PollingGraph
import           System.Taffybar.WorkspaceHUD

import           System.Information.CPU
import           System.Information.Memory

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main = do

  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
      clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      battery = batteryBarNew defaultBatteryConfig 60

  pager <- pagerNew defaultPagerConfig
  let workspaceHUD = buildWorkspaceHUD defaultWorkspaceHUDConfig pager

  defaultTaffybar defaultTaffybarConfig {
      barHeight = 15
    , startWidgets = [
        (menuWidgetNew $ Just "cinnamon-"),
        workspaceHUD
      ]
    , endWidgets = [ tray, battery, clock, mem, cpu ]
  }
