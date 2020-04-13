defaults = {}

-- solarized color map from:
-- https://gist.github.com/weakish/923868
-- defaults.colormap = {
-- '#070736364242',
-- '#dcdc32322f2f',
-- '#858599990000',
-- '#b5b589890000',
-- '#26268b8bd2d2',
-- '#d3d336368282',
-- '#2a2aa1a19898',
-- '#eeeee8e8d5d5',
-- '#00002b2b3636',
-- '#cbcb4b4b1616',
-- '#58586e6e7575',
-- '#65657b7b8383',
-- '#838394949696',
-- '#6c6c7171c4c4',
-- '#9393a1a1a1a1',
-- '#fdfdf6f6e3e3',
-- }

-- more solarized termit links:
-- https://github.com/alpha-omega/termite-colors-solarized
-- https://github.com/altercation/solarized/issues/67

-- defaults.foregroundColor = 'white'
-- defaults.backgroundColor = 'black'
defaults.font = 'Terminus (TTF) Medium 13'
defaults.hideMenubar = true
defaults.encoding = 'UTF-8'
defaults.allowChangingTitle = true
defaults.hideSingleTab = true
defaults.showBorder = false
defaults.showScrollbar = false
defaults.urgencyOnBell = true
setOptions(defaults)

-- bindKey('Ctrl-j', prevTab)
-- bindKey('Ctrl-k', nextTab)
-- Don't close tab with Ctrl-w
bindKey('Ctrl-w', nil)
bindKey('Ctrl-t', nil)
-- bindKey('Ctrl-l', reconfigure)
-- Ctrl-v is used by vim
-- bindKey('Ctrl-v', paste)
-- I need something better for copy
-- bindKey('Ctrl-c', copy)

function changeTabFontSize(delta)
    tab = tabs[currentTabIndex()]
    setTabFont(string.sub(tab.font, 1, string.find(tab.font, '%d+$') - 1)..(tab.fontSize + delta))
end

bindKey('Ctrl-plus', function () changeTabFontSize(1) end)
bindKey('Ctrl-minus', function () changeTabFontSize(-1) end)