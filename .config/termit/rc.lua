defaults = {}
defaults.foregroundColor = 'white'
defaults.backgroundColor = 'black'
defaults.font = 'Monospace 13'
defaults.hideMenubar = true
defaults.encoding = 'UTF-8'
defaults.allowChangingTitle = true
defaults.hideSingleTab = true
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
