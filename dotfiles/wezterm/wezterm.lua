local wezterm = require 'wezterm'
local colors = require 'colors'
--local workspaces = require 'workspaces'
local keymap = require 'keymap'
local config = wezterm.config_builder()

config.default_prog = { '/Users/david/.nix-profile/bin/fish' }

colors.apply_to_config(config)
keymap.apply_to_config(config)

config.tab_bar_at_bottom = true
config.window_decorations = "RESIZE"

config.window_close_confirmation = 'NeverPrompt'

-- plugins to look at
-- https://github.com/twilsoft/wezmode
-- https://github.com/MLFlexer/smart_workspace_switcher.wezterm

return config
