local module = {}
local wezterm = require("wezterm")
local workspace_switcher = wezterm.plugin.require("https://github.com/MLFlexer/smart_workspace_switcher.wezterm")

local wezmode = require("wezmode")

local status_color = "#26BA8B"

wezmode.setup({
        {
            name = "pane",            -- the name that will show in the status bar for the mode
            key = "p",                -- the key you'll use with your modifier to enter the mode
            modeColor = status_color, -- the color of the mode indicator
            keyTable = {
                {
                    key = "v",
                    desc = "vertical",
                    action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" })
                },
                {
                    key = "s",
                    desc = "horizontal",
                    action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" })
                },
                -- Cancel the mode by pressing escape
                -- { key = "Escape", desc = "back", action = "PopKeyTable" },
                { key = "Enter", desc = "back", action = "PopKeyTable" },
            }
        },
        {
            name = "tab",
            key = "t",
            modeColor = status_color,
            keyTable = {
                { key = "n",     desc = "new",   action = wezterm.action.SpawnTab("CurrentPaneDomain") },
                { key = "h",     desc = "left",  action = wezterm.action.ActivateTabRelative(-1) },
                { key = "l",     desc = "right", action = wezterm.action.ActivateTabRelative(1) },
                { key = "x",     desc = "close", action = wezterm.action.CloseCurrentTab({ confirm = false }) },
                -- Cancel the mode by pressing escape
                -- { key = "Escape", desc = "back",  action = "PopKeyTable" },
                { key = "Enter", desc = "back",  action = "PopKeyTable" },
            }
        }
    },
    {
        modifier = "CMD",
        theme = {
            normalModeColor = "fffff8",
            hintColor = status_color
        }
    })

-- 3. Set up the right status text to use our modes
wezmode.handleRightStatusUpdate()



function module.apply_to_config(config)
    config.use_fancy_tab_bar = false
    config.keys = wezmode.extendTable({
        -- pane moving
        { mods = "ALT", key = "h", action = wezterm.action { ActivatePaneDirection = "Left" } },
        { mods = "ALT", key = "l", action = wezterm.action { ActivatePaneDirection = "Right" } },
        { mods = "ALT", key = "k", action = wezterm.action { ActivatePaneDirection = "Up" } },
        { mods = "ALT", key = "j", action = wezterm.action { ActivatePaneDirection = "Down" } },

        { mods = "ALT", key = "s", action = workspace_switcher.switch_workspace() },
    }, wezmode.getKeys())

    config.key_tables = wezmode.getKeyTables()
end

return module
