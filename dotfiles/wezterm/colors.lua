local module = {}

function module.apply_to_config(config)
    config.colors = {
        foreground = '#111111',
        background = '#fffff8',
        cursor_fg = '#fffff8',
        cursor_bg = '#111111',
        ansi = {
            '#111111',
            '#e0000f',
            '#52a352',
            '#e59400',
            '#0073e5',
            '#7d65f6',
            '#00b2b2',
            '#c5c8c6',
        },
        brights = {
            '#111111',
            '#d54e53',
            '#00CC00',
            '#e7c547',
            '#7aa6da',
            '#c397d8',
            '#70c0b1',
            '#979797',
        },
    }
end

return module
