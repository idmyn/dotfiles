return {
  "maxmx03/solarized.nvim",
  lazy = false,
  priority = 1000,
  config = function()
    vim.o.termguicolors = true
    vim.o.background = "light"
    require("solarized").setup({
      variant = "winter",
    })
    vim.cmd.colorscheme("solarized")

    local bg = "#FFFFF8"
    local fg = "#111111"
    local purple = "#484FD5"
    local green = "#91EE92"
    local hl = function(name, val)
      vim.api.nvim_set_hl(0, name, val)
    end

    -- editor
    hl("Normal", { fg = fg, bg = bg })
    hl("NormalNC", { fg = fg, bg = bg })
    hl("NormalFloat", { fg = fg, bg = bg })
    hl("FloatBorder", { fg = "#93A1A1", bg = bg })
    hl("SignColumn", { bg = bg })
    hl("LineNr", { fg = "#93A1A1", bg = bg })
    hl("CursorLineNr", { fg = fg, bg = bg, bold = true })
    hl("CursorLine", { bg = "#F3EDDA" })
    hl("EndOfBuffer", { fg = bg })
    hl("Pmenu", { fg = fg, bg = "#E1E7DF" })
    hl("PmenuSel", { fg = fg, bg = "#C9D1C7" })

    -- gitsigns
    hl("GitSignsAdd", { fg = "#859900", bg = bg })
    hl("GitSignsChange", { fg = "#B58900", bg = bg })
    hl("GitSignsDelete", { fg = "#DC322F", bg = bg })

    -- search
    hl("Search", { bg = green })
    hl("IncSearch", { bg = green })
    hl("CurSearch", { bg = green })

    -- comments: bold, same color as text
    hl("Comment", { fg = fg, bold = true })
    hl("@comment", { fg = fg, bold = true })

    -- keywords/statements: purple
    hl("Keyword", { fg = purple })
    hl("@keyword", { fg = purple })
    hl("Statement", { fg = purple })
    hl("Conditional", { fg = purple })
    hl("Repeat", { fg = purple })
    hl("Include", { fg = purple })
    hl("Define", { fg = purple })
    hl("Macro", { fg = purple })
    hl("PreProc", { fg = purple })
    hl("PreCondit", { fg = purple })
    hl("Exception", { fg = purple })
    hl("StorageClass", { fg = purple })
    hl("Typedef", { fg = purple })

    -- everything else: plain text
    local plain = { fg = fg }
    hl("@punctuation.special", plain)
    hl("Identifier", plain)
    hl("@variable", plain)
    hl("@variable.builtin", plain)
    hl("@variable.parameter", plain)
    hl("@variable.member", plain)
    hl("Property", plain)
    hl("@property", plain)
    hl("@property.json", plain)
    hl("@property.yaml", plain)
    hl("Parameter", { fg = fg, italic = false })
    hl("Function", plain)
    hl("@function", plain)
    hl("@function.builtin", plain)
    hl("@function.call", plain)
    hl("@function.method", plain)
    hl("@function.method.call", plain)
    hl("Constant", plain)
    hl("@constant", plain)
    hl("@constant.builtin", plain)
    hl("String", plain)
    hl("@string", plain)
    hl("Character", plain)
    hl("Number", plain)
    hl("Boolean", plain)
    hl("Float", plain)
    hl("Type", plain)
    hl("@type", plain)
    hl("@type.builtin", plain)
    hl("Structure", plain)
    hl("Special", plain)
    hl("SpecialChar", plain)
    hl("Tag", plain)
    hl("TagAttribute", plain)
    hl("TagDelimiter", plain)
    hl("Delimiter", plain)
    hl("Operator", plain)
    hl("@operator", plain)
    hl("@module", plain)
    hl("@constructor", plain)
    hl("@punctuation.delimiter", plain)
    hl("@punctuation.bracket", plain)
  end,
}
