return {
  "stevearc/conform.nvim",
  cond = not vim.g.vscode,
  event = "BufWritePre",
  opts = {
    formatters_by_ft = {
      javascript = { lsp_format = "prefer", filter = function(c) return c.name == "biome" end },
      typescript = { lsp_format = "prefer", filter = function(c) return c.name == "biome" end },
      javascriptreact = { lsp_format = "prefer", filter = function(c) return c.name == "biome" end },
      typescriptreact = { lsp_format = "prefer", filter = function(c) return c.name == "biome" end },
      json = { lsp_format = "prefer", filter = function(c) return c.name == "biome" end },
      jsonc = { lsp_format = "prefer", filter = function(c) return c.name == "biome" end },
    },
    format_on_save = {
      timeout_ms = 1000,
    },
  },
}
