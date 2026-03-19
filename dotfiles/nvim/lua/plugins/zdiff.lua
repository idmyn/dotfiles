return {
  "martindur/zdiff.nvim",
  cond = not vim.g.vscode,
  cmd = "Zdiff",
  keys = {
    { "<leader>gg", "<cmd>Zdiff<cr>", desc = "Zdiff (uncommitted)" },
  },
  opts = {
    default_expanded = true,
  },
}
