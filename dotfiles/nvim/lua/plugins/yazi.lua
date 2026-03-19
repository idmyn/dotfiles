return {
  "mikavilpas/yazi.nvim",
  cond = not vim.g.vscode,
  dependencies = { "nvim-lua/plenary.nvim" },
  opts = {},
  keys = {
    { "<leader>fd", "<cmd>Yazi<CR>", desc = "Open yazi at current file" },
  },
}
