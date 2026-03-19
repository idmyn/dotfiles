return {
  "dmtrKovalenko/fff.nvim",
  cond = not vim.g.vscode,
  build = function()
    require("fff.download").download_or_build_binary()
  end,
  opts = {},
  lazy = false,
  keys = {
    { "<leader> ", function() require("fff").find_files() end, desc = "Find files" },
    { "<leader>sg", function() require("fff").live_grep() end, desc = "Live grep" },
    { "<leader>sp", function() require("fff").live_grep() end, desc = "Search in project" },
    { "<leader>sw", function() require("fff").live_grep({ query = vim.fn.expand("<cword>") }) end, desc = "Search current word" },
  },
}
