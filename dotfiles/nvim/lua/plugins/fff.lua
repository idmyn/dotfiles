return {
  "dmtrKovalenko/fff.nvim",
  build = function()
    require("fff.download").download_or_build_binary()
  end,
  opts = {},
  lazy = false,
  keys = {
    { "<leader> ", function() require("fff").find_files() end, desc = "Find files" },
    { "<leader>sp", function() require("fff").live_grep() end, desc = "Search in project" },
    { "<leader>sp", function()
      vim.cmd('noau normal! "vy')
      require("fff").live_grep({ query = vim.fn.getreg("v") })
    end, mode = "v", desc = "Search selected text" },
  },
}
