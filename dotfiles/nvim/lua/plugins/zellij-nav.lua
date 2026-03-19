return {
  "swaits/zellij-nav.nvim",
  lazy = true,
  event = "VeryLazy",
  keys = {
    { "<a-h>", "<cmd>ZellijNavigateLeftTab<cr>",  { silent = true, desc = "navigate left or tab" } },
    { "<a-j>", "<cmd>ZellijNavigateDown<cr>",     { silent = true, desc = "navigate down" } },
    { "<a-k>", "<cmd>ZellijNavigateUp<cr>",       { silent = true, desc = "navigate up" } },
    { "<a-l>", "<cmd>ZellijNavigateRightTab<cr>", { silent = true, desc = "navigate right or tab" } },
  },
  opts = {},
}
