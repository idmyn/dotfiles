require('paq-nvim').paq {'nvim-treesitter/nvim-treesitter'}

require'nvim-treesitter.configs'.setup {
  ensure_installed = 'maintained',
  highlight = {
    enable = true,
  },
  indent = {
    enable = true
  }
}
