local paq = require('paq-nvim').paq
paq {'lewis6991/gitsigns.nvim'}
paq {'tpope/vim-fugitive'}
paq {'tpope/vim-rhubarb'}

require('gitsigns').setup {
  signs = {
    add          = {hl = 'GitSignsAdd'   , text = '', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
    change       = {hl = 'GitSignsChange', text = '', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    delete       = {hl = 'GitSignsDelete', text = '', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    topdelete    = {hl = 'GitSignsDelete', text = '', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    changedelete = {hl = 'GitSignsChange', text = '', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
  },
  numhl = true,
}

vim.g['gitgutter_signs'] = 0
vim.g['gitgutter_highlight_linenrs'] = 1

local opts = { noremap=true, silent=true }
vim.api.nvim_set_keymap('n', '<leader>gg', ':G<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>goo', ':GBrowse<CR>', opts)
