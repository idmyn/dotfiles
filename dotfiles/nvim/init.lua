-- inspired by https://crispgm.com/page/neovim-is-overpowering.html
-- and https://oroques.dev/notes/neovim-init/

vim.cmd 'source ~/.vimrc'

local scopes = {o = vim.o, b = vim.bo, w = vim.wo}

local function opt(scope, key, value)
  scopes[scope][key] = value
  if scope ~= 'o' then scopes['o'][key] = value end
end

opt('w', 'number', true)
opt('w', 'cursorline', true)
opt('w', 'signcolumn', 'yes')

opt('o', 'updatetime', 50)

vim.api.nvim_command('set undofile')
vim.api.nvim_command('set undodir=~/.vim/undodir')

vim.cmd 'packadd paq-nvim'              -- load the package manager
local paq = require('paq-nvim').paq     -- a convenient alias
paq {'savq/paq-nvim', opt = true}       -- paq-nvim manages itself
paq {'nvim-lua/plenary.nvim'}           -- required for lots of lua plugins
paq {'nvim-lua/popup.nvim'}             -- required for telescope.nvim

paq {'Raimondi/delimitMate'}
paq {'tpope/vim-commentary'}
paq {'airblade/vim-rooter'}
paq {'tpope/vim-vinegar'}

paq {'idmyn/eink.vim'}
paq {'LnL7/vim-nix'}

require('lsp')
require('telescope')
require('autocomplete')
require('treesitter')
require('git')

opt('o', 'termguicolors', true)
vim.cmd 'colorscheme eink'
