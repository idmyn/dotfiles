-- inspired by https://crispgm.com/page/neovim-is-overpowering.html
-- and https://oroques.dev/notes/neovim-init/

vim.cmd 'source ~/.vimrc'

vim.opt.number = true
vim.opt.cursorline = true
vim.opt.signcolumn = 'yes'

vim.opt.updatetime = 50
vim.opt.hidden = true

vim.api.nvim_command('set undofile')
vim.api.nvim_command('set undodir=~/.vim/undodir')

vim.cmd 'packadd paq-nvim'              -- load the package manager
local paq = require('paq-nvim').paq     -- a convenient alias
paq {'savq/paq-nvim', opt = true}       -- paq-nvim manages itself
paq {'nvim-lua/plenary.nvim'}           -- required for lots of lua plugins
paq {'nvim-lua/popup.nvim'}             -- required for telescope.nvim

paq {'editorconfig/editorconfig-vim'}
paq {'jiangmiao/auto-pairs'}
paq {'tpope/vim-commentary'}
paq {'airblade/vim-rooter'}
paq {'tpope/vim-vinegar'}
paq {'tpope/vim-surround'}
paq {'leafOfTree/vim-matchtag'}
paq {'eraserhd/parinfer-rust', run = 'cargo build --release'}
paq {'mattn/emmet-vim'}

paq {'idmyn/eink.vim'}
paq {'sheerun/vim-polyglot'}

require('lsp')
require('telescope')
require('autocomplete')
require('treesitter')
require('git')

vim.opt.termguicolors = true
vim.cmd 'colorscheme eink'

vim.g.user_emmet_expandabbr_key='<C-e><C-e>'
