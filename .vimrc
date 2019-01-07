syntax on

" Highlight current line when in insert mode
autocmd InsertEnter,InsertLeave * set cul!

" leader key
let mapleader=" "

" indentation
set expandtab tabstop=2 shiftwidth=2

" hybrid line numbers, loosing relative numbers when inserting
set number relativenumber
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

set incsearch
set ignorecase
set smartcase
" Remove highlights with leader + enter
nnoremap <Leader><CR> :nohlsearch<cr>

set clipboard=unnamed

" indication that I've spilled over 79 character line-length limit
highlight ColorColumn ctermbg=red
call matchadd('ColorColumn', '\%80v', 100)

" better-whitespace plugin
let g:strip_whitespace_on_save=1

" open horizontal splits below, and vertical splits to the right
set splitbelow
set splitright

" remove netrw file tree banner and prevent history files
let g:netrw_banner = 0
let g:netrw_dirhistmax = 0

" shiftless commands
nnoremap ; :
nnoremap : ;

" window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" autoclose parenthesis
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>

call plug#begin('~/.vim/plugged')
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'scrooloose/nerdcommenter'
  Plug 'ntpeters/vim-better-whitespace'
  Plug 'machakann/vim-sandwich'
call plug#end()
