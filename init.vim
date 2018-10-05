" colorscheme and true colors
colo dracula
set termguicolors

" indentation
set expandtab tabstop=2 shiftwidth=2

" hybrid line numbers, loosing relative numbers when inserting
set number relativenumber
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

" open vertical splits to the right
set splitright

" remove netrw file tree banner and prevent history files
let g:netrw_banner = 0
let g:netrw_dirhistmax = 0

" keep swap files out of working directory
set dir=/private/tmp

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
inoremap {<CR> {<CR>}<ESC>0

" plugins
call plug#begin('~/.local/share/nvim/plugged')
  Plug 'vim-airline/vim-airline'
  Plug 'alvan/vim-closetag'
call plug#end()
