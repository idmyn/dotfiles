" syntax colorscheme and true colors
colo dracula
set termguicolors

" lightline colorscheme
let g:lightline = {
  \ 'colorscheme': 'one',
  \ }

" indentation
set expandtab tabstop=2 shiftwidth=2

" leader key
let mapleader=" "
" unmap the shortcut to suspend vim
nnoremap <c-z> <nop>
let g:user_emmet_leader_key='<c-z>' " z for zen

" hybrid line numbers, loosing relative numbers when inserting
set number relativenumber
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

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

" keep swap files out of working directory
set dir=/private/tmp

" jk/kj (instead of <ESC>) to leave insert mode
let g:easyescape_chars = { "j": 1, "k": 1 }
let g:easyescape_timeout = 2000
cnoremap jk <ESC>
cnoremap kj <ESC>

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
inoremap {<CR> {<CR><CR>}<up><tab>

" smooth scrolling
noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>

" plugins
call plug#begin('~/.local/share/nvim/plugged')
  Plug 'itchyny/lightline.vim'
  Plug 'alvan/vim-closetag'
  Plug 'scrooloose/nerdcommenter'
  Plug 'zhou13/vim-easyescape'
  Plug 'terryma/vim-smooth-scroll'
  Plug 'ntpeters/vim-better-whitespace'
  Plug 'mattn/emmet-vim'
call plug#end()
