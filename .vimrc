" leader key
let mapleader=" "

set background=light
colo eink

" hybrid line numbers, losing relative numbers when inserting
set number relativenumber
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

" open horizontal splits below, and vertical splits to the right
set splitbelow splitright

" shiftless commands
nnoremap ; :
nnoremap : ;

" easier motion within lines/buffer
noremap H ^
noremap L g_
nnoremap K H
nnoremap J L

" window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" autoclose parenthesis
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
  Plug 'wincent/terminus'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'scrooloose/nerdcommenter'
  Plug 'machakann/vim-sandwich'
call plug#end()

" keep swap/backup/undo files out of working directory
set backupdir=.backup/,~/.backup/,/tmp//
set directory=.swp/,~/.swp/,/tmp//
set undodir=.undo/,~/.undo/,/tmp//

" remove netrw file tree banner and prevent history files
let g:netrw_banner = 0
let g:netrw_dirhistmax = 0

" flexible search settings
set incsearch
set ignorecase
set smartcase
" Remove highlights with leader + enter
nnoremap <Leader><CR> :noh<cr>

" indication that I've spilled over 79 character line-length limit
highlight ColorColumn ctermbg=red
call matchadd('ColorColumn', '\%80v', 100)

" automatically delete all trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e

" indentation and folding (unfolded by default)
set expandtab tabstop=2 shiftwidth=2
set foldmethod=indent
au BufRead * normal zR

" prevent folds closing automatically
set nofoldenable

augroup AutoSaveFolds
  autocmd!
  " view files are about 500 bytes
  " bufleave but not bufwinleave captures closing 2nd tab
  " nested is needed by bufwrite* (if triggered via other autocmd)
  autocmd BufWinLeave,BufLeave,BufWritePost ?* nested silent! mkview!
  autocmd BufWinEnter ?* silent! loadview
augroup end