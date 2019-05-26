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
noremap ; :
noremap : ;

" easier motion within lines
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

if ! filereadable(expand('~/.config/nvim/autoload/plug.vim'))
  echo "Downloading junegunn/vim-plug to manage plugins..."
  silent !mkdir -p ~/.config/nvim/autoload/
  silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.config/nvim/autoload/plug.vim
endif

call plug#begin('~/.config/nvim/plugged')
  Plug 'lervag/vimtex'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'benmills/vimux'
  Plug 'tpope/vim-obsession'
  Plug 'alvan/vim-closetag'
  Plug 'mattn/emmet-vim'
  Plug 'machakann/vim-sandwich'
  Plug 'junegunn/goyo.vim'
  Plug 'junegunn/limelight.vim'
  Plug 'janko-m/vim-test'
  Plug 'wesQ3/vim-windowswap'
  Plug 'scrooloose/nerdcommenter'
  Plug 'edkolev/tmuxline.vim'
  Plug 'honza/vim-snippets'
  Plug 'SirVer/ultisnips'
call plug#end()

function! s:goyo_enter()
  Limelight
  set norelativenumber
  autocmd! numbertoggle
endfunction

function! s:goyo_leave()
  Limelight!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
endfunction

autocmd! User GoyoEnter call <SID>goyo_enter()
autocmd! User GoyoLeave call <SID>goyo_leave()

nnoremap <Leader>g :Goyo<CR>

let g:NERDDefaultAlign = 'left'
let g:NERDCompactSexyComs = 1

" unmap the shortcut to suspend vim
nnoremap <c-z> <nop>
let g:user_emmet_leader_key='<c-z>' " z for zen
" fix weird backtick behaviour in from VimTex
let g:vimtex_imaps_leader = ':'

nnoremap <silent> <leader>f :TestFile<CR>
nnoremap <silent> <leader>n :TestNearest<CR>
nnoremap <silent> <leader>l :TestLast<CR>
let test#strategy = "vimux"
map <Leader>q :VimuxCloseRunner<CR>

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

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

" Automatically delete all trailing whitespace on save
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
