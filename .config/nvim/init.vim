" Copy selected text to system clipboard (requires gvim/nvim/vim-x11)
" (on macOS I emulate this behaviour with cmd-c and cmd-v via iTerm2 prefs)
vnoremap <C-c> "+y
map <C-p> "+P

" remap esc to enter command mode in :terminal
tnoremap <Esc> <C-\><C-n>

" hybrid line numbers, loosing relative numbers when inserting
set number relativenumber
augroup numbertoggle
	autocmd!
	autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
	autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

" syntax highlighting via base16 shell
if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

let g:airline_theme='base16_oceanicnext'

let g:tmuxline_powerline_separators = 0

" autosave if tex
"function! TexPrefFunction()
  "autocmd TextChanged,TextChangedI <buffer> silent write
"endfunction
"autocmd Filetype tex call TexPrefFunction()

let g:tex_flavor = "latex"

let g:vimtex_view_general_viewer = 'TeXShop'
let g:vimtex_fold_enabled = 1

" indentation and folding (unfolded by default)
set expandtab tabstop=2 shiftwidth=2
set foldmethod=indent
au BufRead * normal zR

" prevent folds closing automatically
set nofoldenable

augroup remember_folds
  autocmd!
  autocmd BufWinLeave * mkview
  autocmd BufWinEnter * silent! loadview
augroup END

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

let g:NERDDefaultAlign = 'left'
let g:NERDCompactSexyComs = 1

" leader key
let mapleader=" "
" unmap the shortcut to suspend vim
nnoremap <c-z> <nop>
let g:user_emmet_leader_key='<c-z>' " z for zen
" fix weird backtick behaviour in from VimTex
let g:vimtex_imaps_leader = ':'

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

" open horizontal splits below, and vertical splits to the right
set splitbelow splitright

" remove netrw file tree banner and prevent history files
let g:netrw_banner = 0
let g:netrw_dirhistmax = 0

" keep swap/backup/undo files out of working directory
set backupdir=.backup/,~/.backup/,/tmp//
set directory=.swp/,~/.swp/,/tmp//
set undodir=.undo/,~/.undo/,/tmp//

" shiftless commands
nnoremap ; :
nnoremap : ;

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

nnoremap <silent> <leader>f :TestFile<CR>
nnoremap <silent> <leader>n :TestNearest<CR>
nnoremap <silent> <leader>l :TestLast<CR>
let test#strategy = "vimux"
map <Leader>q :VimuxCloseRunner<CR>

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" plugins
call plug#begin('~/.local/share/nvim/plugged')
  Plug 'chriskempson/base16-vim'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'benmills/vimux'
  Plug 'tpope/vim-obsession'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'alvan/vim-closetag'
  Plug 'mattn/emmet-vim'
  Plug 'machakann/vim-sandwich'
  Plug 'junegunn/goyo.vim'
  Plug 'junegunn/limelight.vim'
  Plug 'lervag/vimtex'
  Plug 'janko-m/vim-test'
  Plug 'wesQ3/vim-windowswap'
  Plug 'scrooloose/nerdcommenter'
	Plug 'edkolev/tmuxline.vim'
	Plug 'honza/vim-snippets'
	Plug 'SirVer/ultisnips'
call plug#end()
