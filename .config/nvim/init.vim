" remap esc to enter command mode in :terminal
tnoremap <Esc> <C-\><C-n>

if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

" autosave if tex
"function! TexPrefFunction()
  "autocmd TextChanged,TextChangedI <buffer> silent write
"endfunction
"autocmd Filetype tex call TexPrefFunction()

let g:vimtex_view_general_viewer = 'TeXShop'
let g:vimtex_fold_enabled = 1

augroup remember_folds
  autocmd!
  autocmd BufWinLeave * mkview
  autocmd BufWinEnter * silent! loadview
augroup END

" distraction-free writing (see plugin list below)
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

function! WordCount()
   let s:old_status = v:statusmsg
   let position = getpos(".")
   exe ":silent normal g\<c-g>"
   let stat = v:statusmsg
   let s:word_count = 0
   if stat != '--No lines in buffer--'
     let s:word_count = str2nr(split(v:statusmsg)[11])
     let v:statusmsg = s:old_status
   end
   call setpos('.', position)
   return s:word_count
endfunction

function! LightLineWordCount()
  return &filetype == 'tex' ? WordCount() . ' words' : ''
endfunction

" lightline colorscheme
let g:lightline = {
      \ 'colorscheme': 'seoul256',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified', 'wordcount' ] ]
      \ },
      \ 'component_function': {
      \   'wordcount': 'LightLineWordCount'
      \ },
      \ }

" indentation and folding (unfolded by default)
set expandtab tabstop=2 shiftwidth=2
set foldmethod=indent
au BufRead * normal zR

" leader key
let mapleader=" "
" unmap the shortcut to suspend vim
nnoremap <c-z> <nop>
let g:user_emmet_leader_key='<c-z>' " z for zen
" fix weird backtick behaviour in from VimTex
let g:vimtex_imaps_leader = ':'

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
nnoremap <Leader><CR> :noh<cr>

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

" window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" autoclose parenthesis
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>

" smooth scrolling
noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>

nnoremap <silent> <leader>f :TestFile<CR>
nnoremap <silent> <leader>n :TestNearest<CR>
nnoremap <silent> <leader>l :TestLast<CR>
let test#strategy = "vimux"
map <Leader>q :VimuxCloseRunner<CR>

" plugins
call plug#begin('~/.local/share/nvim/plugged')
  Plug 'chriskempson/base16-vim'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'benmills/vimux'
  Plug 'tpope/vim-obsession'
  Plug 'itchyny/lightline.vim'
  Plug 'alvan/vim-closetag'
  Plug 'scrooloose/nerdcommenter'
  Plug 'terryma/vim-smooth-scroll'
  Plug 'ntpeters/vim-better-whitespace'
  Plug 'mattn/emmet-vim'
  Plug 'machakann/vim-sandwich'
  Plug 'junegunn/goyo.vim'
  Plug 'junegunn/limelight.vim'
  Plug 'lervag/vimtex'
  Plug 'janko-m/vim-test'
  Plug 'wesQ3/vim-windowswap'
call plug#end()
