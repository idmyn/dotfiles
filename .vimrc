if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

" Change cursor shape between insert and normal mode in iTerm2.app
if $TERM_PROGRAM =~ "iTerm"
    let &t_SI = "\<Esc>]50;CursorShape=1\x7" " Vertical bar in insert mode
    let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
endif

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

" smooth scrolling
noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>

call plug#begin('~/.vim/plugged')
  Plug 'chriskempson/base16-vim'
  Plug 'scrooloose/nerdcommenter'
  Plug 'terryma/vim-smooth-scroll'
  Plug 'ntpeters/vim-better-whitespace'
  Plug 'machakann/vim-sandwich'
call plug#end()
