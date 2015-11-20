" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

set nocp
call pathogen#infect()
syntax on
filetype plugin indent on

version 6.0
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
nmap gx <Plug>NetrwBrowseX
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)\

let &cpo=s:cpo_save
unlet s:cpo_save
set background=dark
set guifont=Ubuntu\ Mono\ 12
set backspace=indent,eol,start
set fileencodings=ucs-bom,utf-8,default,latin1
set helplang=en
set history=50
set nomodeline
set mouse=a
set printoptions=paper:letter
set ruler
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set window=81
set backup
set diffexpr=MyDiff()
set selection=exclusive
set selectmode=mouse,key
set whichwrap=b,s,<,>,[,]

"highlighting of columns
set colorcolumn=81

" vim: set ft=vim :

" ================ Indentation ======================
set autoindent
set smartindent
set smarttab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab
"appearance of tabs and trailing space
set listchars=tab:>-,trail:-
"set intelligent tab width
"text can also be indented with ^T/D in INSERT and >/< in NORMAL

set textwidth=80

"display line numbers
set number
set numberwidth=3

" ================ Search Settings =================
set incsearch "Find the next match as we type the search
set hlsearch "Hilight searches by default

" ================ Set Working Directory =================
"if has("unix")
"    cd ~/
"elseif has("win32")
"    cd C:\Users\Dafydd\Dropbox\Documents
"endif

"syntax highlighting for regular *.txt files
au BufReadPost *.txt set syntax=help

set fileencodings=ucs-bom,utf-8,default,latin1
setglobal fileencoding=utf-8
set encoding=utf-8
set termencoding=utf-8

set fileformat=unix
set fileformats=unix,dos,mac

if has("gui_running")
    set lines=30 columns=87
endif

" This makes vim act like all other editors, buffers can
" exist in the background without being in a window.
" http://items.sjbach.com/319/configuring-vim-right
set hidden

" Display tabs and trailing spaces visually
set list listchars=tab:→\ ,eol:¬,trail:·

" ================ Scrolling ========================
set scrolloff=8 "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1
set display=lastline "ensures that too long paragraphs at the bottom of the page
                     "are at least partially displayed
set guioptions+=b

" ================ Personal Commands and Remaps ========================
" for enabling soft wrapping
command! -nargs=* Wrap set wrap linebreak nolist textwidth=0 wrapmargin=0
" tab navigation
map <c-h> :tabp<cr>
map <c-l> :tabn<cr>

" ================ Folding (more tinkering required!) ========================
" folds automatically on blank line
:set foldexpr=getline(v:lnum-1)=~'^\\s*$'&&getline(v:lnum)=~'\\S'?'>1':1
:set foldmethod=expr
" set a high foldlevel, so that most folds are open on opening a file:
set foldlevel=20

" ================ Tags (more tinkering required!) ========================
" where to look for tags:
:set tags+=tags;/~

" ================ Windows Specific ========================
if has("win32")
    cnoremap <C-F4> c
    inoremap <C-F4> c
    cnoremap <C-Tab> w
    inoremap <C-Tab> w
    cmap <S-Insert> +
    imap <S-Insert> 
    xnoremap  ggVG
    snoremap  gggHG
    onoremap  gggHG
    nnoremap  gggHG
    vnoremap  "+y
    noremap  
    vnoremap  :update
    nnoremap  :update
    onoremap  :update
    nmap  "+gP
    omap  "+gP
    vnoremap  "+x
    noremap  
    noremap  u
    cnoremap   :simalt ~
    inoremap   :simalt ~
    map Q gq
    onoremap <C-F4> c
    nnoremap <C-F4> c
    vnoremap <C-F4> c
    onoremap <C-Tab> w
    nnoremap <C-Tab> w
    vnoremap <C-Tab> w
    vmap <S-Insert> 
    vnoremap <BS> d
    vmap <C-Del> "*d
    vnoremap <S-Del> "+x
    vnoremap <C-Insert> "+y
    nmap <S-Insert> "+gP
    omap <S-Insert> "+gP
    cnoremap  gggHG
    inoremap  gggHG
    inoremap  :update
    inoremap  u
    cmap  +
    inoremap  
    inoremap  u
    noremap   :simalt ~
    set guifont=UbuntuMono:h12
endif

" ================ Unix Specific ========================
if has("unix")
    map <S-Insert> <MiddleMouse>
    map! <S-Insert> <MiddleMouse>
endif

colorscheme desert

" LISP / SLIMV?
let g:slimv_lisp = '$HOME/ccl-1.7/lx86cl64'
let g:lisp_rainbow=1

" MY OWN BINDINGS:
" nnoremap / :perldo 
" replace with perl regex:
nnoremap // :%!perl -pi -e ''<Left>
" send selected code to generic repl running in other window via ConqueTerm:
map \r y<C-w>wp
" send current line of code ...
map \l Y<C-w>wp
