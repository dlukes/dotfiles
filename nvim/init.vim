let mapleader = ' '
let maplocalleader = ' '
" don't use `pyenv which` here, because that can be overridden by `pyenv
" local`
let g:python3_host_prog = trim(system('pyenv root'))
  \ . '/versions/'
  \ . trim(system('pyenv global'))
  \ . '/bin/python3'

"--------------------------------------------------------------- Plugins

call plug#begin()
" core
Plug 'tpope/vim-sensible'
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'ctrlpvim/ctrlp.vim'
" `:w !sudo tee %` doesn't work in nvim: https://github.com/neovim/neovim/issues/8678
Plug 'lambdalisue/suda.vim'
" community-maintained configs for various langs
Plug 'neovim/nvim-lsp'
Plug 'nvim-lua/completion-nvim'
Plug 'nvim-lua/diagnostic-nvim'
Plug 'nvim-lua/lsp-status.nvim'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'nvim-treesitter/nvim-treesitter-refactor'
Plug 'nvim-treesitter/nvim-treesitter-textobjects'
Plug 'airblade/vim-gitgutter'

" visuals
Plug 'junegunn/seoul256.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" nice to have
Plug 'liuchengxu/vim-which-key'
Plug 'jreybert/vimagit'
Plug 'Vigemus/iron.nvim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/vim-easy-align'
Plug 'nvim-treesitter/playground'

" filetype-specific
Plug 'psf/black', { 'for': 'python', 'branch': 'stable' }
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
" TODO: get rid of all custom markdown config once TreeSitter support
" lands
Plug 'tpope/vim-markdown', { 'for': 'markdown' }
Plug 'ElmCast/elm-vim', { 'for': 'elm' }
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'dag/vim-fish', { 'for': 'fish' }
call plug#end()

"------------------------------------------------ Functions and commands

" Let the EditorConfig plugin handle this.
" function! s:cleanup_whitespace()
"   let save_cursor = getpos('.')
"   silent %s/\s\+$//e
"   silent %s/\($\n\s*\)\+\%$//e
"   call setpos('.', save_cursor)
" endfunction

function! s:auto_chmod()
  let first_line = getline(1)
  if first_line =~ '^#!'
    silent !chmod +x %
  endif
endfunction

function! s:find_git_root()
  return systemlist('git rev-parse --show-toplevel')[0]
endfunction

function! ZoteroCite()
  " The citation format to use is determined either via a magic comment
  " containing cayw=format on the first line, or via the filetype.
  "
  " LaTeX citation format cheatsheet:
  "
  " latex = natbib (citep is parenthetical, citet is textual, and there
  " are other variations for omitting parts of the citation (e.g.
  " citeyearpar is only the year in parens, for when the text already
  " contains the author's name)
  "
  " biblatex = biblatex (cite, parencite, autocite etc.)
  let matches = matchlist(getline(1), 'cayw=\(\S\+\)')
  if len(matches) > 1
    let format = matches[1]
  else
    let format = &filetype =~ '.*tex' ? 'biblatex' : 'pandoc'
  endif
  let api_call = 'http://localhost:23119/better-bibtex/cayw?format='.format.'&brackets=1'
  let ref = system('curl -s '.shellescape(api_call))
  return ref
endfunction

function! ToggleFolds()
  if &l:foldenable
    setlocal nofoldenable
    setlocal foldcolumn=0
  else
    write
    setlocal foldenable
    setlocal foldcolumn=auto:9
    normal! zx
  endif
endfunction

function! s:black_reinstall()
  " black's vim integration is not exactly smooth and I never remember
  " all the stuff I have to nuke to start afresh
  call plug#load('black')
  echom "BlackReinstall: Removing Black's virtualenv in ".g:black_virtualenv.'.'
  echom repeat('=', 72)
  python3 import os, shutil, vim; shutil.rmtree(os.path.expanduser(vim.eval("g:black_virtualenv")))
  BlackUpgrade
  echom repeat('=', 72)
  echom 'BlackReinstall: If the issue persists, run :PlugUpdate black and retry reinstalling.'
endfunction

command! BlackReinstall :call s:black_reinstall()

command! -bang LspClients :cal v:lua.init.lsp_clients(<bang>0)

"--------------------------------------------------------- Auto commands

" sane behavior when switching buffers -- leave my cursor where it is!
autocmd BufLeave * let b:winview = winsaveview()
autocmd BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif

autocmd BufWritePost * call s:auto_chmod()

" comment syntax definitions not provided by vim-commentary
autocmd FileType sql setlocal commentstring=--\ %s
autocmd FileType cfg setlocal commentstring=#\ %s
autocmd FileType markdown setlocal foldmethod=syntax

augroup Python
  autocmd!
  autocmd BufWritePre *.py Black
  autocmd FileType python setlocal makeprg=pylint\ --output-format=parseable\ --score=no
  " autocmd BufWritePost *.py silent make! <afile> | silent redraw!
augroup END

" auto-open quickfix list if :make et al. yield errors
autocmd QuickFixCmdPost [^l]* cwindow

"-------------------------------------------------------------- Settings

" allow hiding buffers with changes
set hidden
set timeoutlen=500
set updatetime=500
" don't redraw screen in the middle of a macro (faster)
set lazyredraw
set splitbelow splitright
" make sure vim knows there are 256 colors
set t_Co=256
" don't hard wrap when appending to line which is already longer than
" textwidth
set formatoptions+=l
set list
set completeopt=menuone,noinsert,noselect
" shows ex command results preview as you type, only works with :s
" currently
set inccommand=nosplit
" character triggering completion in macros
set wildcharm=<Tab>
" let find search in dir of active buffer and recursively under current
" working directory (works better without autochdir)
set path=.,**
set ignorecase smartcase
" persistent undo instead of backups, and swap files tucked away please
set nobackup
set undofile
if !isdirectory($HOME . "/.config/nvim/undo")
  call mkdir($HOME . "/.config/nvim/undo", "p", 0700)
endif
set undodir=~/.config/nvim/undo
if !isdirectory($HOME . "/.config/nvim/swp")
  call mkdir($HOME . "/.config/nvim/swp", "p", 0700)
endif
set directory=~/.config/nvim/swp
" allow modelines (some distros disable them)
set modeline
set modelines=5
" always showing sign column adds room from screen edge and prevents
" horizontal shifting when signs are shown/hidden
set signcolumn=yes
set cursorline
" disable folds by default (toggle them with zi)
set nofoldenable
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
set mouse=a
if executable("rg")
  set grepprg=rg\ --vimgrep\ --no-heading
  set grepformat=%f:%l:%c:%m,%f:%l:%m
endif

" elflord is a nicely readable built-in one but seoul256 is better;
" another fairly nice one but harder on the eyes is
" liuchengxu/space-vim-dark
colorscheme seoul256
highlight! Comment cterm=italic
" readable background for whichkey floating windows
highlight! link WhichKeyFloating NormalFloat
highlight! link LspDiagnosticsError SpellBad
highlight! link LspDiagnosticsWarning SpellRare
highlight! link LspDiagnosticsInformation SpellCap
highlight! link LspDiagnosticsHint SpellLocal
highlight! link LspReferenceText Search
highlight! link LspReferenceRead Search
highlight! link LspReferenceWrite Search
call sign_define("LspDiagnosticsErrorSign", {"text" : "×", "texthl" : "LspDiagnosticsError"})
call sign_define("LspDiagnosticsWarningSign", {"text" : "!", "texthl" : "LspDiagnosticsWarning"})
call sign_define("LspDiagnosticsInformationSign", {"text" : "i", "texthl" : "LspDiagnosticsInformation"})
call sign_define("LspDiagnosticsHintSign", {"text" : "›", "texthl" : "LspDiagnosticsHint"})

let g:diagnostic_insert_delay = 1

let g:markdown_folding = 1
let g:markdown_fenced_languages = ['python', 'rust',
    \ 'sh', 'sql', 'r',
    \ 'html', 'css', 'javascript',
    \ 'perl', 'ruby', 'c', 'java', 'haskell', 'lisp', 'clojure',
    \ 'conf', 'diff', 'xml', 'systemd'
    \ ]

function! LspStatus() abort
  let status = luaeval('require("lsp-status").status()')
  return trim(status)
endfunction
call airline#parts#define_function('lsp_status', 'LspStatus')
call airline#parts#define_condition('lsp_status', 'luaeval("#vim.lsp.buf_get_clients() > 0")')
let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#nvimlsp#enabled = 0
let g:airline_section_warning = airline#section#create_right(['lsp_status'])

let g:elm_format_autosave = 1

" make sure *.tex files are never interpreted as ft=plaintex, so that
" vimtex is used
let g:tex_flavor = 'latex'
let g:vimtex_format_enabled = 1

"---------------------------------------------------------- Key bindings

inoremap fd <Esc>
" switch to normal mode in :terminal
tnoremap fd <C-\><C-n>
" avoid having to escape special characters in searches ("very magic")
noremap / /\v
noremap ? ?\v
noremap <Right> gt
noremap <Left> gT
noremap <Down> :bn<CR>
noremap <Up> :bp<CR>
noremap <silent> zi :call ToggleFolds()<CR>
inoremap <silent><expr> <C-n>
  \ pumvisible() ? "\<C-n>" :
  \ completion#trigger_completion()

nnoremap <silent> <leader> :<C-u>WhichKey '<Space>'<CR>
noremap <leader><leader> :
" execute visual selection of Vimscript code
vnoremap <leader>x y \| :@"<CR>
nnoremap <leader>pp :lua print(vim.inspect())<Left><Left>
nnoremap <silent> <leader>xx :lua run_md_block()<CR>
nnoremap <silent> <leader>xb :lua run_md_blocks("before")<CR>
nnoremap <silent> <leader>xa :lua run_md_blocks("after")<CR>
nnoremap <silent> <leader>xf :lua run_md_blocks()<CR>
noremap <leader><Tab> <C-^>
noremap <leader>w <C-w>
" enable command history filtering for Ctrl-P and Ctrl-N
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
" without autochdir, this is useful to quickly complete the path of the
" active buffer
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

noremap <leader>ff :CtrlP<CR>
noremap <leader>fr :CtrlPMRU<CR>
noremap <leader>bb :CtrlPBuffer<CR>
noremap <leader>fb :CtrlPMixed<CR>
noremap <leader>dg :diffget \| diffupdate<CR>
noremap <leader>dp :diffput \| diffupdate<CR>
noremap <leader>ll :Goyo \| Limelight!!<CR>
noremap <leader>z "=ZoteroCite()<CR>p
inoremap <C-z> <C-r>=ZoteroCite()<CR>

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

"------------------------------------------------------- Pseudo-snippets

inoremap ;py ```python<CR><CR>```<Up>

"-------------------------------------------------------------- Lua init

lua init = require("init")
