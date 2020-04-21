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
Plug 'jiangmiao/auto-pairs'
" `:w !sudo tee %` doesn't work in nvim: https://github.com/neovim/neovim/issues/8678
Plug 'lambdalisue/suda.vim'
Plug 'junegunn/seoul256.vim'
Plug 'neovim/nvim-lsp'

" nice to have
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'jreybert/vimagit'
Plug 'Vigemus/iron.nvim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/vim-easy-align'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" filetype-specific
Plug 'Vimjas/vim-python-pep8-indent', { 'for': 'python' }
Plug 'psf/black', { 'for': 'python', 'tag': '*' }
Plug 'tmhedberg/SimpylFold', { 'for': 'python' }
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
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

let s:fzf_options = '--preview "bat --style numbers,changes --color=always --decorations=always {} | head -500"'

command! -bang -nargs=? -complete=dir Files
  \ call fzf#run(fzf#wrap(
  \   {
  \     'source': 'fd --type f .\* '.(empty(<q-args>) ? '' : shellescape(<q-args>)),
  \     'down': '40%',
  \     'options': s:fzf_options
  \   }, <bang>0))

command! -bang -nargs=? -complete=dir AllFiles
  \ call fzf#run(fzf#wrap(
  \   {
  \     'source': 'fd --type f --hidden --follow --exclude .git .\* '
  \       .(empty(<q-args>) ? '' : shellescape(<q-args>)),
  \     'down': '40%',
  \     'options': s:fzf_options
  \   }, <bang>0))

command! -bang -nargs=? -complete=dir Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case "^" '
  \     .(empty(<q-args>) ? '' : shellescape(<q-args>)),
  \   1, fzf#vim#with_preview(), <bang>0)

command! -bang -nargs=? -complete=dir RgAll
  \ call fzf#vim#grep(
  \   'rg -uu --column --line-number --no-heading --color=always --smart-case "^" '
  \     .(empty(<q-args>) ? '' : shellescape(<q-args>)),
  \   1, fzf#vim#with_preview(), <bang>0)

command! -bang -nargs=? -complete=dir GGrep
  \ call fzf#vim#grep(
  \   'git grep --line-number "^" -- '.shellescape(<q-args>), 0,
  \   fzf#vim#with_preview({ 'dir': s:find_git_root() }), <bang>0)

let s:filetype2jump_query = {
  \ 'markdown': '^# ',
  \ 'python': '^def\  | ^class\  ',
  \ 'rust': '^fn\  | ^type\  | ^struct\  | ^enum\  | ^union\  | ^const\  | ^static\  | ^trait\  | ^impl\  | ^pub\  ',
  \ }

function! s:jump_buffer_lines(bang)
  let jump_query = s:filetype2jump_query[&filetype]
  call fzf#vim#buffer_lines(jump_query, a:bang)
endfunction

command! -bang JumpBLines call s:jump_buffer_lines(<bang>0)

function s:list_mappings()
  let mappings = ""
  redir =>mappings
  silent map
  redir END
  return split(mappings, "\n")
endfunction

command! -bang Mappings
  \ call fzf#run(fzf#wrap(
  \   {
  \     'source': s:list_mappings(),
  \     'down': '40%'
  \   }, <bang>0))

"--------------------------------------------------------- Auto commands

" sane behavior when switching buffers -- leave my cursor where it is!
autocmd BufLeave * let b:winview = winsaveview()
autocmd BufEnter * if(exists('b:winview')) | call winrestview(b:winview) | endif

autocmd BufWritePost * call s:auto_chmod()

" comment syntax definitions not provided by vim-commentary
autocmd FileType sql setlocal commentstring=--\ %s
autocmd FileType cfg setlocal commentstring=#\ %s

augroup Python
  autocmd!
  autocmd BufWritePre *.py Black
  autocmd FileType python setlocal makeprg=pylint\ --output-format=parseable\ --score=no
  autocmd BufWritePost *.py silent make! <afile> | silent redraw!
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
" disable folds by default (toggle them with zi)
set nofoldenable
" default folding method (works e.g. with Rust)
set foldmethod=syntax
set mouse=a
if executable("rg")
  set grepprg=rg\ --vimgrep\ --no-heading
  set grepformat=%f:%l:%c:%m,%f:%l:%m
endif
" elflord is a nicely readable built-in one but seoul256 is better;
" another fairly nice one but harder on the eyes is
" liuchengxu/space-vim-dark
colorscheme seoul256
highlight Comment cterm=italic

let g:markdown_folding = 1
let g:markdown_fenced_languages = ['python', 'rust',
    \ 'sh', 'sql', 'r',
    \ 'html', 'css', 'javascript',
    \ 'perl', 'ruby', 'c', 'java', 'haskell', 'lisp', 'clojure',
    \ 'conf', 'diff', 'xml', 'systemd'
    \ ]

let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'
let g:airline#extensions#tabline#enabled = 1

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
noremap <leader><leader> :
" execute visual selection of Vimscript code
vnoremap <leader>x y \| :@"<CR>
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

noremap <leader>ff :Files<CR>
noremap <leader>fg :GFiles<CR>
noremap <leader>fa :AllFiles<CR>
noremap <leader>fr :History<CR>
noremap <leader>bb :Buffers<CR>
noremap <leader>bw :Windows<CR>
noremap <leader>sr :Rg<CR>
noremap <leader>sa :RgAll<CR>
noremap <leader>sg :GGrep<CR>
noremap <leader>ss :Lines<CR>
noremap <leader>sb :BLines<CR>
noremap <leader>sj :JumpBLines<CR>
noremap <leader>gg :Commits<CR>
noremap <leader>gb :BCommits<CR>
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
