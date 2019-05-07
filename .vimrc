let mapleader = ' '

"------------------------------ Plugins ------------------------------

" first time setup
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !pip3 install --user --upgrade --upgrade-strategy eager pynvim python-language-server
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" :PlugInstall to install plugins
" :PlugUpdate to update plugins
" :PlugUpgrade to update vim-plug itself
call plug#begin()
" core
Plug 'tpope/vim-sensible'
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/seoul256.vim'

" nice to have
Plug 'junegunn/fzf', { 'dir': '~/.local/fzf', 'do': './install --all && ln -s ../fzf/bin/fzf ../bin' }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/vim-easy-align'

" ncm2 targets nvim, it seems it can be made to work on Vim 8, but I
" might as well just install nvim on any machine where I work often
" enough to justify setting up completion
if has('nvim')
  " core
  " `:w !sudo tee %` doesn't work in nvim: https://github.com/neovim/neovim/issues/8678
  Plug 'lambdalisue/suda.vim'

  " completion
  Plug 'roxma/nvim-yarp'
  Plug 'ncm2/ncm2'
  Plug 'ncm2/ncm2-bufword'
  Plug 'ncm2/ncm2-path'

  " LSP
  Plug 'autozimu/LanguageClient-neovim', {
      \ 'branch': 'next',
      \ 'do': 'bash install.sh',
      \ }
endif

" filetype-specific
Plug 'Vimjas/vim-python-pep8-indent', { 'for': 'python' }
Plug 'ambv/black', { 'for': 'python' }
Plug 'tmhedberg/SimpylFold', { 'for': 'python' }
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'tpope/vim-markdown', { 'for': 'markdown' }
call plug#end()

"------------------------------ Functions and commands ------------------------------

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

let s:filetype2jump_query = {
  \ 'markdown': '^# ',
  \ 'python': '^def\  | ^class\  ',
  \ 'rust': '^fn\  | ^type\  | ^struct\  | ^enum\  | ^union\  | ^const\  | ^static\  | ^trait\  | ^impl\  | ^pub\  ',
  \ }

function! s:jump_buffer_lines(bang)
  let jump_query = s:filetype2jump_query[&filetype]
  call fzf#vim#buffer_lines(jump_query, a:bang)
endfunction

function! ZoteroCite()
  " (La)TeX default: citep is parenthetical, citet is textual, and there
  " are other variations for omitting parts of the citation (e.g.
  " citeyearpar is only the year in parens, for when the text already
  " contains the author's name)
  let format = &filetype =~ '.*tex' ? 'citep' : 'pandoc'
  let api_call = 'http://localhost:23119/better-bibtex/cayw?format='.format.'&brackets=1'
  let ref = system('curl -s '.shellescape(api_call))
  return ref
endfunction

let s:fzf_options = '--preview "head -500 {}"'

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

command! -bang JumpBLines call s:jump_buffer_lines(<bang>0)

"------------------------------ Auto commands ------------------------------

" general template for external commands:
" autocmd BufWritePre *.py silent %!black -q -
" autocmd BufWritePre * call s:cleanup_whitespace()
autocmd BufWritePre *.py :Black
autocmd BufWritePre *.rs :RustFmt
autocmd BufWritePost * call s:auto_chmod()

let s:completeopt_default = 'longest,menuone'
if has('nvim')
  " enable completion in all buffers
  autocmd BufEnter * call ncm2#enable_for_buffer()
  " cf. :help Ncm2PopupOpen
  autocmd User Ncm2PopupOpen set completeopt=noinsert,menuone,noselect
  autocmd User Ncm2PopupClose let &completeopt=s:completeopt_default
endif

" comment syntax definitions not provided by vim-commentary
autocmd FileType sql setlocal commentstring=--\ %s
autocmd FileType cfg setlocal commentstring=#\ %s

"------------------------------ Settings ------------------------------

" allow hiding buffers with changes
set hidden
set timeoutlen=500
set updatetime=500
set splitbelow splitright
" make sure vim knows there are 256 colors
set t_Co=256
" don't hard wrap when appending to line which is already longer than
" textwidth
set formatoptions+=l
set list
" more ergonomic completion behavior
let &completeopt=s:completeopt_default
" character triggering completion in macros
set wildcharm=<Tab>
" let find search in dir of active buffer and recursively under current
" working directory (works better without autochdir)
set path=.,**
set ignorecase smartcase
" persistent undo instead of backups, and swap files tucked away please
set nobackup
set undofile
if !isdirectory($HOME . "/.vim/undo")
  call mkdir($HOME . "/.vim/undo", "p", 0700)
endif
set undodir=~/.vim/undo
if !isdirectory($HOME . "/.vim/swp")
  call mkdir($HOME . "/.vim/swp", "p", 0700)
endif
set directory=~/.vim/swp
" allow modelines (some distros disable them)
set modeline
set modelines=5
" disable folds by default (toggle them with zi)
set nofoldenable
" default folding method (works e.g. with Rust)
set foldmethod=syntax
" elflord is a nicely readable built-in one but seoul256 is better;
" another fairly nice one but harder on the eyes is
" liuchengxu/space-vim-dark
colorscheme seoul256

let g:markdown_folding = 1
let g:markdown_fenced_languages = ['python', 'rust',
    \ 'sh', 'sql', 'r',
    \ 'html', 'css', 'javascript',
    \ 'perl', 'ruby', 'c', 'java', 'haskell', 'lisp', 'clojure',
    \ 'conf', 'xml', 'systemd'
    \ ]

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'stable', 'rls'],
    \ 'python': ['pyls'],
    \ }

"------------------------------ Key bindings ------------------------------

inoremap fd <Esc>
" avoid having to escape special characters in searches ("very magic")
noremap / /\v
noremap <Right> gt
noremap <Left> gT
noremap <Down> :bn<CR>
noremap <Up> :bp<CR>
noremap <leader><leader> :
" execute visual selection of Vimscript code
vnoremap <leader>x y \| :@"<CR>
noremap <leader><Tab> <C-^>
noremap <leader>w <C-w>
" enable command history filtering for Ctrl-P and Ctrl-N
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
" without autochdir, this is useful to quickly complete the path of the
" active buffer
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

nnoremap <silent> gh :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

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
noremap <leader>l :Goyo \| Limelight!!<CR>
noremap <leader>z "=ZoteroCite()<CR>p
inoremap <C-z> <C-r>=ZoteroCite()<CR>

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
