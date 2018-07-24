"------------------------------ Plugins ------------------------------

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !pip3 install --user neovim
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" :PlugInstall to install plugins
" :PlugUpdate to update plugins
" :PlugUpgrade to update vim-plug itself
call plug#begin()
Plug 'tpope/vim-sensible'
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/seoul256.vim'

Plug 'junegunn/fzf', { 'dir': '~/.local/fzf', 'do': './install --all && ln -s ../fzf/bin/fzf ../bin' }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'

Plug 'Vimjas/vim-python-pep8-indent', { 'for': 'python' }
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
Plug 'ambv/black', { 'for': 'python' }
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
call plug#end()

"------------------------------ Functions and commands ------------------------------

function! CleanupWhitespace()
  let save_cursor = getpos('.')
  silent %s/\s\+$//e
  silent %s/\($\n\s*\)\+\%$//e
  call setpos('.', save_cursor)
endfunction

command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>, fzf#vim#with_preview(), <bang>0)

function! s:find_git_root()
  return systemlist('git rev-parse --show-toplevel')[0]
endfunction

command! -bang -nargs=* GitGrep
  \ call fzf#vim#grep(
  \   'git grep --line-number '.shellescape(<q-args>), 0,
  \   { 'dir': s:find_git_root() }, <bang>0)

command! ProjectFiles execute 'Files' s:find_git_root()

"------------------------------ Auto commands ------------------------------

" general template for external commands:
" autocmd BufWritePre *.py silent %!black -q -
autocmd BufWritePre * call CleanupWhitespace()
autocmd BufWritePre *.py :Black

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
" more ergonomic completion behavior
set completeopt=longest,menuone
" character triggering completion in macros
set wildcharm=<Tab>
set autochdir
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
" elflord is a nicely readable default one but seoul256 is better;
" another fairly nice one but harder on the eyes is
" liuchengxu/space-vim-dark
colorscheme seoul256

"------------------------------ Key bindings ------------------------------

let mapleader = ' '
inoremap fd <Esc>
" avoid having to escape special characters in searches ("very magic")
nnoremap / /\v
nnoremap <leader><Tab> <C-^>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fr :History<CR>
nnoremap <leader>fp :ProjectFiles<CR>
nnoremap <leader>fg :GitFiles<CR>
nnoremap <leader>bb :Buffers<CR>
nnoremap <leader>bw :Windows<CR>
nnoremap <leader>sa :Ag<CR>
nnoremap <leader>sg :GitGrep<CR>
nnoremap <leader>ss :Lines<CR>
nnoremap <leader>gg :Commits<CR>
nnoremap <leader>gb :BCommits<CR>

let g:jedi#completions_command = "<C-N>"

"------------------------------ Help ------------------------------

" # Python
"
" If using Neovim, don't forget to install the module which enables
" Python support:
"
" pip3 install --user neovim
"
" By default, jedi-vim uses the most recent Python version found on the
" system. To override this (even at runtime), do the following:
"
" let g:jedi#force_py_version = 2
"
" If you want to avoid having to tinker with that, simply start Vim in a
" virtualenv.

" # Tables
"
" If it ever turns out I need to edit tables in Vim, this looks like a
" good mode: <https://github.com/dhruvasagar/vim-table-mode>.

" # Mouse
"
" Mouse support (selection, window resizing) currently only works in
" Vim, not Neovim.

" # Scrolling around
"
" H   move to top of screen
" M   move to middle of screen
" L   move to bottom of screen
"
" zz  scroll the line with the cursor to the center of the screen
" zt  scroll the line with the cursor to the top
" zb  scroll the line with the cursor to the bottom
"
" Ctrl-D  move half-page down
" Ctrl-U  move half-page up
" Ctrl-B  page up
" Ctrl-F  page down
" Ctrl-O  jump to last (older) cursor position
" Ctrl-I  jump to next cursor position (after Ctrl-O)
" Ctrl-Y  move view pane up
" Ctrl-E  move view pane down

" # File system navigation
"
" When using :e to open files, navigating the file system can be a
" little unintuitive. Here are some hints:
"
" - Ctrl-D shows a suggestion list without completing anything
" - up and down keys navigate directories in case what you typed matches
"   multiple suggestions
" - you can always tweak how suggestions work by modifying the wildmode
"   variable, but be careful, it also affects how commands are
"   completed; e.g. set wildmode=longest:full,full will only complete
"   the longest match first and show suggestions for the rest, which is
"   what you want for files, but for commands, it's generally more
"   useful to complete the first full one, as it's probably the right
"   one
" - don't forget also Ex, Sex, Vex and Tex for opening files
