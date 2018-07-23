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
Plug 'Vimjas/vim-python-pep8-indent'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/seoul256.vim'

Plug 'junegunn/fzf', { 'dir': '~/.local/fzf', 'do': './install --all && ln -s ../fzf/bin/fzf ../bin' }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'davidhalter/jedi-vim'
Plug 'ambv/black'
Plug 'rust-lang/rust.vim'
call plug#end()

"------------------------------ Functions ------------------------------

function! CleanupWhitespace()
  let save_cursor = getpos('.')
  silent %s/\s\+$//e
  silent %s/\($\n\s*\)\+\%$//e
  call setpos('.', save_cursor)
endfunction

" enhanced bro[wse] ol[dfiles]: allows searching & opening with Enter
" credit: https://stackoverflow.com/a/21938644/1826241
command! Browse :new +setl\ buftype=nofile | 0put =v:oldfiles | nnoremap <buffer> <CR> :e <C-r>=getline('.')<CR><CR>

" a quicker alternative which doesn't open a separate buffer
" credit: https://stackoverflow.com/a/22701319/1826241

" open file arg for editing
function! Mru(arg)
  execute 'edit ' . a:arg
endfunction

" completion function which searches the oldfiles list
function! MruComplete(ArgLead, CmdLine, CursorPos)
  return filter(copy(v:oldfiles), 'v:val =~ a:ArgLead')
endfunction

" Mru command: runs Mru function and uses MruComplete function for
" completion
command! -nargs=1 -complete=customlist,MruComplete Mru call Mru(<f-args>)

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
" 'liuchengxu/space-vim-dark'
colorscheme seoul256

"------------------------------ Key bindings ------------------------------

let mapleader = ' '
inoremap fd <Esc>

let g:jedi#completions_command = "<C-N>"

"------------------------------ Help ------------------------------

" # Python
"
" If using Neovim, don't forget to install the module which enables Python
" support:
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
