let mapleader = ' '
let maplocalleader = ' '
if executable('mamba')
  let g:python3_host_prog = fnamemodify('~/.local/mambaforge/bin/python', ':p')
elseif executable('pyenv')
  " don't use `pyenv which` here, because that can be overridden by `pyenv local`
  let g:python3_host_prog = trim(system('pyenv root'))
    \ . '/versions/'
    \ . trim(system('pyenv global'))
    \ . '/bin/python3'
else
  let g:python3_host_prog = exepath('python3')
endif

"--------------------------------------------------------------- Plugins {{{1

call plug#begin()
" TODO: try these out some day:
" https://github.com/bfredl/nvim-ipy
" https://github.com/bfredl/nvim-luadev
" https://github.com/mfussenegger/nvim-dap
" https://github.com/nvim-telescope/telescope-ui-select.nvim
" https://github.com/nvim-neorg/neorg
" https://github.com/nvim-orgmode/orgmode
" https://github.com/teal-language/vim-teal
"
" TODO: More tips on setting up Rust support in Neovim:
" https://sharksforarms.dev/posts/neovim-rust/

" core
Plug 'tpope/vim-sensible'
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-eunuch'
" `:w !sudo tee %` or `:SudoWrite` from vim-eunuch doesn't work in nvim:
" https://github.com/neovim/neovim/issues/8678
Plug 'lambdalisue/suda.vim'
" community-maintained configs for various langs
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/lsp-status.nvim'
Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
Plug 'nvim-treesitter/nvim-treesitter-refactor'
Plug 'nvim-treesitter/nvim-treesitter-textobjects'
Plug 'airblade/vim-gitgutter'

" fuzzy-finder = Telescope + deps
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
Plug 'nvim-telescope/telescope-file-browser.nvim'

" visuals
Plug 'https://gitlab.com/protesilaos/tempus-themes-vim.git'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" For quickly picking an Airline theme matching the colorscheme:
" Plug 'qdddddd/vim-cycle-airline-theme'

" nice to have
Plug 'liuchengxu/vim-which-key'
Plug 'jreybert/vimagit'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/vim-easy-align'
Plug 'nvim-treesitter/playground'
Plug 'SirVer/ultisnips'
Plug 'chrisbra/unicode.vim'

" completion
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'quangnguyen30192/cmp-nvim-ultisnips'

" filetype-specific
Plug 'psf/black', { 'for': 'python', 'tag': 'stable' }
" NOTE: (Neo)vim now ships with a Rust filetype plugin, which is typically an old(er)
" version of rust.vim. Since I'm using LSP for IDE features and Treesitter for
" highlighting, the only thing this plugin is good for is the commands it defines
" (Rust*). I'm not really using those right now, but they keep evolving and might be
" useful, so probably keep this plugin, to have access to up-to-date versions?
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
" NOTE: A dedicated LSP config plugin for Rust. Supports standalone files, but *not
" within a git repo*. Also, has to be loaded eagerly, otherwise additional setup in
" init.lua fails.
Plug 'simrat39/rust-tools.nvim'  " , { 'for': 'rust' }
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'dag/vim-fish', { 'for': 'fish' }
call plug#end()

"------------------------------------------------ Functions and commands {{{1

function! s:auto_chmod()
  let first_line = getline(1)
  if first_line =~ '^#!'
    silent !chmod +x %
  endif
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

function! s:update_everything()
  PlugUpgrade
  PlugSnapshot ~/.files/plug.lock
  PlugUpdate
  TSUpdate
endfunction

command! UpdateEverything :call s:update_everything()

" d2_ is what 2dd remaps to
command! -nargs=1 -complete=command Redir :enew | put =execute('<args>') | setlocal buftype=nofile bufhidden=hide noswapfile | normal! ggd2_

" aliases for when I accidentally keep Shift pressed after entering
" command mode
command! Q :q
command! W :w
command! X :x

"--------------------------------------------------------- Auto commands {{{1

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
  " autocmd BufWritePost *.py silent make! <afile> | silent redraw!
augroup END

" auto-open quickfix list if :make et al. yield errors
autocmd QuickFixCmdPost [^l]* cwindow

" automatically put help windows in a vert split
augroup vim_help
  autocmd!
  autocmd BufEnter *.txt if &buftype == 'help' | wincmd L | endif
augroup END

augroup highlight_yank
  autocmd!
  autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank({timeout = 300})
augroup END

autocmd VimLeave * set guicursor=a:ver25

"-------------------------------------------------------------- Settings {{{1

" allow hiding buffers with changes
set hidden
set timeoutlen=300
set updatetime=500
" don't redraw screen in the middle of a macro (faster)
set lazyredraw
set splitbelow splitright
" don't hard wrap when appending to line which is already longer than
" textwidth
set formatoptions+=l
set nojoinspaces
set list
set completeopt=menuone,noinsert,noselect
set pumheight=10
" shows ex command results preview as you type, only works with :s, :sm
" and :sno
" currently
set inccommand=split
" character triggering completion in macros
set wildcharm=<Tab>
" let find search in dir of active buffer and recursively under current
" working directory (works better without autochdir)
set path=.,**
set ignorecase wildignorecase smartcase
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

" Nice colorschemes:
" - elflord (:set notermguicolors) is a nicely readable built-in one
" - seoul256 (looks better with :set notermguicolors) is beautiful and I
"   used it for a long time, but it's low-contrast by design, which
"   makes it an eye-strain in low-light conditions
" - tempus_future (requires :set termguicolors) is a bit garish, but was
"   engineered to have high contrast, which is desirable
colorscheme tempus_future
set termguicolors
" To show currently defined highlight groups, run :highlight or
" :highlight GroupName.
" By default, tempus_future sets a fg color for listchars which is too
" bright, making it indistinguishable from regular text, which is
" confusing, so override it:
highlight! NonText guifg=#595271
highlight! Comment cterm=italic
" The following highlight groups weren't defined in seoul256, so I had
" to configure them manually. Keeping these definitions as a reference
" if I ever come across another theme with similar issues.
" readable background for whichkey floating windows
" highlight! link WhichKeyFloating NormalFloat
" highlight! link LspDiagnosticsDefaultError SpellBad
" highlight! link LspDiagnosticsDefaultWarning SpellRare
" highlight! link LspDiagnosticsDefaultInformation SpellCap
" highlight! link LspDiagnosticsDefaultHint SpellLocal
" highlight! link LspReferenceText Search
" highlight! link LspReferenceRead Search
" highlight! link LspReferenceWrite Search

" this fixes one of the most egregious issues with Vim's default Python
" indentation (= ugly and buggy indentation inside parentheses); it's
" not really necessary as I'll be using tree-sitter indentation going
" forward, but let's keep it in here for good measure, in case I ever
" need to fall back to the default indentation algorithm
let g:pyindent_disable_parentheses_indenting = 1
" I'm sick of fixing Black's virtualenv, let's just install it globally.
let g:black_virtualenv = fnamemodify(g:python3_host_prog, ':h:h')

function! LspStatus() abort
  let status = luaeval('require("lsp-status").status()')
  return trim(status)
endfunction
call airline#parts#define_function('lsp_status', 'LspStatus')
call airline#parts#define_condition('lsp_status', 'luaeval("#vim.lsp.buf_get_clients() > 0")')
let g:airline_powerline_fonts = 1
" Some nice Airline themes are: night_owl, monochrome, base16, zenburn,
" bubblegum, seoul256. But if you change your colorscheme, you might
" want to re-check all of them, one of the others might match it better.
" Use qdddddd/vim-cycle-airline-theme for that.
let g:airline_theme = 'night_owl'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#nvimlsp#enabled = 0
let g:airline_section_warning = airline#section#create_right(['lsp_status'])

" make sure *.tex files are never interpreted as ft=plaintex, so that
" vimtex is used
let g:tex_flavor = 'latex'
let g:vimtex_format_enabled = 1

let g:UltiSnipsSnippetStorageDirectoryForUltiSnipsEdit = '~/.files/snippets'
let g:UltiSnipsEditSplit = 'context'

"---------------------------------------------------------- Key bindings {{{1

inoremap fd <Esc>
" switch to normal mode in :terminal
tnoremap fd <C-\><C-n>
" avoid having to escape special characters in searches ("very magic")
noremap / /\v
noremap ? ?\v
noremap <Right> gt
noremap <Left> gT
noremap <silent> gO :call voom#Init(&filetype, 1, 1)<CR>
noremap <Down> :bn<CR>
noremap <Up> :bp<CR>
noremap <silent> zi :call ToggleFolds()<CR>
" make Y behave like D, C -> perform action from cursor to end of line
nnoremap Y y$
" keep cursor centered during various operations
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap J mzJ`z
" undo break points (useful especially when typing large amounts of
" prose in one go)
inoremap . .<C-g>u
inoremap , ,<C-g>u
inoremap ; ;<C-g>u
inoremap ! !<C-g>u
inoremap ? ?<C-g>u
inoremap " "<C-g>u
inoremap - -<C-g>u
inoremap ( (<C-g>u
inoremap ) )<C-g>u
" moving selections around while reindenting properly, without messing
" up registers (!)
vnoremap J :move '>+1<CR>gv=gv
vnoremap K :move '<-2<CR>gv=gv
" TODO: variations on the above for insert and normal mode, but I can't
" think of good bindings for those right now
" inoremap ??? <Esc>:move .+1<CR>==
" inoremap ??? <Esc>:move .-2<CR>==
" nnoremap ??? :move .+1<CR>==
" nnoremap ??? :move .-2<CR>==

" Telescope shortcuts; Telescope buffers also have additional key-bindings for
" navigating the lists, selecting items, sending them to quickfix (M-q / C-q) etc.
noremap <silent> <leader>ff :Telescope file_browser<CR>
noremap <silent> <leader>fp :Telescope git_files<CR>
noremap <silent> <leader>fr :Telescope oldfiles<CR>
noremap <silent> <leader>bb :Telescope buffers<CR>
noremap <silent> <leader>ss :Telescope current_buffer_fuzzy_find<CR>
noremap <silent> <leader>sd :Telescope live_grep<CR>
noremap <silent> <leader>sj :Telescope jumplist<CR>
noremap <silent> <leader>tt :Telescope builtin<CR>
noremap <silent> <leader>tr :Telescope resume<CR>
noremap <silent> <leader>t: :Telescope command_history<CR>
noremap <silent> <leader>t/ :Telescope search_history<CR>
noremap <silent> <leader>t" :Telescope registers<CR>
noremap <silent> <leader>tc :Telescope colorscheme<CR>

nnoremap <silent> <leader> :<C-u>WhichKey '<Space>'<CR>
noremap <leader><leader> :
" execute visual selection of Vimscript code
vnoremap <leader>x y \| :@"<CR>
nnoremap <leader>pp :lua print(vim.inspect())<Left><Left>
" rr stands for redirect (from pager to buffer), even though with recent
" versions of (Neo)vim, there's a simpler way than using redir
nnoremap <leader>rr :Redir<Space>
noremap <leader><Tab> <C-^>
noremap <leader>w <C-w>
" enable command history filtering for Ctrl-P and Ctrl-N
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
" without autochdir, this is useful to quickly complete the path of the
" active buffer
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" and these are for fuzzy-finding files anywhere in the directory tree
cnoremap <expr> ** getcmdtype() == ':' ? '**/*' : '**'
cnoremap <expr> *** getcmdtype() == ':' ? '**/.*' : '***'
cnoremap <expr> ~~ getcmdtype() == ':' ? '~/' : '~~'

noremap <leader>dg :diffget \| diffupdate<CR>
noremap <leader>dp :diffput \| diffupdate<CR>
noremap <leader>ll :Goyo \| Limelight!!<CR>
noremap <leader>z "=ZoteroCite()<CR>p
inoremap <C-z> <C-r>=ZoteroCite()<CR>
" Fix spelling mapping from https://castel.dev/post/lecture-notes-1/
" (there are lots of good tips there, go back for more at some point).
" <C-g>u groups actions into one undo step; [s jumps to previous
" spelling mistake, 1z= applies the first suggestion from the list of
" fixes, `] jumps to the end of the last edit
inoremap <C-l> <C-g>u<Esc>[s1z=`]a<C-g>u

" Start LiveEasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(LiveEasyAlign)
" Start LiveEasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(LiveEasyAlign)

"-------------------------------------------------------------- Lua init {{{1

lua init = require("init")

" vi: foldmethod=marker
