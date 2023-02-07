--[[

See:

- `:help lua-guide` for pointers on Lua and Lua in Neovim, and the different parts of
  the API available through `vim.*`
- https://github.com/nvim-lua/kickstart.nvim/compare/4916072854d01d0503821b7f3061daeb381f0441..master
  for new additions to the config that this one is based on
- https://github.com/nvim-lua/kickstart.nvim/blob/master/README.md for helpful tips e.g.
  on managing multi-file configs

In particular, Lua files intended for:

- requiring (the equivalent of VimL's autoload mechanism) should go under lua/
- early eager loading should go under plugin/
- late eager loading (overriding anything else) should go under after/

--]]

local api = vim.api
-- TODO: Ideally, you'd `setglobal` option values, so that sourcing this init file
-- doesn't override any existing local ones (e.g. set by the modeline or manually).
-- However, this currently doesn't quite work because of how `:help startup` works (see
-- https://github.com/neovim/neovim/issues/21668 for details). So for now, use regular
-- `set` and mitigate the sourcing problem at least for modeline option tweaks (if not
-- for the manual ones) by adding `doautocmd BufRead`, which reloads the modeline.
-- local o = vim.go
-- local opt = vim.opt_global
local o = vim.o
local opt = vim.opt
-- See `:help vim.keymap.set()`, which is a more flexible Lua-only interface compared to
-- the stable API's `:help nvim_set_keymap()`. See `:help map-table` for which mode
-- short name (~ map command prefix) applies the mapping to which mode(s). Quick hint:
-- "" is for map, "!" is for map!, and you can specify multiple ones in a table.
local map = vim.keymap.set

--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Use correct Python executable as provider.
local conda = vim.env.CONDA_EXE
vim.g.python3_host_prog = conda == nil and vim.fn.exepath("python3")
  or vim.fn.fnamemodify(conda, ":h:h") .. "/envs/umrk/bin/python"
-- Disable other providers.
vim.g.loaded_ruby_provider = 0
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0

-- stylua: ignore start
local ts_langs = {
  -- NOTE: Don't use Treesitter for Perl. Parsing Perl is a tall order, and what
  -- Treesitter can't parse, it won't highlight. In practice, this often results in
  -- only partially highlighted files, up to the first unexpected Perl "feature".
  -- Everything...
  "bash", "bibtex", "c", "comment", "cpp", "css", "dockerfile", "elm", "fish", "go",
  "help", "html", "java", "javascript", "json", "latex", "lua", "make", "markdown",
  "markdown_inline", "php", "python", "r", "regex", "rst", "ruby", "rust", "toml",
  "typescript", "vim", "yaml", "zig",
  -- ... and the kitchen sink.
  "clojure", "cmake", "commonlisp", "dot", "fennel", "fortran", "gomod", "gowork",
  "haskell", "hjson", "http", "jsdoc", "json5", "jsonc", "julia", "kotlin", "llvm",
  "ninja", "nix", "ocaml", "org", "pascal", "query", "scala", "scheme", "scss",
  "supercollider", "svelte", "tlaplus", "tsx", "vala", "vue",
}
-- stylua: ignore end

--------------------------------------------------------------------- Package management {{{1

local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
local is_bootstrap = false
if vim.fn.isdirectory(install_path) == 0 then
  is_bootstrap = true
  vim.fn.system { "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }
  vim.cmd("packadd packer.nvim")
end

-- NOTE: Careful with run = ... options in Packer: using a plugin command is prone to
-- fail during first bootstrap, because the plugin is not available at first, so the
-- command doesn't get loaded. Prepending `packloadall |` might help, to force reloading
-- packages before running the command. For an example and details, see
-- https://github.com/nvim-treesitter/nvim-treesitter/issues/3135#issuecomment-1178792955.
require("packer").startup(function(use)
  use("https://github.com/wbthomason/packer.nvim")

  -- Core functionality extensions
  -- TODO: Remove if https://github.com/neovim/neovim/issues/16339 gets implemented.
  use {
    "https://github.com/ethanholz/nvim-lastplace",
    config = function()
      require("nvim-lastplace").setup()
    end,
  }
  use {
    "https://github.com/numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
    end,
  }

  use("https://github.com/tpope/vim-surround")
  use("https://github.com/tpope/vim-repeat")
  use {
    "https://github.com/tpope/vim-eunuch",
    config = function()
      vim.g.eunuch_chmod_shebang = true
    end,
  }
  -- `:w !sudo tee %` or `:SudoWrite` from vim-eunuch doesn't work in nvim:
  -- https://github.com/neovim/neovim/issues/1496
  -- https://github.com/tpope/vim-eunuch/issues/56
  -- Instead, use Suda{Read,Write}, or the smart edit option, from:
  use {
    "https://github.com/lambdalisue/suda.vim",
    config = function()
      vim.g.suda_smart_edit = true
    end,
  }
  use("https://github.com/chrisbra/unicode.vim")
  use {
    "https://github.com/TimUntersberger/neogit",
    requires = { "https://github.com/nvim-lua/plenary.nvim" },
    config = function()
      require("neogit").setup()
    end,
  }

  -- UI
  use {
    "https://github.com/rebelot/kanagawa.nvim",
    config = function()
      local c = require("kanagawa.color")
      local colors = require("kanagawa.colors").setup()
      if not DlukesKanagawaColors then
        -- Only define this once as a global, otherwise the background switch below will
        -- keep toggling back and forth with each sourcing of the init file.
        DlukesKanagawaColors = {
          -- Switch backgrounds for dimInactive (i.e., make active darker and inactive
          -- lighter).
          sumiInk1 = colors.sumiInk1b,
          sumiInk1b = colors.sumiInk1,
        }
      end
      require("kanagawa").setup {
        globalStatus = true,
        dimInactive = true,
        colors = DlukesKanagawaColors,
        overrides = {
          -- Switch colors for cursorline and colorcolumn: cursorline bg being more
          -- understated makes the fg more readable.
          CursorLine = { bg = colors.sumiInk2 },
          ColorColumn = { bg = colors.sumiInk3 },
          Comment = { fg = tostring(c(colors.fujiGray):lighten(1.33)) },
        },
      }
      vim.cmd.colorscheme("kanagawa")
    end,
  }
  use {
    "https://github.com/nvim-lualine/lualine.nvim",
    requires = { "https://github.com/kyazdani42/nvim-web-devicons", opt = true },
    config = function()
      require("lualine").setup()
    end,
  }
  use("https://github.com/lewis6991/gitsigns.nvim")
  use("https://github.com/dstein64/nvim-scrollview")
  use {
    "https://github.com/folke/which-key.nvim",
    config = function()
      -- NOTE: which-key can also be used to set up (nested) keymaps in a convenient
      -- declarative way, see the register function.
      require("which-key").setup {
        triggers_blacklist = {
          i = { "f", "d" },
          v = { "f", "d" },
        },
      }
    end,
  }
  use {
    "https://github.com/norcalli/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup {
        "elm",
        "css",
        "html",
        "javascript",
      }
    end,
  }

  -- Fuzzy find all the things = Telescope + deps
  use {
    "https://github.com/nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    requires = { "https://github.com/nvim-lua/plenary.nvim" },
  }
  use { "https://github.com/nvim-telescope/telescope-fzf-native.nvim", run = "make" }
  use { "https://github.com/nvim-telescope/telescope-file-browser.nvim" }

  -- LSP, completion, snippets
  use {
    "https://github.com/neovim/nvim-lspconfig",
    requires = {
      "https://github.com/j-hui/fidget.nvim",
      "https://github.com/folke/neodev.nvim",
    },
  }
  use {
    "https://github.com/hrsh7th/nvim-cmp",
    requires = {
      "https://github.com/hrsh7th/cmp-path",
      "https://github.com/hrsh7th/cmp-nvim-lsp",
      "https://github.com/quangnguyen30192/cmp-nvim-ultisnips",
    },
  }
  -- Possibly migrate to https://github.com/L3MON4D3/LuaSnip for snippets? There's a
  -- guide at https://evesdropper.dev/files/luasnip/, but frankly, from what I can see,
  -- the snippet authoring UX leaves a lot to be desired compared to UltiSnips... OTOH,
  -- it has nicer integration with nvim-cmp (see comments in completion config below).
  use {
    "https://github.com/SirVer/ultisnips",
    config = function()
      vim.g.UltiSnipsSnippetStorageDirectoryForUltiSnipsEdit = "~/.files/snippets"
      vim.g.UltiSnipsEditSplit = "context"
    end,
  }

  -- Treesitter
  use {
    "https://github.com/nvim-treesitter/nvim-treesitter",
    run = function()
      require("nvim-treesitter.install").update()(ts_langs)
    end,
  }
  use {
    "https://github.com/nvim-treesitter/nvim-treesitter-textobjects",
    after = "nvim-treesitter",
  }
  use {
    "https://github.com/nvim-treesitter/nvim-treesitter-refactor",
    after = "nvim-treesitter",
  }
  -- use 'nvim-treesitter/playground'

  -- Filetype specific
  use {
    "https://github.com/psf/black",
    ft = "python",
    tag = "stable",
    config = function()
      -- I'm sick of fixing Black's virtualenv, let's just use a global install.
      vim.g.black_virtualenv = vim.fn.fnamemodify(vim.g.python3_host_prog, ":h:h")
    end,
  }
  -- (Neo)vim now ships with a Rust filetype plugin, which is typically an old(er)
  -- version of rust.vim. Since I'm using LSP for IDE features and Treesitter for
  -- highlighting, the only thing this plugin is good for is the commands it defines
  -- (Rust*). I'm not really using those right now, but they keep evolving and might be
  -- useful, so probably keep this plugin, to have access to up-to-date versions?
  use { "https://github.com/rust-lang/rust.vim", ft = "rust" }
  -- A dedicated LSP config plugin for Rust. Supports standalone files, but *not within
  -- a git repo*. Also, has to be loaded eagerly (i.e. not via `ft = 'rust'`), otherwise
  -- additional setup below fails.
  use("https://github.com/simrat39/rust-tools.nvim")
  use { "https://github.com/dag/vim-fish", ft = "fish" }

  if is_bootstrap or vim.env.PACKER_SYNC then
    require("packer").sync()
  end
end)

if is_bootstrap then
  return
end

-------------------------------------------------------------------------------- Options {{{1

o.mouse = "a"
o.timeoutlen = 300
o.updatetime = 500
-- Don't redraw screen in the middle of a macro (faster).
o.lazyredraw = true

-- Don't hard wrap when appending to line which is already longer than
-- textwidth.
opt.formatoptions:append("l")
o.breakindent = true
o.joinspaces = false

o.ignorecase = true
o.wildignorecase = true
o.smartcase = true

-- For `:help ins-completion`.
o.completeopt = "menuone,noinsert,noselect,preview"
-- For `:help cmdline-completion`.
o.wildmode = "longest:full:lastused,full"
o.pumheight = 10
-- Let find search in dir of active buffer and recursively under current working
-- directory (works better without autochdir).
o.path = ".,**"

-- Let's play it safe and have both persistent undo and backups. Undo files are tucked
-- away in `set undodir?`, swap files in `set directory?`.
o.undofile = true
o.backup = true
-- Never store backup files in current directory.
opt.backupdir:remove(".")

o.splitbelow = true
o.splitright = true
o.termguicolors = true
-- Hide cmdline when not using it (TODO: enable once teething pains are resolved,
-- possibly alongside https://github.com/folke/noice.nvim once it has gotten off the
-- bleeding edge too).
-- o.cmdheight = 0
o.cursorline = true
o.colorcolumn = "+1"
o.list = true
-- Always showing sign column adds room from screen edge and prevents horizontal
-- shifting when signs are shown/hidden.
o.signcolumn = "yes"
-- Just one global statusline...
o.laststatus = 3
-- ... so display modified info + filename at top right
o.winbar = [[%=%m %f]]
-- Shows ex command results preview as you type, only works with :s, :sm and
-- :sno currently.
o.inccommand = "split"
-- Better understanding of same-line changes in diffs, see
-- https://github.com/neovim/neovim/pull/14537.
opt.diffopt:append("linematch:60")
opt.fillchars:append {
  horiz = "━",
  horizup = "┻",
  horizdown = "┳",
  vert = "┃",
  vertleft = "┨",
  vertright = "┣",
  verthoriz = "╋",
}

-- Disable folds by default (toggle them with zi).
o.foldenable = false
o.foldmethod = "expr"
o.foldexpr = "nvim_treesitter#foldexpr()"

if vim.fn.executable("rg") == 1 then
  o.grepprg = "rg --vimgrep --no-heading"
  o.grepformat = [[%f:%l:%c:%m,%f:%l:%m]]
end

-- The default indentation for Python is a bit WTF, but fortunately, it's configurable,
-- see `:help ft-python-indent`. Once Treesitter indentation for Python becomes usable,
-- enable it instead, but probably keep this config too, in case I ever need to fall
-- back to the default indentation algorithm.
vim.g.python_indent = {
  open_paren = "shiftwidth()",
  continue = "shiftwidth()",
  closed_paren_align_last_line = false,
}

-- Make sure *.tex files are never interpreted as ft=plaintex.
vim.g.tex_flavor = "latex"

------------------------------------------------------------------------------ Telescope {{{1

-- See `:help telescope`.
local telescope = require("telescope")
local tactions = require("telescope.actions")
-- See `:help telescope.builtin`.
local tbuiltin = require("telescope.builtin")
-- See `:help telescope.resolve.resolve_width()`.
local twidth = 90

local tpickers = {}
for picker_name, _ in pairs(tbuiltin) do
  tpickers[picker_name] = { theme = "dropdown", layout_config = { width = twidth } }
end

-- See `:help telescope.setup()`.
telescope.setup {
  defaults = {
    mappings = {
      i = {
        ["<esc>"] = tactions.close,
      },
    },
  },
  pickers = tpickers,
  extensions = {
    file_browser = {
      -- See `:help telescope-file-browser.picker.file_browser()` for available opts.
      theme = "dropdown",
      layout_config = { width = twidth },
      -- Group directories first. This makes file_browser use plenary.scandir instead of
      -- fd, which has the advantage that you don't have to install fd. Also, I've
      -- experienced problems caused apparently by an obsolete version of fd (spuriously
      -- empty file list, except for parent dir), so better avoid the coupling. Or maybe
      -- it was caused by fd being slow to execute, which interacted badly with async?
      -- At any rate -- the main purpose of using fd is to get fast file listings while
      -- respecting .gitignore files, which I specifically *don't* want, I want to be
      -- able to access any file from file_browser.
      grouped = true,
      hidden = true,
      respect_gitignore = false,
    },
  },
}
telescope.load_extension("fzf")
telescope.load_extension("file_browser")

-- Telescope shortcuts; Telescope buffers also have additional key-bindings for
-- navigating the lists, selecting items, sending them to quickfix (M-q / C-q) etc. You
-- can show all available mappings from within a picker in insert mode with C-/.
for _, map_def in pairs {
  { "bb", "buffers", "Switch buffer" },
  { "hh", "help_tags", "Help, help tags!" },
  -- Instead of builtin find_files, which doesn't allow switching directories.
  { "ff", "file_browser", "Find files with browser" },
  { "fp", "git_files", "Find files in Git project" },
  { "fr", "oldfiles", "Find recent files" },
  { "ss", "current_buffer_fuzzy_find", "Fuzzy search this buffer" },
  { "sw", "grep_string", "Search directory for current word" },
  { "sd", "live_grep", "Search directory" },
  { "sj", "jumplist", "Search jumplist" },
  { "tb", "builtin", "Telescope builtin pickers" },
  { "tr", "resume", "Telescope resume" },
  { "t:", "command_history", "Telescope command history" },
  { "t/", "search_history", "Telescope search history" },
  { [[t"]], "registers", "Telescope registers" },
  { "tc", "colorscheme", "Telescope colorschemes" },
  { "tk", "keymaps", "Telescope keymaps" },
} do
  local keys, pick, desc = unpack(map_def)
  map("", "<leader>" .. keys, tbuiltin[pick] or telescope.extensions[pick][pick], { desc = desc })
end

----------------------------------------------------------------------------- Treesitter {{{1

-- See `:help nvim-treesitter`.
local ts = require("nvim-treesitter.configs")

ts.setup {
  ensure_installed = ts_langs,
  highlight = { enable = true },
  -- TODO: Labeled as experimental, but kickstart.lua enables it, except for Python, see
  -- https://github.com/nvim-lua/kickstart.nvim/issues/78. Periodically check if it's
  -- gotten better and if so, enable even for Python (and get rid of your
  -- ft-python-indent config).
  indent = { enable = true, disable = { "python" } },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      -- These only become active after init_selection has been started.
      node_incremental = "n",
      node_decremental = "N",
      scope_incremental = "s",
    },
  },
  refactor = {
    highlight_definitions = { enable = true },
    -- nice in theory but unfortunately sort of ugly because the highlight is ragged;
    -- cf. :help syn-pattern: "The highlighted area will never be outside of the matched
    -- text."
    -- highlight_current_scope = { enable = true },
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        -- Use capture groups from textobjects.scm or define your own. TODO: The comment
        -- textobject is currently not that useful, pending
        -- <https://github.com/nvim-treesitter/nvim-treesitter-textobjects/issues/133>.
        ["ac"] = "@comment.outer",
        ["aa"] = "@parameter.outer",
        ["ia"] = "@parameter.inner",
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["aC"] = "@class.outer",
        ["iC"] = "@class.inner",
      },
    },
    move = {
      enable = true,
      set_jumps = true,
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ["<leader>a"] = "@parameter.inner",
      },
      swap_previous = {
        ["<leader>A"] = "@parameter.inner",
      },
    },
  },
}

------------------------------------------------------------------------------------ LSP {{{1
-- Logging/debugging: "trace", "debug", "info", "warn", "error"
--
--   vim.lsp.set_log_level("info")
--   print(vim.lsp.get_log_path())
--
-- Check server capabilities:
--
--   vim.lsp.get_active_clients()[1].server_capabilities

-- NOTE: Neodev must come before lspconfig.
require("neodev").setup()
local lspconfig = require("lspconfig")

local lsp_group = api.nvim_create_augroup("lsp_buffer_local", {})
local on_attach = function(client, bufnr)
  -- This is where you can selectively disable some capabilities if they're broken or
  -- annoying, e.g.:
  --
  --   client.server_capabilities.documentFormattingProvider = false
  --   client.server_capabilities.documentRangeFormattingProvider = false

  vim.api.nvim_clear_autocmds { group = lsp_group, buffer = bufnr }

  if client.server_capabilities.documentRangeFormattingProvider then
    -- Set formatexpr for gq operator.
    vim.bo[bufnr].formatexpr = "v:lua.vim.lsp.formatexpr(#{timeout_ms:3000})"
  end

  if client.server_capabilities.documentFormattingProvider then
    -- Set up autoformat on save, except for filetypes which have a custom formatting
    -- autocommand. NOTE: If the buffer is open in multiple windows, this unfortunately
    -- resets the cursor to the top of the file in all views but the focused one. See:
    -- https://github.com/neovim/neovim/issues/14645
    local exclude_fts = { python = true, lua = true }
    local autoformat_on_save = true
    for _, ft in pairs(client.config.filetypes) do
      if exclude_fts[ft] then
        autoformat_on_save = false
        break
      end
    end
    if autoformat_on_save then
      api.nvim_create_autocmd("BufWritePre", {
        callback = function()
          vim.lsp.buf.format { async = false, timeout_ms = 3000 }
        end,
        buffer = bufnr,
        group = lsp_group,
      })
    end
  end

  local function nmap(rhs, lhs, desc)
    if desc then
      desc = "LSP: " .. desc
    end
    map("n", rhs, lhs, { buffer = bufnr, desc = desc })
  end

  -- Cf. `:help lsp-config` for tips on what to map.
  nmap("<leader>lr", vim.lsp.buf.rename, "Rename")
  nmap("<leader>la", vim.lsp.buf.code_action, "Code action")
  nmap("<leader>ls", require("telescope.builtin").lsp_document_symbols, "Document symbols")
  nmap("<leader>lS", require("telescope.builtin").lsp_dynamic_workspace_symbols, "Workspace symbols")

  -- NOTE: Diagnostics can in theory be set by other things than LSP, so these mappings
  -- might be relevant beyond buffers with LSP enabled.
  nmap("[d", function()
    vim.diagnostic.goto_prev { wrap = false }
  end)
  nmap("]d", function()
    vim.diagnostic.goto_next { wrap = false }
  end)
  nmap("<leader>ld", function()
    vim.diagnostic.open_float { scope = "cursor" }
  end)
  -- Alternative: Telescope's builtin diagnostics picker.
  nmap("<leader>lD", vim.diagnostic.setloclist)

  nmap("gd", vim.lsp.buf.definition, "Go to definition")
  -- Alternative: vim.lsp.buf.references() and copen.
  nmap("gr", require("telescope.builtin").lsp_references, "Go to references")
  nmap("gI", vim.lsp.buf.implementation, "Go to implementation")
  nmap("gt", vim.lsp.buf.type_definition, "Go to type definition")
  nmap("gD", vim.lsp.buf.declaration, "Go to declaration")

  -- See `:help K` for why this keymap.
  nmap("K", vim.lsp.buf.hover, "Hover Documentation")
  nmap("<C-k>", vim.lsp.buf.signature_help, "Signature Documentation")
end

local servers = {
  -- Rust support is handled by a separate plugin, see below.
  -- Full reference of config values:
  -- https://github.com/sumneko/lua-language-server/blob/master/locale/en-us/setting.lua
  sumneko_lua = {
    settings = {
      Lua = {
        workspace = { checkThirdParty = false },
        telemetry = { enable = false },
        -- TODO: https://github.com/sumneko/lua-language-server/issues/1809
        semantic = { enable = false },
        -- Additional setup to enable a nice development experience for Lua in Neovim
        -- is handled by the folke/neodev plugin.
      },
    },
  },
  -- jedi_language_server = {},
  pyright = {
    settings = {
      python = {
        analysis = {
          -- See https://github.com/microsoft/pyright/blob/main/docs/configuration.md.
          extraPaths = {
            vim.env.HOME .. "/.files/python/typings",
            vim.env.HOME .. "/.local/share/python-type-stubs",
          },
        },
      },
    },
  },
  r_language_server = {},
  bashls = {},
  elmls = {},
  vimls = {},
  tsserver = {},
}

local capabilities = require("cmp_nvim_lsp").default_capabilities()
for ls, config in pairs(servers) do
  lspconfig[ls].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    settings = config.settings,
    handlers = config.handlers,
  }
end

require("fidget").setup()
require("rust-tools").setup {
  server = {
    on_attach = on_attach,
    settings = {
      ["rust-analyzer"] = {
        checkOnSave = {
          command = "clippy",
        },
        inlayHints = {
          -- These should let you peek under the veil of some of the magic Rust does,
          -- but I'm not sure how they'll display in Neovim. Suggested by:
          -- https://github.com/rust-lang/reference/issues/788#issuecomment-1420494386
          expressionAdjustmentHints = {
            enable = true,
          },
          lifetimeElisionHints = {
            enable = true,
          },
        },
      },
    },
  },
}

----------------------------------------------------------------------------- Completion {{{1
-- NOTE: Most of this section is lifted from the nvim-lspconfig wiki, check there for
-- updates: https://github.com/neovim/nvim-lspconfig/wiki/Autocompletion

local cmp = require("cmp")
cmp.setup {
  snippet = {
    expand = function(args)
      vim.fn["UltiSnips#Anon"](args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
        -- NOTE: Some snippet plugins allow for more clever integration where keys can
        -- mean even more things based on context. With UltiSnips, you'll have to expand
        -- using Enter.
        -- elseif luasnip.expand_or_jumpable() then
        --   luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
        -- elseif luasnip.jumpable(-1) then
        --   luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),
  },
  sources = {
    { name = "path" },
    { name = "nvim_lsp" },
    { name = "ultisnips" },
  },
}

------------------------------------------------------------------------------- Gitsigns {{{1
-- NOTE: Most of this section is lifted from the Gitsigns readme, check there for
-- updates: https://github.com/lewis6991/gitsigns.nvim

local gs = require("gitsigns")
gs.setup {
  on_attach = function(bufnr)
    local function gmap(mode, lhs, rhs, desc)
      print(lhs)
      map(mode, lhs, rhs, { buffer = bufnr, desc = "Git: " .. desc })
    end

    -- Navigation
    gmap("n", "]c", function()
      if vim.wo.diff then
        return "]c"
      end
      vim.schedule(function()
        gs.next_hunk()
      end)
      return "<Ignore>"
    end, "Next hunk")

    gmap("n", "[c", function()
      if vim.wo.diff then
        return "[c"
      end
      vim.schedule(function()
        gs.prev_hunk()
      end)
      return "<Ignore>"
    end, "Prev hunk")

    -- Actions
    gmap({ "n", "v" }, "<leader>hs", ":Gitsigns stage_hunk<CR>", "Stage hunk")
    gmap({ "n", "v" }, "<leader>hr", ":Gitsigns reset_hunk<CR>", "Reset hunk")
    gmap("n", "<leader>hS", gs.stage_buffer, "Stage buffer")
    gmap("n", "<leader>hu", gs.undo_stage_hunk, "Undo stage hunk")
    gmap("n", "<leader>hR", gs.reset_buffer, "Reset buffer")
    gmap("n", "<leader>hp", gs.preview_hunk, "Preview hunk")
    gmap("n", "<leader>hb", function()
      gs.blame_line { full = true }
    end, "Blame")
    gmap("n", "<leader>hB", gs.toggle_current_line_blame, "Toggle line blame")
    gmap("n", "<leader>hd", gs.diffthis, "Diff index")
    gmap("n", "<leader>hD", function()
      gs.diffthis("~")
    end, "Diff last commit")
    gmap("n", "<leader>ht", gs.toggle_deleted, "Toggle deleted")

    -- Text object
    gmap({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "Select hunk")
  end,
}

------------------------------------------------------------------------- (Auto)commands {{{1
-- See `:help events` for the various kinds of events you can hook into. These also
-- determine what the next argument, the pattern, will be matched against -- the buffer
-- name for Buf events, the filetype for FileType events, etc.

api.nvim_create_user_command(
  "Redir",
  -- d2_ is what 2dd remaps to
  [[:enew | put =execute('<args>') | setlocal buftype=nofile bufhidden=hide noswapfile | normal! ggd2_]],
  {
    nargs = 1,
    complete = "command",
  }
)

-- Add auto commands to an augroup, so that you don't accumulate multiple copies of them
-- when you reload your init file. You could use multiple augroups for grouping related
-- autocommands (and therefore resetting them more granularly), but that's overkill.
local g = api.nvim_create_augroup("MYVIMRC", {})

api.nvim_create_autocmd("VimLeave", { command = "set guicursor=a:ver25", pattern = "*", group = g })

-- Open quickfix if :make et al. yield errors. Exclude commands with an l prefix via
-- pattern, because those fill the location list instead, and the event is triggered
-- before jumping to the first error/match, i.e. before jumping to the buffer from which
-- the location list can be straightforwardly opened.
api.nvim_create_autocmd("QuickFixCmdPost", { command = "cwindow", pattern = "[^l]*", group = g })

-- Put help windows in a vert split.
api.nvim_create_autocmd("FileType", { command = "wincmd L", pattern = "help", group = g })

-- Formatting.
api.nvim_create_autocmd("BufWritePre", { command = "Black", pattern = "*.py", group = g })
api.nvim_create_autocmd("BufWritePre", {
  callback = function()
    local view = vim.fn.winsaveview()
    vim.cmd([[%!stylua --search-parent-directories -]])
    vim.fn.winrestview(view)
  end,
  pattern = "*.lua",
  group = g,
})

-- Linting; see also this gist on vanilla Vim linting:
-- https://gist.github.com/romainl/ce55ce6fdc1659c5fbc0f4224fd6ad29
-- api.nvim_create_autocmd("FileType", {
--   callback = function()
--     vim.bo.makeprg = "pylint --output-format=parseable --score=no"
--   end,
--   pattern = "python",
--   group = g,
-- })
-- api.nvim_create_autocmd(
--   "BufWritePost",
--   { command = "silent make! <afile> | silent redraw!", pattern = "*.py", group = g }
-- )

-- Temporarily highlight yanked text.
api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank { timeout = 300 }
  end,
  pattern = "*",
  group = g,
})

-- Templates for comment syntax definitions not provided by your comment plugin.
-- autocmd FileType sql setlocal commentstring=--\ %s
-- autocmd FileType cfg setlocal commentstring=#\ %s

-- Automatically source and re-compile Packer whenever this init.lua is saved.
vim.api.nvim_create_autocmd("BufWritePost", {
  command = "source <afile> | doautocmd BufRead | PackerCompile",
  pattern = vim.fn.resolve(vim.env.MYVIMRC),
  group = g,
})

------------------------------------------------------------------- General key bindings {{{1
-- NOTE: From v0.7 onwards, Neovim can map arbitrary Ctrl sequences in supported
-- terminals, distinguishing e.g. between C-i and Tab or mapping even C-; or C-1. See
-- http://neovim.io/news/2022/04#distinguishing-modifier-keys for details.

map("i", "fd", "<Esc>")
-- Switch to normal mode in :terminal.
map("t", "fd", [[<C-\><C-n>]])

-- Aliases for when I accidentally keep Shift pressed after entering command mode.
for _, cmd in pairs { "Q", "W", "X" } do
  map("n", ":" .. cmd, ":" .. cmd:lower())
end

-- You can auto-trigger completion in a command-line mode mapping. Traditionally, you
-- had to configure a `:help 'wildcharm'` in order to be able to use it on the RHS of a
-- macro (mapping), but Neovim has `:help c_CTRL_Z` which is always available for this
-- purpose.
map("n", ":e ", ":edit <C-z>")

-- Avoid having to escape special characters in searches ("very magic").
map("", "/", [[/\v]])
map("", "?", [[?\v]])

map("", "<Right>", "gt")
map("", "<Left>", "gT")
map("", "<Down>", ":bn<CR>")
map("", "<Up>", ":bp<CR>")
-- Enable command history filtering for Ctrl-P and Ctrl-N.
map("c", "<C-p>", "<Up>")
map("c", "<C-n>", "<Down>")

-- Toggle folds.
map("", "zi", function()
  if vim.wo.foldenable then
    vim.wo.foldenable = false
    vim.wo.foldcolumn = "0"
  else
    -- vim.cmd.write()
    vim.wo.foldenable = true
    vim.wo.foldcolumn = "auto:9"
    vim.cmd([[normal! zx]])
  end
end, { silent = true })

-- Make Y behave like D, C -> perform action from cursor to end of line.
map("n", "Y", "y$")

-- Keep cursor centered during various operations.
map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")
map("n", "J", "mzJ`z")

-- Undo break points (useful especially when typing large amounts of prose in one go).
map("i", ".", ".<C-g>u")
map("i", ",", ",<C-g>u")
map("i", ";", ";<C-g>u")
map("i", "!", "!<C-g>u")
map("i", "?", "?<C-g>u")
map("i", [["]], [["<C-g>u]])
map("i", "-", "-<C-g>u")
map("i", "(", "(<C-g>u")
map("i", ")", ")<C-g>u")

-- Moving selections around while reindenting properly, without messing up registers (!)
map("v", "J", [[:move '>+1<CR>gv=gv]])
map("v", "K", [[:move '<-2<CR>gv=gv]])
-- TODO: variations on the above for insert and normal mode, but I can't think of good
-- bindings for those right now.
-- map("i", "???", [[<Esc>:move .+1<CR>==]])
-- map("i", "???", [[<Esc>:move .-2<CR>==]])
-- map("n", "???", [[:move .+1<CR>==]])
-- map("n", "???", [[:move .-2<CR>==]])

map("", "<leader><leader>", ":", { remap = true })
-- Execute visual selection of Lua code.
map("v", "<leader>x", [[y \| :call luaeval(@")<CR>]])
-- Pretty-print Lua expression.
map("n", "<leader>pp", ":lua=")
-- rr stands for redirect (from pager to buffer), even though with recent versions of
-- (Neo)vim, there's a simpler way than using redir.
map("n", "<leader>rr", ":Redir<Space>")
map("n", "<leader><Tab>", "<C-^>")
map("n", "<leader>w", "<C-w>")

-- Without autochdir, this is useful to quickly complete the path of the active buffer.
map("c", "%%", [[getcmdtype() == ':' ? expand('%:h').'/' : '%%']], { expr = true })
-- And these are for fuzzy-finding files anywhere in the directory tree.
map("c", "**", [[getcmdtype() == ':' ? '**/*' : '**']], { expr = true })
map("c", "***", [[getcmdtype() == ':' ? '**/.*' : '***']], { expr = true })
map("c", "~~", [[getcmdtype() == ':' ? '~/' : '~~']], { expr = true })

-- Fix spelling mapping from https://castel.dev/post/lecture-notes-1/ (there are lots of
-- good tips there, go back for more at some point). <C-g>u groups actions into one undo
-- step; [s jumps to previous spelling mistake, 1z= applies the first suggestion from
-- the list of fixes, `] jumps to the end of the last edit
map("i", "<C-l>", [[<C-g>u<Esc>[s1z=`]a<C-g>u]])

-- vi: foldmethod=marker
