local vim = vim
local api = vim.api

local M = {}

----------------------------------------------------------------------------- LSP config {{{1

local lspconfig = require("lspconfig")
local lsp_status = require("lsp-status")

-- use LSP SymbolKinds themselves as the kind labels
local kind_labels_mt = {__index = function(_, k) return k end}
local kind_labels = {}
setmetatable(kind_labels, kind_labels_mt)

lsp_status.register_progress()
lsp_status.config({
  kind_labels = kind_labels,
  indicator_errors = "E:",
  indicator_warnings = "W:",
  indicator_info = "I:",
  indicator_hint = "H:",
  indicator_ok = "✓",
  -- the default is a wide codepoint which breaks absolute and relative
  -- line counts if placed before airline's Z section
  status_symbol = "LSP",
})

function M.clear_document_highlight()
  local ns_id = api.nvim_create_namespace("vim_lsp_references")
  api.nvim_buf_clear_namespace(0, ns_id, 0, -1)
end

-- cf. :help lsp-config for tips on what to map; possibly migrate to Telescope
-- alternatives where available when they've matured (right now for instance, there seem
-- to be issues with multiple lang servers per buffer)
local lsp_mappings = {
  {"n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>"},
  {"n", "gh", "<cmd>lua vim.lsp.buf.signature_help()<CR>"},
  {"n", "<C-]>", "<cmd>lua vim.lsp.buf.definition()<CR>"},
  {"n", "gO", "<cmd>lua vim.lsp.buf.references()<CR><cmd>copen<CR>"},
  -- {"n", "gd", "<cmd>lua require'telescope.builtin'.lsp_definitions{}<CR>"},
  -- {"n", "gO", "<cmd>lua require'telescope.builtin'.lsp_references{}<CR>"},
  {"n", "[d", "<cmd>lua vim.diagnostic.goto_prev { wrap = false }<CR>"},
  {"n", "]d", "<cmd>lua vim.diagnostic.goto_next { wrap = false }<CR>"},
  {"n", "<leader>ls", "<cmd>lua require'telescope.builtin'.lsp_workspace_symbols{}<CR>"},
  {"n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<CR>"},
  {"n", "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>"},
  {"", "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>"},
  -- {"n", "<leader>la", "<cmd>lua require'telescope.builtin'.lsp_code_actions{}<CR>"},
  -- {"", "<leader>la", "<cmd>lua require'telescope.builtin'.lsp_range_code_actions{}<CR>"},
  {"n", "<leader>lh", "<cmd>lua vim.lsp.buf.document_highlight()<CR>"},
  {"n", "<leader>lH", "<cmd>lua init.clear_document_highlight()<CR>"},
  {"n", "<leader>ld", "<cmd>lua vim.diagnostic.open_float(0, {scope='cursor'})<CR>"},
  {"n", "<leader>lD", "<cmd>lua vim.diagnostic.setloclist()<CR><cmd>lopen<CR>"},
  -- {"n", "<leader>lD", "<cmd>lua require'telescope.builtin'.lsp_document_diagnostics{}<CR>"},
}
-- NOTE: in order to yield all elements, unpack has to be the last
-- (or only) expression in a list of expressions, so append the options
-- to each mapping
local mapping_opts = {}--{noremap = true}--, silent = true}
for _, mapping in ipairs(lsp_mappings) do
  table.insert(mapping, mapping_opts)
end

local on_attach = function(client, bufnr)
  lsp_status.on_attach(client, bufnr)
  -- NOTE: uncomment to inspect features supported by language server
  -- print(vim.inspect(client.resolved_capabilities))
  if client.resolved_capabilities.document_formatting then
    api.nvim_command("autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync(nil, 1000)")
  end

  for _, args in ipairs(lsp_mappings) do
    api.nvim_buf_set_keymap(bufnr, unpack(args))
  end
end

local servers = {
  -- Rust support is handled by a separate plugin, see below
  r_language_server = {},
  sumneko_lua = {
    settings = {
      Lua = {
        runtime = {
          version = "LuaJIT",
          -- set up Lua path
          path = vim.split(package.path, ";"),
        },
        diagnostics = {
          globals = {"vim"},
        },
        workspace = {
          -- make the server aware of Neovim runtime files
          library = {
            [vim.fn.expand("$VIMRUNTIME/lua")] = true,
            [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
          },
        },
      },
    },
  },

  -- Node-based
  pyright = {
    settings = {
      python = {
        analysis = {
          stubPath = vim.env.HOME.."/.files/python/typings",
        },
      },
    },
  },
  bashls = {},
  elmls = {},
  vimls = {},
  tsserver = {},
}

local lls = vim.env.HOME .. "/.local/lua-language-server"
for ls, config in pairs(servers) do
  local cmd = ls == "sumneko_lua" and {lls .. "/bin/lua-language-server", "-E", lls .. "/main.lua"} or nil
  local capabilities = vim.tbl_extend("keep", lspconfig[ls].capabilities or {}, lsp_status.capabilities)
  capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)
  lspconfig[ls].setup {
    cmd = cmd,
    on_attach = on_attach,
    capabilities = capabilities,
    settings = config.settings,
    handlers = config.handlers,
  }
end

require("rust-tools").setup {
  server = {
    on_attach = on_attach,
    settings = {
      ["rust-analyzer"] = {
        checkOnSave = {
          command = "clippy",
        },
      },
    },
  },
}

--[[
Logging/debugging

Log levels by name: "trace", "debug", "info", "warn", "error"

The log path should typically be ~/.local/share/nvim/lsp.log
--]]
-- vim.lsp.set_log_level("info")
-- print(vim.lsp.get_log_path())

----------------------------------------------------------------------------- Completion {{{1

-- NOTE: Most of this section is lifted from the nvim-lspconfig wiki, check there for
-- updates: <https://github.com/neovim/nvim-lspconfig/wiki/Autocompletion>.

local cmp = require("cmp")
cmp.setup {
  snippet = {
    expand = function(args)
      vim.fn["UltiSnips#Anon"](args.body)
    end,
  },
  mapping = {
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
    ["<Tab>"] = function(fallback)
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
    end,
    ["<S-Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      -- elseif luasnip.jumpable(-1) then
      --   luasnip.jump(-1)
      else
        fallback()
      end
    end,
  },
  sources = {
    { name = "path" },
    { name = "nvim_lsp" },
    { name = "ultisnips" },
  },
}

---------------------------------------------------------------------- Treesitter config {{{1

local ts = require("nvim-treesitter.configs")

ts.setup {
  ensure_installed = {
    -- Everything...
    "bash", "bibtex", "c", "comment", "cpp", "css", "dockerfile", "elm", "fish", "go",
    "help", "html", "java", "javascript", "json", "latex", "lua", "make", "markdown",
    "perl", "php", "python", "r", "regex", "rst", "ruby", "rust", "toml", "typescript",
    "vim", "yaml", "zig",
    -- ... and the kitchen sink.
    "clojure", "cmake","commonlisp", "dot", "fennel", "fortran", "gomod", "gowork",
    "haskell", "hjson", "http", "jsdoc", "json5", "jsonc", "julia", "kotlin", "llvm",
    "ninja", "nix", "norg", "ocaml", "org", "pascal", "query", "scala", "scheme",
    "scss", "supercollider", "swift", "svelte", "tlaplus", "tsx", "vala", "vue",
  },
  highlight = { enable = true },
  -- Currently only affects the = operator and labeled as experimental, so not worth
  -- enabling?
  indent = { enable = false },
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
    -- nice in theory but unfortunately sort of ugly because the
    -- highlight is ragged; cf. :help syn-pattern: "The highlighted area
    -- will never be outside of the matched text."
    -- highlight_current_scope = { enable = true },
  },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
       -- use capture groups from textobjects.scm or define your own
       -- TODO: The comment textobject is currently not that useful, pending
       -- <https://github.com/nvim-treesitter/nvim-treesitter-textobjects/issues/133>.
       ["ac"] = "@comment.outer",
       ["af"] = "@function.outer",
       ["if"] = "@function.inner",
       ["aC"] = "@class.outer",
       ["iC"] = "@class.inner",
      }
    },
    move = {
      enable = true,
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
  },
}

----------------------------------------------------------------------- Telescope config {{{1

local telescope = require("telescope")
local tactions = require("telescope.actions")
local tbuiltin = require("telescope.builtin")

local tpickers = {}
for picker_name, _ in pairs(tbuiltin) do
  tpickers[picker_name] = {theme = "dropdown"}
end

telescope.setup{
  defaults = {
    mappings = {
      i = {
        ["<esc>"] = tactions.close
      },
    },
  },
  pickers = tpickers,
  extensions = {
    file_browser = {
      theme = "dropdown",
    },
  },
}
telescope.load_extension("fzf")
telescope.load_extension("file_browser")

-------------------------------------------------------------------------- Return module {{{1

return M

-- vi: foldmethod=marker
