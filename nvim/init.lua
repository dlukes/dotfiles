local vim = vim
local api = vim.api

local M = {}

------------------------------------------------------------- LSP config {{{1

local lspconfig = require("lspconfig")
local configs = require("lspconfig/configs")
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

-- cf. :help lsp-config for tips on what to map
local lsp_mappings = {
  {"n", "gh", "<cmd>lua vim.lsp.buf.hover()<CR>"},
  {"n", "gH", "<cmd>lua vim.lsp.buf.signature_help()<CR>"},
  {"n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>"},
  {"n", "gO", "<cmd>lua vim.lsp.buf.references()<CR><cmd>copen<CR>"},
  {"n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev { wrap = false }<CR>"},
  {"n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next { wrap = false }<CR>"},
  {"n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<CR>"},
  {"n", "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>"},
  {"", "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>"},
  {"n", "<leader>lh", "<cmd>lua vim.lsp.buf.document_highlight()<CR>"},
  {"n", "<leader>lH", "<cmd>lua init.clear_document_highlight()<CR>"},
  {"n", "<leader>ld", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>"},
  {"n", "<leader>lD", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR><cmd>lopen<CR>"},
}
-- NOTE: in order to yield all elements, unpack has to be the last
-- (or only) expression in a list of expressions, so append the options
-- to each mapping
local mapping_opts = {noremap = true, silent = true}
for _, mapping in ipairs(lsp_mappings) do
  table.insert(mapping, mapping_opts)
end

local on_attach = function(client, bufnr)
  require("completion").on_attach(client, bufnr)
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
  rust_analyzer = {
    settings = {
      ["rust-analyzer"] = {
        checkOnSave = {
          command = "clippy",
        },
      },
    },
  },
  jedi_language_server = {},  -- as in, Python's Jedi
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
    handlers = {
      -- disable pyright rename so that it doesn't race with jedi. TODO:
      -- apparently, this should be achievable in a cleaner way by
      -- disabling the rename capability instead of the handler
      -- (something like `capabilities.textDocument.rename = false`),
      -- but when I do that, the LSP log still shows rename as enabled.
      -- another way that does seem to work is to override
      -- client.resolved_capabilities in on_attach above.
      ["textDocument/definition"] = function() end,
      ["textDocument/hover"] = function() end,
      ["textDocument/rename"] = function() end,
      ["textDocument/codeAction"] = function() end,
    }
  },
  bashls = {},
  elmls = {},
  vimls = {},
  tsserver = {},
}

local lls = vim.env.HOME .. "/.local/lua-language-server"
for ls, config in pairs(servers) do
  require("lspconfig/" .. ls)
  local cmd = ls == "sumneko_lua" and {lls .. "/bin/lua-language-server", "-E", lls .. "/main.lua"} or nil
  local capabilities = vim.tbl_extend("keep", configs[ls].capabilities or {}, lsp_status.capabilities)
  -- TODO: this will show snippets in the completion menu, but I don't
  -- really have them configured right now, they're very dumb, the text
  -- just gets inserted into the buffer, but sometimes it should replace
  -- existing text, use placeholders etc. -- none of that works, it
  -- first needs support from completion-nvim and possibly UltiSnips
  -- (search docs/issues for "LSP snippets")
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  lspconfig[ls].setup {
    cmd = cmd,
    on_attach = on_attach,
    capabilities = capabilities,
    settings = config.settings,
    handlers = config.handlers,
  }
end

function M.lsp_clients(verbose)
  print(string.format([[
TIPS
- See also the LSP log for more info: %s
- And remember there can be more criteria for triggering a server than
  just the filetype. Some servers, such as pyright, only work inside
  projects (as determined e.g. by a .git directory, pyproject.toml or
  similar), so don't be surprised if it's not available for every Python
  file. Cf. the root_dir attribute in the server's config.
  ]], vim.lsp.get_log_path()))

  print()
  print("CLIENTS")
  local clients = vim.lsp.buf_get_clients()
  for i, client in ipairs(clients) do
    print(i, "::", client.config.name)
    if verbose == 1 then
      print(string.rep("=", 72))
      print(vim.inspect(client))
      print(string.rep("=", 72))
    end
  end
  if #clients == 0 then
    print("No clients found.")
  end
end

--[[
Logging/debugging

Log levels by name: "trace", "debug", "info", "warn", "error"

The log path should typically be ~/.local/share/nvim/lsp.log
--]]
-- vim.lsp.set_log_level("info")
-- print(vim.lsp.get_log_path())

------------------------------------------------------ Treesitter config {{{1

local ts = require('nvim-treesitter.configs')

ts.setup {
  ensure_installed = "all",
  -- TODO: use_languagetree = true once it stabilizes
  highlight = { enable = true },
  indent = { enable = false },
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
       ["af"] = "@function.outer",
       ["if"] = "@function.inner",
       ["ac"] = "@class.outer",
       ["ic"] = "@class.inner",
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

---------------------------------------------------------- Return module {{{1

return M

-- vi: foldmethod=marker
