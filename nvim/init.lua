local vim = vim
local api = vim.api

local M = {}

------------------------------------------------------------- Iron REPLs {{{1

local iron = require("iron")

iron.core.set_config {
  -- only one REPL per filetype
  manager = iron.behavior.scope.singleton,
  preferred = {
    python = "ipython",
  },
  repl_open_cmd = "topleft vertical 80 split",
}

------------------------------------------ Markdown code block execution {{{1

-- Execute a markdown code block in a REPL appropriate for the language.
function M.run_md_block(cursor_row, lines, quiet)
  if not cursor_row then
    cursor_row = api.nvim_win_get_cursor(0)[1]
  end
  if not lines then
    lines = api.nvim_buf_get_lines(0, 0, -1, false)
  end

  -- TODO: rewrite this parsing code to leverage TreeSitter, once the
  -- Markdown parser becomes usable, that is
  local from = cursor_row
  local lang, found, line
  while true do
    line = lines[from]
    found, _, lang = string.find(line, "^%s*```(%w+)")
    if found then
      from = from + 1
      break
    elseif from == 1 or from ~= cursor_row and string.find(line, "^%s*```") then
      if not quiet then
        print("Not inside a code block.")
      end
      return
    end
    from = from - 1
  end

  local num_lines = #lines
  local to = cursor_row
  while true do
    line = lines[to]
    found = string.find(line, "^%s*```%s*$")
    if found then
      to = to - 1
      break
    elseif to == num_lines or to ~= cursor_row and string.find(line, "^%s*```%s*%S") then
      if not quiet then
        print("Not inside a code block.")
      end
      return
    end
    to = to + 1
  end

  iron.core.send(lang, { unpack(lines, from, to) })
end

-- Run markdown code blocks in file.
--   if how == "before", then all before current one inclusive
--   if how == "after", then all after current one inclusive
--   otherwise, all of them
function M.run_md_blocks(how)
  local cursor_row = api.nvim_win_get_cursor(0)[1]
  local all_lines = api.nvim_buf_get_lines(0, 0, -1, false)

  local lines
  if how == "before" then
    lines = { unpack(all_lines, 1, cursor_row) }
  elseif how == "after" then
    M.run_md_block(cursor_row, all_lines, true)
    lines = { unpack(all_lines, cursor_row) }
  end

  -- TODO: rewrite this parsing code to leverage TreeSitter, once the
  -- Markdown parser becomes usable, that is
  local lang, from, to
  for i, line in ipairs(lines) do
    if not from then
      local found, _, lng = string.find(line, "^%s*```(%w+)")
      if found then
        from = i + 1
        lang = lng
      end
    else
      if string.find(line, "^%s*```%s*$") then
        to = i - 1
        iron.core.send(lang, { unpack(lines, from, to) })
        from = nil
      end
    end
  end

  if how == "before" then
    M.run_md_block(cursor_row, all_lines)
  end
end

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

local on_attach = function(client, bufnr)
  require('completion').on_attach(client, bufnr)
  lsp_status.on_attach(client, bufnr)
  -- NOTE: uncomment to inspect features supported by language server
  -- print(vim.inspect(client.resolved_capabilities))
  if client.resolved_capabilities.document_formatting then
    api.nvim_command("autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync(nil, 1000)")
  end

  local opts = {noremap = true, silent = true}
  -- cf. :help lsp-config for tips on what to map
  api.nvim_buf_set_keymap(bufnr, "n", "gh", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "gH", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<C-]>", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "gO", "<cmd>lua vim.lsp.buf.references()<CR><cmd>copen<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev { wrap = false }<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next { wrap = false }<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<leader>lh", "<cmd>lua vim.lsp.buf.document_highlight()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<leader>lH", "<cmd>lua init.clear_document_highlight()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<leader>ld", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<leader>lD", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR><cmd>lopen<CR>", opts)
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
