local vim = vim
local api = vim.api

local M = {}

------------------------------------------------------------- Iron REPLs

local iron = require("iron")

iron.core.set_config {
  -- only one REPL per filetype
  manager = iron.behavior.scope.singleton,
  preferred = {
    python = "ipython",
  },
  repl_open_cmd = "topleft vertical 80 split",
}

------------------------------------------ Markdown code block execution

-- Execute a markdown code block in a REPL appropriate for the language.
function M.run_md_block(cursor_row, lines, quiet)
  if not cursor_row then
    cursor_row = api.nvim_win_get_cursor(0)[1]
  end
  if not lines then
    lines = api.nvim_buf_get_lines(0, 0, -1, false)
  end

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

------------------------------------------------------------- LSP config

local lsp = vim.lsp
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

function M.formatting_sync(options, timeout)
  -- START lifted from vim.lsp.buf.formatting
  vim.validate { options = {options, "t", true} }
  local sts = vim.bo.softtabstop;
  options = vim.tbl_extend("keep", options or {}, {
    tabSize = (sts > 0 and sts) or (sts < 0 and vim.bo.shiftwidth) or vim.bo.tabstop;
    insertSpaces = vim.bo.expandtab;
  })
  local params = {
    textDocument = { uri = vim.uri_from_bufnr(0) };
    options = options;
  }
  -- END lifted from vim.lsp.buf.formatting
  local result = lsp.buf_request_sync(0, "textDocument/formatting", params, timeout)
  if not result then return end
  result = result[1].result
  lsp.util.apply_text_edits(result)
end

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
    api.nvim_command("autocmd BufWritePre <buffer> lua init.formatting_sync(nil, 1000)")
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
  -- these don't work with :LspInstall (yet?)
  rust_analyzer = {
    ["rust-analyzer"] = {
      checkOnSave = {
        command = "clippy",
      },
    },
  },
  jedi_language_server = {},  -- as in, Python's Jedi
  -- pyright = {},  -- TODO: enable once it settles down
  r_language_server = {},

  elmls = {},
  sumneko_lua = {},
  tsserver = {},
  vimls = {},
}
for ls, settings in pairs(servers) do
  lspconfig[ls].setup {
    on_attach = on_attach,
    settings = settings,
    capabilities = vim.tbl_extend("keep", configs[ls].capabilities or {}, lsp_status.capabilities),
  }
end

function M.lsp_clients(verbose)
  for i, client in ipairs(vim.lsp.buf_get_clients()) do
    print(i, "::", client.config.name)
    if verbose == 1 then
      print(string.rep("=", 72))
      print(vim.inspect(client))
      print(string.rep("=", 72))
    end
  end
end

--[[
Logging/debugging

Log levels by name: "trace", "debug", "info", "warn", "error"

The log path should typically be ~/.local/share/nvim/vim-lsp.log
--]]
-- lsp.set_log_level("info")
-- print(lsp.get_log_path())

------------------------------------------------------ Treesitter config

local ts = require('nvim-treesitter.configs')

ts.setup {
  ensure_installed = "all",
  -- TODO: use_languagetree = true once it stabilizes
  highlight = { enable = true },
  indent = { enable = true },
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

---------------------------------------------------------- Return module

return M
