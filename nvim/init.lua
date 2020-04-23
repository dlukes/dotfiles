local vim = vim
local api = vim.api

local M = {}

------------------------------------------------------------- Iron REPLs

local iron = require("iron")

iron.core.set_config {
  -- only one REPL per filetype
  manager = iron.behavior.manager.singleton,
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
local nvim_lsp = require("nvim_lsp")
local configs = require("nvim_lsp/configs")
-- load configs.sumneko_lua
require("nvim_lsp/sumneko_lua")

-- TODO: get rid of this once an R lang server config is merged into
-- nvim_lsp
configs.Rls = {
  default_config = {
    cmd = {"R", "--slave", "-e", "languageserver::run()"},
    filetypes = {"r", "rmd"},
    root_dir = function(fname)
      return nvim_lsp.util.find_git_ancestor(fname) or vim.loop.os_homedir()
    end,
    log_level = lsp.protocol.MessageType.Warning,
    settings = {},
  }
}

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
  api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  -- NOTE: uncomment to inspect features supported by language server
  -- print(vim.inspect(client.resolved_capabilities))
  if client.resolved_capabilities.document_formatting then
    api.nvim_command("autocmd BufWritePre <buffer> lua init.formatting_sync(nil, 1000)")
  end

  local opts = {noremap = true, silent = true}
  -- cf. :help lsp-config for tips on what to map
  api.nvim_buf_set_keymap(bufnr, "n", "gh", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "gH", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR><cmd>copen<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<leader>lh", "<cmd>lua vim.lsp.buf.document_highlight()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<leader>lH", "<cmd>lua init.clear_document_highlight()<CR>", opts)
  api.nvim_buf_set_keymap(bufnr, "n", "<leader>ld", "<cmd>lua vim.lsp.util.show_line_diagnostics()<CR>", opts)
end

-- populate quickfix list with diagnostics
local method = "textDocument/publishDiagnostics"
local default_callback = lsp.callbacks[method]
lsp.callbacks[method] = function(err, method, result, client_id)
  default_callback(err, method, result, client_id)
  if result and result.diagnostics then
    for _, v in ipairs(result.diagnostics) do
      v.bufnr = client_id
      v.lnum = v.range.start.line + 1
      v.col = v.range.start.character + 1
      v.text = v.message
    end
    lsp.util.set_qflist(result.diagnostics)
  end
end

local servers = {
  rust_analyzer = {
    ["rust-analyzer"] = {
      checkOnSave = {
        command = "clippy",
      },
    },
  },
  pyls_ms = {},
  elmls = {},
  Rls = {},
  sumneko_lua = {},
  vimls = {},
}
for ls, settings in pairs(servers) do
  nvim_lsp[ls].setup {
    on_attach = on_attach,
    settings = settings,
  }
end

-- sign column
api.nvim_set_var("LspDiagnosticsErrorSign", "×")                    -- ✖
api.nvim_set_var("LspDiagnosticsWarningSign", "!")                  -- ⚠
api.nvim_set_var("LspDiagnosticsInformationSign", "i")              -- ℹ
api.nvim_set_var("LspDiagnosticsHintSign", "›")                     -- ➤

api.nvim_command("highlight! link LspDiagnosticsError SpellBad")
api.nvim_command("highlight! link LspDiagnosticsWarning SpellRare")
api.nvim_command("highlight! link LspDiagnosticsInformation SpellCap")
api.nvim_command("highlight! link LspDiagnosticsHint SpellLocal")
api.nvim_command("highlight! link LspReferenceText Search")
api.nvim_command("highlight! link LspReferenceRead Search")
api.nvim_command("highlight! link LspReferenceWrite Search")

--[[
Logging/debugging

Log levels by name: "trace", "debug", "info", "warn", "error"

The log path should typically be ~/.local/share/nvim/vim-lsp.log
--]]
-- lsp.set_log_level("info")
-- print(lsp.get_log_path())

---------------------------------------------------------- Return module

return M
