local api = vim.api
local iron = require("iron")

iron.core.set_config {
  -- only one REPL per filetype
  manager = iron.behavior.manager.singleton,
  preferred = {
    python = "ipython",
  },
  repl_open_cmd = "topleft vertical 80 split",
}

-- Execute a markdown code block in a REPL appropriate for the language.
function execute_markdown_code_block()
  local cursor_row = api.nvim_win_get_cursor(0)[1]
  local lines = api.nvim_buf_get_lines(0, 0, -1, false)

  local from = cursor_row
  local lang, found, line
  while true do
    line = lines[from]
    found, _, lang = string.find(line, "^%s*```(%w+)")
    if found then
      from = from + 1
      break
    elseif from == 1 or from ~= cursor_row and string.find(line, "^%s*```") then
      print("Not inside a code block.")
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
      print("Not inside a code block.")
      return
    end
    to = to + 1
  end

  iron.core.send(lang, { unpack(lines, from, to) })
end
