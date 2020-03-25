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
function run_md_block(cursor_row, lines, quiet)
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
function run_md_blocks(how)
  local cursor_row = api.nvim_win_get_cursor(0)[1]
  local all_lines = api.nvim_buf_get_lines(0, 0, -1, false)

  if how == "before" then
    lines = { unpack(all_lines, 1, cursor_row) }
  elseif how == "after" then
    run_md_block(cursor_row, all_lines, true)
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
    run_md_block(cursor_row, all_lines)
  end
end
