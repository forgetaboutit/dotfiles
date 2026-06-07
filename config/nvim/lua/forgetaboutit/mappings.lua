local k = vim.keymap

-- Helper for easily reloading lua files
k.set({ "n" }, "<leader><leader>x", function() vim.cmd([[ source % ]]) end, { desc = "Source current buffer" })

local hop = require("hop")
local directions = require("hop.hint").HintDirection;

hop.setup {}

k.set({ "v", "n", "o" }, "<leader>ha", function()
  hop.hint_char1({
    current_line_only = false,
    multi_windows = false,
  })
end, { desc = "Hop anywhere" })
k.set("", "<leader>hl", function() hop.hint_lines_skip_whitespace({}) end, { desc = "Hop line" })
k.set("", "<leader>f", function() hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = false }) end,
  { desc = "Hop forward" })

k.set("n", "<leader>qf",
  function() vim.lsp.buf.code_action({ filter = function(a) return a.isPreferred end, apply = true }) end,
  { noremap = true, silent = true, desc = "Apply code action" })
