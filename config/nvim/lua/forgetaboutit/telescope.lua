require("telescope").setup({
  defaults = {},
  pickers = {},
  extensions = {},
})

---@param binding string The binding to use
---@param f function A lua function which gets passed the `telescope.builtin` module
---@param opts? vim.keymap.set.Opts Options for the binding
local function nmap(binding, f, opts)
  vim.keymap.set({"n"}, binding, function() f(require("telescope.builtin")) end, opts)
end

nmap("<leader>tk", function(t) t.keymaps() end, { desc = "Search keymaps" })
nmap("<leader>tg", function(t) t.live_grep() end, { desc = "Search text" })
nmap("<leader>tf", function(t) t.find_files() end, { desc = "Search files" })
nmap("<leader>tt", function(t) t.treesitter() end, { desc = "Search treesitter" })
nmap("<leader>gs", function(t) t.git_status() end, { desc = "Show Git status" })
nmap("<leader>tb", function(t) t.buffers() end, { desc = "Search buffers" })
nmap("<leader>tq", function(t) t.quickfix() end, { desc = "Search quickfixes" })
nmap("<leader>tc", function(t) t.commands() end, { desc = "Search commands" })
nmap("<leader>tm", function(t) t.marks() end, { desc = "Search marks" })
nmap("<leader>tr", function(t) t.registers() end, { desc = "Search registers" })
nmap("<leader>td", function(t) t.diagnostics() end, { desc = "Search diagnostics" })
nmap("<leader>lr", function(t) t.lsp_references() end, { desc = "Search LSP references" })
nmap("<leader>li", function(t) t.lsp_incoming_calls() end, { desc = "Search LSP incoming calls" })
nmap("<leader>lo", function(t) t.lsp_outgoing_calls() end, { desc = "Search LSP outgoing calls" })
nmap("<leader>ld", function(t) t.lsp_definitions() end, { desc = "Search LSP definitions" })
nmap("<leader>lt", function(t) t.lsp_type_definitions() end, { desc = "Search LSP type definitions" })
nmap("<leader>lI", function(t) t.lsp_implementations() end, { desc = "Search LSP implementations" })
nmap("<leader>ls", function(t) t.lsp_workspace_symbols() end, { desc = "Search LSP workspace symbols" })
nmap("<leader>lS", function(t) t.lsp_document_symbols() end, { desc = "Search LSP document symbols" })

