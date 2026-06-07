-- Use experimental module loader with bytecode caching
vim.loader.enable()

require("lze").register_handlers(require("lzextras").lsp)

vim.cmd [[colorscheme catppuccin-mocha]]

require("forgetaboutit.opts")
require("forgetaboutit.diagnostic")
require("forgetaboutit.mappings")
require("forgetaboutit.cmp")
require("forgetaboutit.qol")
require("forgetaboutit.lsp")
require("forgetaboutit.lualine")
require("forgetaboutit.telescope")

