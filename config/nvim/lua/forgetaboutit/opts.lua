--
-- Global variables
--
local g = vim.g

-- <leader> keys
g.mapleader = ' '
g.maplocalleader = ','

--
-- Global options
--
local opt = vim.o
local fn = vim.fn

if fn.has('termguicolors') then
  -- Enable 24 bit true color support if possible
  opt.termguicolors = true
end

-- Search down into subfolders with find commands
opt.path = vim.o.path .. '**'

-- Show line numbers
opt.number = true

-- Show line numbers relative to current line
opt.relativenumber = true

-- Highlight matching parentheses, etc
opt.showmatch = true

-- Show matches as we type
opt.incsearch = true

-- Don't highlight all matches for search
opt.hlsearch = false

-- Spell checking
opt.spell = true
-- ... for english
opt.spelllang = 'en'

-- Sensible indenting
opt.expandtab = true
opt.tabstop = 2
opt.softtabstop = 2
opt.shiftwidth = 2

-- Try to use smart indents for new lines
opt.smartindent = true

-- Store longer history
opt.history = 2000

-- Number formats for CTRL-A and CTRL-X
opt.nrformats = 'bin,hex' -- 'octal'

-- Don't create local swap and backup files
opt.swapfile = false

-- ... but use undo files for undo history
opt.undofile = true

-- Create new vsplit window to the right of the current one
opt.splitright = true

-- Create new split window to the bottom of the current one
opt.splitbelow = true

-- Never show more than 8 empty rows on the bottom if possible
opt.scrolloff = 8

-- Always show the sign column to prevent jumpiness
opt.signcolumn = 'yes'

-- Idleness in milliseconds to update the swap file for recovery
opt.updatetime = 50

-- Characters to fill special lines with
opt.fillchars = [[eob: ,fold: ,foldopen:,foldsep: ,foldclose:]]

-- Characters to display in a special way such as whitespace
opt.list = true
opt.listchars = "tab:» ,trail:·,nbsp:␣"

-- Color the optimal width column to make it easy to see
opt.colorcolumn = '80'

-- Always use a block cursor
opt.guicursor = "a:block"

-- We don't use that thing here
opt.mouse = ""

-- Show completion menu, even when there's only one candidate; require explicit
-- selection of an option
opt.completeopt = "menu,menuone,noinsert"

-- Nicer borders for our windows
opt.winborder = "rounded"

--
-- Additional configuration
--
local cmd = vim.cmd

-- Enable filetype detection, plugin loading, and indentation
cmd.filetype('plugin', 'indent', 'on')
