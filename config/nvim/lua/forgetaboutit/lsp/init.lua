-- NOTE: This file uses lzextras.lsp handler https://github.com/BirdeeHub/lzextras?tab=readme-ov-file#lsp-handler
-- This is a slightly more performant fallback function
-- for when you don't provide a filetype to trigger on yourself.
-- nixCats gives us the paths, which is faster than searching the rtp!
local old_ft_fallback = require('lze').h.lsp.get_ft_fallback()
require('lze').h.lsp.set_ft_fallback(function(name)
  print("Running fallback " .. name)
  local lspcfg = nixCats.pawsible({ "allPlugins", "opt", "nvim-lspconfig" })
      or nixCats.pawsible({ "allPlugins", "start", "nvim-lspconfig" })

  if lspcfg then
    local ok, cfg = pcall(dofile, lspcfg .. "/lsp/" .. name .. ".lua")
    if not ok then
      ok, cfg = pcall(dofile, lspcfg .. "/lua/lspconfig/configs/" .. name .. ".lua")
    end
    return (ok and cfg or {}).filetypes or {}
  else
    return old_ft_fallback(name)
  end
end)

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(event)
    local bufrn = event.buf

    local map = function(mode, lhs, rhs, desc)
      vim.keymap.set(mode, lhs, rhs, {
        buffer = bufrn,
        silent = true,
        desc = "LSP: " .. desc
      })
    end

    map("n", "<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
    map("n", "<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")
  end
})

require("lze").load {
  {
    "nvim-lspconfig",
    for_cat = "general",
    on_require = { "lspconfig" },
    lsp = function(plugin)
      vim.lsp.config(plugin.name, plugin.lsp or {})
      vim.lsp.enable(plugin.name)
    end,
  },
  {
    "lazydev",
    enabled = true,
    ft = "lua",
    after = function(_)
      require("lazydev").setup({
        library = {
          {
            path = "${3rd}/luv/library",
            words = { "vim%.uv" }
          },
        }
      })
    end
  },
  {
    "lua_ls",
    enabled = true,
    lsp = {
      filetypes = { "lua" },
      settings = {
        Lua = {
          runtime = { version = "LuaJIT" },
          formatters = {
            ignoreComments = true,
          },
          signatureHelp = { enable = true },
          diagnostics = {
            globals = { "nixCats", "vim", },
            disable = { "missing-fields" },
          },
          telemetry = { enabled = false },
        }
      }
    }
  },
  {
    "ts_ls",
    enabled = true,
    lsp = {
      filetypes = { "javascript", "typescript" },
    },
  },
  {
    "purescriptls",
    enabled = true,
    lsp = {
      filetypes = { "purescript" },
    },
  },
  {
    "tinymist",
    enabled = true,
    lsp = {
      filetypes = { "typst" },
      settings = {
        formatterMode = "typstyle",
        exportPdf = "onType",
        semanticTokens = "disable",
      },
    },
  },
  {
    "rust_analyzer",
    enabled = true,
    lsp = {
      filetypes = { "rust" },
      settings = {
        ["rust-analyzer"] = {
          checkOnSave = true,
          check = {
            command = "clippy",
          },
          diagnostics = {
            enable = true,
          },
        },
      },
    },
  },
  {
    "zls",
    enabled = true,
    lsp = {
      filetypes = { "zig", "zir" },
      settings = {
        ["zls"] = {

        }
      }
    }
  }
}

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp", { clear = true }),
  callback = function(args)
    vim.api.nvim_create_autocmd("BufWritePre", {
      buffer = args.buf,
      callback = function()
        vim.lsp.buf.format { async = false, id = args.data.client_id }
      end
    })
  end
})
