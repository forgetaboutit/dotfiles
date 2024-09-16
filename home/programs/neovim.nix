{
  pkgs,
  lib,
  config,
  ...
}: let
  icons = {
    ActiveLSP = "";
    ActiveTS = "";
    ArrowLeft = "";
    ArrowRight = "";
    Bookmarks = "";
    BufferClose = "󰅖";
    DapBreakpoint = "";
    DapBreakpointCondition = "";
    DapBreakpointRejected = "";
    DapLogPoint = ".>";
    DapStopped = "󰁕";
    Debugger = "";
    DefaultFile = "󰈙";
    Diagnostic = "󰒡";
    DiagnosticError = "";
    DiagnosticHint = "󰌵";
    DiagnosticInfo = "󰋼";
    DiagnosticWarn = "";
    Ellipsis = "…";
    FileNew = "";
    FileModified = "";
    FileReadOnly = "";
    FoldClosed = "";
    FoldOpened = "";
    FoldSeparator = " ";
    FolderClosed = "";
    FolderEmpty = "";
    FolderOpen = "";
    Git = "󰊢";
    GitAdd = "";
    GitBranch = "";
    GitChange = "";
    GitConflict = "";
    GitDelete = "";
    GitIgnored = "◌";
    GitRenamed = "➜";
    GitSign = "▎";
    GitStaged = "✓";
    GitUnstaged = "✗";
    GitUntracked = "★";
    LSPLoaded = "";
    LSPLoading1 = "";
    LSPLoading2 = "󰀚";
    LSPLoading3 = "";
    MacroRecording = "";
    Package = "󰏖";
    Paste = "󰅌";
    Refresh = "";
    Search = "";
    Selected = "❯";
    Session = "󱂬";
    Sort = "󰒺";
    Spellcheck = "󰓆";
    Tab = "󰓩";
    TabClose = "󰅙";
    Terminal = "";
    Window = "";
    WordFile = "󰈭";
  };
  helpers = {
    mkRaw = str: {__raw = str;};
  };
  repeatChar = char: count: builtins.concatStringsSep "" (builtins.genList (_: char) count);
  getIcon = {
    name,
    padding ? 0,
  }:
    builtins.getAttr name icons + repeatChar " " padding;
  noop = "function() end";
  # Sections with pretty icons but without actions
  sections =
    lib.mapAttrs' (name: value: {
      name = "<leader>${name}";
      value = {
        action = noop;
        desc = "${getIcon {name = value.icon;}} ${value.desc}";
      };
    }) {
      f = {
        desc = "Find";
        icon = "Search";
      };
      p = {
        desc = "Packages";
        icon = "Package";
      };
      l = {
        desc = "LSP";
        icon = "ActiveLSP";
      };
      u = {
        desc = "UI/UX";
        icon = "Window";
      };
      b = {
        desc = "Buffers";
        icon = "Tab";
      };
      bs = {
        desc = "Sort Buffers";
        icon = "Sort";
      };
      d = {
        desc = "Debugger";
        icon = "Debugger";
      };
      g = {
        desc = "Git";
        icon = "Git";
      };
      S = {
        desc = "Session";
        icon = "Session";
      };
      t = {
        desc = "Terminal";
        icon = "Terminal";
      };
    };

  luaSnippetsPath = "${config.xdg.dataHome}/nvim/luasnippets";
in {
  options = {
    neovim-custom = {
      username-undodir = lib.mkOption {
        type = lib.types.str;
        description = "The current user name into who's home we store our undo dir";
      };
    };
  };

  config = {
    programs.nixvim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      defaultEditor = true;

      enableMan = true;

      # globals and options shamelessly stolen from AstroNvim
      globals = {
        mapleader = " "; # set leader key
        localleader = ","; # set default local leader key
        # AstroNvim specific global options
        max_file = {
          size = 1024 * 100;
          lines = 10000;
        }; # set global limits for large files
        autoformat_enabled = true; # enable or disable auto formatting at start (lsp.formatting.format_on_save must be enabled)
        autopairs_enabled = true; # enable autopairs at start
        cmp_enabled = true; # enable completion at start
        codelens_enabled = true; # enable or disable automatic codelens refreshing for lsp that support it
        diagnostics_mode = 3; # set the visibility of diagnostics in the UI (0=off, 1=only show in status line, 2=virtual text off, 3=all on)
        highlighturl_enabled = true; # highlight URLs by default
        icons_enabled = true; # disable icons in the UI (disable if no nerd font is available)
        inlay_hints_enabled = true; # enable or disable LSP inlay hints on startup (Neovim v0.10 only)
        lsp_handlers_enabled = true; # enable or disable default vim.lsp.handlers (hover and signature help)
        semantic_tokens_enabled = true; # enable or disable LSP semantic tokens on startup
        ui_notifications_enabled = true; # disable notifications (TODO: rename to  notifications_enabled in AstroNvim v4)
        git_worktrees = null; # enable git integration for detached worktrees (specify a table where each entry is of the form { toplevel = vim.env.HOME, gitdir=vim.env.HOME .. "/.dotfiles" })
      };

      opts = {
        # Use a blocky cursor always
        guicursor = "";

        # Show line numbers
        number = true;
        # Make them relative
        relativenumber = true;

        # Use sensible indenting
        tabstop = 2;
        softtabstop = 2;
        shiftwidth = 2;
        expandtab = true;

        # Try to use smart indent for new lines
        smartindent = true;

        # Don't wrap lines
        wrap = false;

        # Don't create local swap and backup files ...
        swapfile = false;
        backup = false;
        # ... but keep them in a global directory instead
        undodir = "/home/${config.neovim-custom.username-undodir}/.local/state/nvim/undodir";
        undofile = true;

        # Don't highlight all matches for search
        hlsearch = false;
        # Show matches as we type
        incsearch = true;

        # Enable 24 bit colors in the terminal
        termguicolors = true;

        # Never show more than 8 empty rows on the bottom if possible
        scrolloff = 8;
        # Always show the sign column to prevent jumpiness
        signcolumn = "yes";

        # Idleness in milliseconds to update the swap file for recovery
        updatetime = 50;

        # Mark the holy column
        colorcolumn = "80";

        # Timeout, e.g. for which-key
        timeout = true;
        timeoutlen = 0;
      };

      colorscheme = "catppuccin";

      colorschemes = {
        catppuccin = {
          enable = true;
          settings = {
            flavour = "mocha";
          };
        };

        tokyonight = {
          enable = true;
        };
      };

      keymaps = [
        # Tab navigation
        {
          action = helpers.mkRaw "function() vim.cmd.tabnext() end";
          key = "]t";
          mode = ["n"];
          options = {
            desc = "Next tab";
          };
        }
        {
          action = helpers.mkRaw "function() vim.cmd.tabprevious() end";
          key = "[t";
          mode = ["n"];
          options = {
            desc = "Prev tab";
          };
        }
        # Luasnip
        {
          action = helpers.mkRaw ''function() require("luasnip").expand() end'';
          key = "<C-K>";
          mode = ["i"];
          options = {
            desc = "Expand snippet";
          };
        }
        {
          action = helpers.mkRaw ''function() require("luasnip").jump(1) end'';
          key = "<C-L>";
          mode = ["i" "s"];
          options = {
            desc = "Next snippet";
          };
        }
        {
          action = helpers.mkRaw ''function() require("luasnip").jump(-1) end'';
          key = "<C-J>";
          mode = ["i" "s"];
          options = {
            desc = "Prev snippet";
          };
        }
        {
          action = helpers.mkRaw ''function() local ls = require("luasnip"); if ls.choice_active() then ls.change_choice(1) end end'';
          key = "<C-E>";
          mode = ["i" "s"];
          options = {
            desc = "Change active choice";
            silent = true;
          };
        }
        # Telescope
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").resume() end'';
          key = "<leader>f<CR>";
          mode = ["n"];
          options = {
            desc = "Resume previous search";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").buffers() end'';
          key = "<leader>fb";
          mode = ["n"];
          options = {
            desc = "Find buffers";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").grep_string() end'';
          key = "<leader>fc";
          mode = ["n"];
          options = {
            desc = "Find word under cursor";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").commands() end'';
          key = "<leader>fC";
          mode = ["n"];
          options = {
            desc = "Find commands";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").find_files() end'';
          key = "<leader>ff";
          mode = ["n"];
          options = {
            desc = "Find files";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").find_files { hidden = true, no_ignore = true } end'';
          key = "<leader>fF";
          mode = ["n"];
          options = {
            desc = "Find all files";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").help_tags() end'';
          key = "<leader>fh";
          mode = ["n"];
          options = {
            desc = "Find help";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").keymaps() end'';
          key = "<leader>fk";
          mode = ["n"];
          options = {
            desc = "Find keymaps";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").man_pages() end'';
          key = "<leader>fm";
          mode = ["n"];
          options = {
            desc = "Find man";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").oldfiles() end'';
          key = "<leader>fo";
          mode = ["n"];
          options = {
            desc = "Find history";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").registers() end'';
          key = "<leader>fr";
          mode = ["n"];
          options = {
            desc = "Find registers";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").colorscheme { enable_preview = true } end'';
          key = "<leader>ft";
          mode = ["n"];
          options = {
            desc = "Find themes";
          };
        }
        {
          action = helpers.mkRaw ''function() require("telescope.builtin").live_grep() end'';
          key = "<leader>fw";
          mode = ["n"];
          options = {
            desc = "Find words";
          };
        }
        # Harpoon
        {
          action = helpers.mkRaw ''function() require("harpoon"):list():add() end'';
          key = "<leader>a";
          mode = ["n"];
          options = {
            desc = "Add buffer to harpoon";
          };
        }
        {
          action = helpers.mkRaw ''
            function()
              local harpoon = require("harpoon")
              harpoon.ui:toggle_quick_menu(harpoon:list())
            end'';
          key = "<C-e>";
          mode = ["n"];
          options = {
            desc = "Show harpoon menu";
          };
        }
        {
          action = helpers.mkRaw ''function() require("harpoon"):list():select(1) end'';
          key = "<C-h>";
          mode = ["n"];
          options = {
            desc = "Show harpoon item 1";
          };
        }
        {
          action = helpers.mkRaw ''function() require("harpoon"):list():select(2) end'';
          key = "<C-t>";
          mode = ["n"];
          options = {
            desc = "Show harpoon item 2";
          };
        }
        {
          action = helpers.mkRaw ''function() require("harpoon"):list():select(3) end'';
          key = "<C-n>";
          mode = ["n"];
          options = {
            desc = "Show harpoon item 3";
          };
        }
        {
          action = helpers.mkRaw ''function() require("harpoon"):list():select(4) end'';
          key = "<C-s>";
          mode = ["n"];
          options = {
            desc = "Show harpoon item 4";
          };
        }
        {
          action = helpers.mkRaw ''function() require("harpoon"):list():replace_at(1) end'';
          key = "<leader><C-h>";
          mode = ["n"];
          options = {
            desc = "Set harpoon item 1";
          };
        }
        {
          action = helpers.mkRaw ''function() require("harpoon"):list():replace_at(2) end'';
          key = "<leader><C-t>";
          mode = ["n"];
          options = {
            desc = "Set harpoon item 2";
          };
        }
        {
          action = helpers.mkRaw ''function() require("harpoon"):list():replace_at(3) end'';
          key = "<leader><C-n>";
          mode = ["n"];
          options = {
            desc = "Set harpoon item 3";
          };
        }
        {
          action = helpers.mkRaw ''function() require("harpoon"):list():replace_at(4) end'';
          key = "<leader><C-s>";
          mode = ["n"];
          options = {
            desc = "Set harpoon item 4";
          };
        }
        # CHADTree
        {
          action = "<cmd>CHADopen<CR>";
          key = "<leader>o";
          mode = ["n"];
          options = {
            desc = "Toggle CHADtree";
          };
        }
        # netrw
        {
          action = "<cmd>vim.cmd.Ex<CR>";
          key = "<leader>pv";
          mode = ["n"];
          options = {
            desc = "Open netrw";
          };
        }
        # Navigating should keep the screen centered
        {
          action = "<C-f>zz";
          key = "<C-f>";
          mode = ["n"];
        }
        {
          action = "<C-b>zz";
          key = "<C-b>";
          mode = ["n"];
        }
        {
          action = "<C-u>zz";
          key = "<C-u>";
          mode = ["n"];
        }
        {
          action = "<C-d>zz";
          key = "<C-d>";
          mode = ["n"];
        }
        {
          action = "<C-u>zz";
          key = "<C-u>";
          mode = ["n"];
        }
        {
          action = "<cmd>lnext<CR>zz";
          key = "<leader>k";
          mode = ["n"];
        }
        {
          action = "<cmd>lprev<CR>zz";
          key = "<leader>j";
          mode = ["n"];
        }
        {
          action = "<cmd>cnext<CR>zz";
          key = "<C-k>";
          mode = ["n"];
        }
        {
          action = "<cmd>cprev<CR>zz";
          key = "<C-j>";
          mode = ["n"];
        }
        {
          action = "nzzzv";
          key = "nz";
          mode = ["n"];
        }
        {
          action = "Nzzzv";
          key = "Nz";
          mode = ["n"];
        }
        # Keep the yanked contents for pasting
        {
          action = ''"_dP'';
          key = "<leader>p";
          mode = "x";
        }
        # Disable accidential macro recording
        {
          action = "<nop>";
          key = "Q";
          mode = "n";
        }
        # Format current buffer
        {
          action = helpers.mkRaw ''function() vim.lsp.format() end'';
          key = "<leader>fg";
          mode = "n";
          options = {
            desc = "Format current buffer";
          };
        }
        # Stay on the current cursor position when joining lines from below
        {
          action = "mzJ`z";
          key = "J";
          mode = "n";
        }
        # Alternative to ESC
        {
          action = "<Esc>";
          key = "<C-c>";
          mode = "i";
        }
      ];

      plugins = {
        # Greeter
        # https://github.com/goolord/alpha-nvim
        alpha = {
          enable = true;
          theme = "dashboard";
        };

        # Nice file trees
        # https://github.com/ms-jpq/chadtree
        chadtree = {
          enable = true;
        };

        # Completion plugin
        # https://github.com/hrsh7th/nvim-cmp/
        cmp = {
          enable = true;
          settings = {
            mapping = {
              "<C-Space>" = ''cmp.mapping.complete()'';
              "<CR>" = ''cmp.mapping.confirm({ select = true })'';
            };

            sources = [
              {
                name = "nvim_lsp";
              }
              {
                name = "cmp_luasnip";
              }
              {
                name = "buffer";
              }
            ];
          };
        };

        # Completion for buffer words
        # https://github.com/hrsh7th/cmp-buffer
        cmp-buffer = {
          enable = true;
        };

        cmp_luasnip = {
          enable = true;
        };

        # EasyMotion-like navigation
        # https://github.com/folke/flash.nvim
        flash = {
          enable = true;
        };

        # Super fast navigation between a few marked files
        # https://github.com/ThePrimeagen/harpoon
        harpoon = {
          enable = true;
          enableTelescope = true;
          package = pkgs.vimPlugins.harpoon2;
        };

        # EasyMotion-like plugin for faster navigation
        # https://github.com/smoka7/hop.nvim
        #hop = {
        #  enable = true;
        #};

        # LSP plugin
        lsp = {
          enable = true;
          servers = {
            # Golang
            gopls = {
              enable = true;
              autostart = true;
            };
            golangci-lint-ls = {
              enable = true;
              autostart = true;
            };

            # Lua
            lua-ls = {
              enable = true;
            };

            # Nix
            nixd = {
              enable = true;
            };
          };
        };

        # Formatting on save using LSP servers
        # https://github.com/lukas-reineke/lsp-format.nvim
        lsp-format = {
          enable = true;
        };

        luasnip = {
          enable = true;

          settings = {
            enable_autosnippets = true;
            store_selection_keys = "<Tab>";
          };

          fromLua = [
            {
              paths = luaSnippetsPath;
            }
          ];
        };

        orgmode = {
          enable = true;
          settings = {
            org_agenda_files = "~/org/**/*";
            org_default_notes_file = "~/org/refile.org";

            mappings = {
              normal = {
                org_todo = "<leader>cit";
                org_todo_prev = "<leader>ciT";
              };
            };
          };
        };

        treesitter = {
          enable = true;
        };

        treesitter-context = {
          enable = true;

          settings = {
            line_numbers = true;
          };
        };

        trim = {
          enable = true;
        };

        # Move inside of words, e.g. with camelCase, kebab-case etc.
        # https://github.com/chrisgrieser/nvim-spider
        # Disabled for now: Weird behavior inside of strings
        #spider = {
        #  enable = true;
        #  keymaps.motions = {
        #    b = "b";
        #    e = "e";
        #    w = "w";
        #  };
        #};

        # Fuzzy finder for lists
        # https://github.com/nvim-telescope/telescope.nvim/
        telescope = {
          enable = true;
        };

        # Pretty diagnostics, references, ...
        # https://github.com/folke/trouble.nvim/
        trouble = {
          enable = true;
        };

        undotree = {
          enable = true;
        };

        which-key = {
          enable = true;
        };
      };

      extraConfigLua = ''
        vim.fn.sign_define(
          "DiagnosticsSignError",
          { texthl = "DiagnosticSignError", text = "󰗖", numhl = "DiagnosticSignError" }
        )
        vim.fn.sign_define(
          "DiagnosticSignWarning",
          { texthl = "DiagnosticSignWarning", text = "󰀪", numhl = "DiagnosticSignWarning" }
        )
        vim.fn.sign_define(
          "DiagnosticSignHint",
          { texthl = "DiagnosticSignHint", text = "󰌶", numhl = "DiagnosticSignHint" }
        )
        vim.fn.sign_define(
          "DiagnosticSignInformation",
          { texthl = "DiagnosticSignInformation", text = "", numhl = "DiagnosticSignInformation" }
        )
      '';
    };
  };
}
