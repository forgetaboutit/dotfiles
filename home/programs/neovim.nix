{
  pkgs,
  lib,
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
in {
  programs.nixvim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    defaultEditor = true;

    enableMan = true;

    extraLuaPackages = pkgs:
      with pkgs; [
        luajitPackages.lua-utils-nvim
      ];

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
      undodir = "/home/sammy/.local/state/nvim/undodir";
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

    colorscheme = "tokyonight";

    colorschemes = {
      tokyonight = {
        enable = true;
      };
    };

    keymaps = [
      # Tab navigation
      {
        action = "function() vim.cmd.tabnext() end";
        lua = true;
        key = "]t";
        mode = ["n"];
        options = {
          desc = "Next tab";
        };
      }
      {
        action = "function() vim.cmd.tabprevious() end";
        lua = true;
        key = "[t";
        mode = ["n"];
        options = {
          desc = "Prev tab";
        };
      }
      # Telescope
      {
        action = ''function() require("telescope.builtin").resume() end'';
        lua = true;
        key = "<leader>f<CR>";
        mode = ["n"];
        options = {
          desc = "Resume previous search";
        };
      }
      {
        action = ''function() require("telescope.builtin").buffers() end'';
        lua = true;
        key = "<leader>fb";
        mode = ["n"];
        options = {
          desc = "Find buffers";
        };
      }
      {
        action = ''function() require("telescope.builtin").grep_string() end'';
        lua = true;
        key = "<leader>fc";
        mode = ["n"];
        options = {
          desc = "Find word under cursor";
        };
      }
      {
        action = ''function() require("telescope.builtin").commands() end'';
        lua = true;
        key = "<leader>fC";
        mode = ["n"];
        options = {
          desc = "Find commands";
        };
      }
      {
        action = ''function() require("telescope.builtin").find_files() end'';
        lua = true;
        key = "<leader>ff";
        mode = ["n"];
        options = {
          desc = "Find files";
        };
      }
      {
        action = ''function() require("telescope.builtin").find_files { hidden = true, no_ignore = true } end'';
        lua = true;
        key = "<leader>fF";
        mode = ["n"];
        options = {
          desc = "Find all files";
        };
      }
      {
        action = ''function() require("telescope.builtin").help_tags() end'';
        lua = true;
        key = "<leader>fh";
        mode = ["n"];
        options = {
          desc = "Find help";
        };
      }
      {
        action = ''function() require("telescope.builtin").keymaps() end'';
        lua = true;
        key = "<leader>fk";
        mode = ["n"];
        options = {
          desc = "Find keymaps";
        };
      }
      {
        action = ''function() require("telescope.builtin").man_pages() end'';
        lua = true;
        key = "<leader>fm";
        mode = ["n"];
        options = {
          desc = "Find man";
        };
      }
      {
        action = ''function() require("telescope.builtin").oldfiles() end'';
        lua = true;
        key = "<leader>fo";
        mode = ["n"];
        options = {
          desc = "Find history";
        };
      }
      {
        action = ''function() require("telescope.builtin").registers() end'';
        lua = true;
        key = "<leader>fr";
        mode = ["n"];
        options = {
          desc = "Find registers";
        };
      }
      {
        action = ''function() require("telescope.builtin").colorscheme { enable_preview = true } end'';
        lua = true;
        key = "<leader>ft";
        mode = ["n"];
        options = {
          desc = "Find themes";
        };
      }
      {
        action = ''function() require("telescope.builtin").live_grep() end'';
        lua = true;
        key = "<leader>fw";
        mode = ["n"];
        options = {
          desc = "Find words";
        };
      }
      # Harpoon
      {
        action = ''function() require("harpoon"):list():add() end'';
        lua = true;
        key = "<leader>a";
        mode = ["n"];
        options = {
          desc = "Add buffer to harpoon";
        };
      }
      {
        action = ''
          function()
            local harpoon = require("harpoon")
            harpoon.ui:toggle_quick_menu(harpoon:list())
          end'';
        lua = true;
        key = "<C-e>";
        mode = ["n"];
        options = {
          desc = "Show harpoon menu";
        };
      }
      {
        action = ''function() require("harpoon"):list():select(1) end'';
        lua = true;
        key = "<C-h>";
        mode = ["n"];
        options = {
          desc = "Show harpoon item 1";
        };
      }
      {
        action = ''function() require("harpoon"):list():select(2) end'';
        lua = true;
        key = "<C-t>";
        mode = ["n"];
        options = {
          desc = "Show harpoon item 2";
        };
      }
      {
        action = ''function() require("harpoon"):list():select(3) end'';
        lua = true;
        key = "<C-n>";
        mode = ["n"];
        options = {
          desc = "Show harpoon item 3";
        };
      }
      {
        action = ''function() require("harpoon"):list():select(4) end'';
        lua = true;
        key = "<C-s>";
        mode = ["n"];
        options = {
          desc = "Show harpoon item 4";
        };
      }
      {
        action = ''function() require("harpoon"):list():replace_at(1) end'';
        lua = true;
        key = "<leader><C-h>";
        mode = ["n"];
        options = {
          desc = "Set harpoon item 1";
        };
      }
      {
        action = ''function() require("harpoon"):list():replace_at(2) end'';
        lua = true;
        key = "<leader><C-t>";
        mode = ["n"];
        options = {
          desc = "Set harpoon item 2";
        };
      }
      {
        action = ''function() require("harpoon"):list():replace_at(3) end'';
        lua = true;
        key = "<leader><C-n>";
        mode = ["n"];
        options = {
          desc = "Set harpoon item 3";
        };
      }
      {
        action = ''function() require("harpoon"):list():replace_at(4) end'';
        lua = true;
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
        action = "function() vim.lsp.format() end";
        lua = true;
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
          # Lua
          lua-ls = {
            enable = true;
          };

          # Nix
          nixd = {
            enable = true;
          };

          # Typescript
          tsserver = {
            enable = true;
          };
        };
      };

      # Formatting on save using LSP servers
      # https://github.com/lukas-reineke/lsp-format.nvim
      lsp-format = {
        enable = true;
      };

      # neorg: org for neovim
      # https://github.com/nvim-neorg/neorg
      neorg = {
        # Disabled for now until https://github.com/NixOS/nixpkgs/pull/302442
        # is fixed
        enable = false;
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

      -- Remove trailing whitespace on save
      vim.api.nvim_create_autocmd({ "BufWritePre" }, {
        pattern = { "*" },
        command = [[%s/\s\+$//e]],
      })
    '';
  };

  #programs.neovim = {
  #  enable = true;
  #  viAlias = true;
  #  vimAlias = true;
  #};

  #programs.nixneovim = {
  #  enable = true;
  #  viAlias = true;
  #  vimAlias = true;

  #  usePluginDefaults = true;
  #  colorscheme = "astrotheme";

  #  # globals and options shamelessly stolen from AstroNvim
  #  globals = {
  #    mapleader = " "; # set leader key
  #    maplocalleader = ","; # set default local leader key
  #    # AstroNvim specific global options
  #    max_file = {
  #      size = 1024 * 100;
  #      lines = 10000;
  #    }; # set global limits for large files
  #    autoformat_enabled = true; # enable or disable auto formatting at start (lsp.formatting.format_on_save must be enabled)
  #    autopairs_enabled = true; # enable autopairs at start
  #    cmp_enabled = true; # enable completion at start
  #    codelens_enabled = true; # enable or disable automatic codelens refreshing for lsp that support it
  #    diagnostics_mode = 3; # set the visibility of diagnostics in the UI (0=off, 1=only show in status line, 2=virtual text off, 3=all on)
  #    highlighturl_enabled = true; # highlight URLs by default
  #    icons_enabled = true; # disable icons in the UI (disable if no nerd font is available)
  #    inlay_hints_enabled = false; # enable or disable LSP inlay hints on startup (Neovim v0.10 only)
  #    lsp_handlers_enabled = true; # enable or disable default vim.lsp.handlers (hover and signature help)
  #    semantic_tokens_enabled = true; # enable or disable LSP semantic tokens on startup
  #    ui_notifications_enabled = true; # disable notifications (TODO: rename to  notifications_enabled in AstroNvim v4)
  #    git_worktrees = null; # enable git integration for detached worktrees (specify a table where each entry is of the form { toplevel = vim.env.HOME, gitdir=vim.env.HOME .. "/.dotfiles" })
  #  };

  #  options = {
  #    # show whitespace characters
  #    list = true;
  #    # visualize all whitespace characters in neovim
  #    listchars = "tab:»·,trail:·,extends:→,precedes:←,nbsp:␣";
  #    # enable whitespace characters
  #    showbreak = "↪";

  #    breakindent = true; # wrap indent to match  line start
  #    clipboard = "unnamedplus"; # connection to the system clipboard
  #    cmdheight = 0; # hide command line unless needed
  #    #completeopt = { "menu"; "menuone", "noselect" }, # Options for insert mode completion
  #    copyindent = true; # copy the previous indentation on autoindenting
  #    cursorline = true; # highlight the text line of the cursor
  #    expandtab = true; # enable the use of space in tab
  #    fileencoding = "utf-8"; # file content encoding for the buffer
  #    #fillchars = { eob = " " }; # disable `~` on nonexistent lines
  #    foldenable = true; # enable fold for nvim-ufo
  #    foldlevel = 99; # set high foldlevel for nvim-ufo
  #    foldlevelstart = 99; # start with all code unfolded
  #    #foldcolumn = true; # show foldcolumn
  #    history = 100; # number of commands to remember in a history table
  #    ignorecase = true; # case insensitive searching
  #    infercase = true; # infer cases in keyword completion
  #    laststatus = 3; # global statusline
  #    linebreak = true; # wrap lines at 'breakat'
  #    mouse = "a"; # enable mouse support
  #    number = true; # show numberline
  #    preserveindent = true; # preserve indent structure as much as possible
  #    pumheight = 10; # height of the pop up menu
  #    relativenumber = true; # show relative numberline
  #    shiftwidth = 2; # number of space inserted for indentation
  #    showmode = false; # disable showing modes in command line
  #    showtabline = 2; # always display tabline
  #    signcolumn = "yes"; # always show the sign column
  #    smartcase = true; # case sensitive searching
  #    splitbelow = true; # splitting a new window below the current one
  #    splitright = true; # splitting a new window at the right of the current one
  #    tabstop = 2; # number of space in a tab
  #    termguicolors = true; # enable 24-bit RGB color in the TUI
  #    timeoutlen = 500; # shorten key timeout length a little bit for which-key
  #    title = true; # set terminal title to the filename and path
  #    undofile = true; # enable persistent undo
  #    updatetime = 300; # length of time to wait before triggering the plugin
  #    virtualedit = "block"; # allow going past end of line in visual block mode
  #    wrap = false; # disable wrapping of lines longer than the width of window
  #    writebackup = false; # disable making a backup before overwriting a file
  #  };

  #  mappings = {
  #    normal =
  #      {
  #        # Tab navigation
  #        "]t" = {
  #          action = "function() vim.cmd.tabnext() end";
  #          desc = "Next tab";
  #        };
  #        "[t" = {
  #          action = "function() vim.cmd.tabprevious() end";
  #          desc = "Previous tab";
  #        };

  #        # NeoTree
  #        "<>e" = {
  #          action = ''"<cmd>Neotree toggle<cr>"'';
  #          desc = "Toggle explorer";
  #        };
  #        "<leader>o" = {
  #          action = ''
  #            function()
  #              if vim.bo.filetype == "neo-tree" then
  #                vim.cmd.wincmd "p"
  #              else
  #                vim.cmd.Neotree "focus"
  #              end
  #            end'';
  #          desc = "Toggle explorer focus";
  #        };
  #        "<leader>f<CR>" = {
  #          action = ''function() require("telescope.builtin").resume() end'';
  #          desc = "Resume previous search";
  #        };
  #        "<leader>fb" = {
  #          action = ''function() require("telescope.builtin").buffers() end'';
  #          desc = "Find buffers";
  #        };
  #        "<leader>fc" = {
  #          action = ''function() require("telescope.builtin").grep_string() end'';
  #          desc = "Find word under cursor";
  #        };
  #        "<leader>fC" = {
  #          action = ''function() require("telescope.builtin").commands() end'';
  #          desc = "Find commands";
  #        };
  #        "<leader>ff" = {
  #          action = ''function() require("telescope.builtin").find_files() end'';
  #          desc = "Find files";
  #        };
  #        "<leader>fF" = {
  #          action = ''function() require("telescope.builtin").find_files { hidden = true, no_ignore = true } end'';
  #          desc = "Find all files";
  #        };
  #        "<leader>fh" = {
  #          action = ''function() require("telescope.builtin").help_tags() end'';
  #          desc = "Find help";
  #        };
  #        "<leader>fk" = {
  #          action = ''function() require("telescope.builtin").keymaps() end'';
  #          desc = "Find keymaps";
  #        };
  #        "<leader>fm" = {
  #          action = ''function() require("telescope.builtin").man_pages() end'';
  #          desc = "Find man";
  #        };
  #        "<leader>fo" = {
  #          action = ''function() require("telescope.builtin").oldfiles() end'';
  #          desc = "Find history";
  #        };
  #        "<leader>fr" = {
  #          action = ''function() require("telescope.builtin").registers() end'';
  #          desc = "Find registers";
  #        };
  #        #  if is_available "nvim-notify" then
  #        #    maps.n["<leader>fn"] =
  #        #      { function() require("telescope").extensions.notify.notify() end, desc = "Find notifications" }
  #        #    maps.n["<leader>uD"] =
  #        #      { function() require("notify").dismiss { pending = true, silent = true } end, desc = "Dismiss notifications" }
  #        #  end
  #        #  maps.n["<leader>ft"] =
  #        #    { function() require("telescope.builtin").colorscheme { enable_preview = true } end, desc = "Find themes" }
  #        #  maps.n["<leader>fw"] = { function() require("telescope.builtin").live_grep() end, desc = "Find words" }
  #        #  maps.n["<leader>fW"] = {
  #        #    function()
  #        #      require("telescope.builtin").live_grep {
  #        #        additional_args = function(args) return vim.list_extend(args, { "--hidden", "--no-ignore" }) end,
  #        #      }
  #        #    end,
  #        #    desc = "Find words in all files",
  #        #  }
  #      }
  #      // sections;
  #  };

  #  plugins = {
  #    harpoon = {
  #      enable = true;
  #      enableTelescope = true;
  #    };

  #    lspkind = {
  #      enable = true;
  #      mode = "symbol_text";
  #    };

  #    lspformat = {
  #      enable = true;
  #    };

  #    lsp = {
  #      enable = true;

  #      servers = {
  #        tsserver = {
  #          enable = true;
  #          autostart = true;
  #          filetypes = [
  #            "javascript"
  #            "typescript"
  #            "nix"
  #          ];
  #        };
  #      };
  #    };

  #    telescope.enable = true;
  #    treesitter.enable = true;
  #    undotree.enable = true;
  #    which-key.enable = true;
  #    copilot.enable = true;
  #    nvim-dap.enable = true;
  #    #nvim-dap-ui.enable = true;
  #    nvim-lightbulb.enable = true;

  #    # 🚦A pretty diagnostics, references, telescope results, quickfix and
  #    # location list to help you solve all the trouble your code is causing.
  #    trouble.enable = true;
  #  };

  #  extraPlugins = [
  #    pkgs.vimPlugins.aerial-nvim
  #    pkgs.vimPlugins.astrotheme
  #    pkgs.vimPlugins.neo-tree-nvim
  #    pkgs.vimPlugins.omnisharp-extended-lsp-nvim
  #  ];

  #  extraConfigLua = ''
  #    -- Astrotheme requires a global setup. Otherwise loading a theme will
  #    -- not work.
  #    require("astrotheme").setup();
  #  '';

  #  extraLuaPostConfig = ''
  #    -- Aerial requires a setup call to work properly.
  #    require("aerial").setup();

  #    -- print all fields of the lua table
  #    function print_table(table)
  #      for k, v in pairs(table) do
  #        print(k, v)
  #      end
  #    end

  #    --print_table(require("omnisharp_extended"));

  #    -- Trouble
  #    require("trouble").setup {
  #    };

  #    -- Neotree configuration
  #    require("neo-tree").setup {
  #      auto_clean_after_session_restore = true,
  #      close_if_last_window = true,
  #      sources = { "filesystem", "buffers", "git_status" },
  #      source_selector = {
  #        winbar = true,
  #        content_layout = "center",
  #        sources = {
  #          { source = "filesystem", display_name = "${getIcon {name = "FolderClosed"; padding = 1;} + "File" }"},
  #          { source = "buffers", display_name = "${getIcon {name = "DefaultFile"; padding = 1;} + "Bufs" }"},
  #          { source = "git_status", display_name = "${getIcon {name = "Git"; padding = 1;} + "Git" }"},
  #          { source = "diagnostics", display_name = "${getIcon {name = "Diagnostic"; padding = 1;} + "Diagnostic" }"},
  #        },
  #      },
  #      default_component_configs = {
  #        indent = { padding = 0 },
  #        icon = {
  #          folder_closed = "${getIcon {name = "FolderClosed";}}",
  #          folder_open = "${getIcon {name = "FolderOpen";}}",
  #          folder_empty = "${getIcon {name = "FolderEmpty";}}",
  #          folder_empty_open = "${getIcon {name = "FolderEmpty";}}",
  #          default = "${getIcon {name = "DefaultFile";}}",
  #        },
  #        modified = { symbol = "${getIcon {name = "FileModified";}}" },
  #        git_status = {
  #          symbols = {
  #            added = "${getIcon {name = "GitAdd";}}",
  #            deleted = "${getIcon {name = "GitDelete";}}",
  #            modified = "${getIcon {name = "GitChange";}}",
  #            renamed = "${getIcon {name = "GitRenamed";}}",
  #            untracked = "${getIcon {name = "GitUntracked";}}",
  #            ignored = "${getIcon {name = "GitIgnored";}}",
  #            unstaged = "${getIcon {name = "GitUnstaged";}}",
  #            staged = "${getIcon {name = "GitStaged";}}",
  #            conflict = "${getIcon {name = "GitConflict";}}",
  #          },
  #        },
  #      },
  #      window = {
  #        width = 30,
  #        mappings = {
  #          ["<space>"] = false, -- disable space until we figure out which-key disabling
  #          ["[b"] = "prev_source",
  #          ["]b"] = "next_source",
  #          O = "system_open",
  #          Y = "copy_selector",
  #          h = "parent_or_close",
  #          l = "child_or_open",
  #          o = "open",
  #        },
  #        fuzzy_finder_mappings = { -- define keymaps for filter popup window in fuzzy_finder_mode
  #          ["<C-j>"] = "move_cursor_down",
  #          ["<C-k>"] = "move_cursor_up",
  #        },
  #      },
  #      filesystem = {
  #        follow_current_file = { enabled = true },
  #        hijack_netrw_behavior = "open_current",
  #        use_libuv_file_watcher = vim.fn.has "win32" ~= 1,
  #      },
  #      event_handlers = {
  #        {
  #          event = "neo_tree_buffer_enter",
  #          handler = function(_) vim.opt_local.signcolumn = "auto" end,
  #        },
  #      },
  #    };
  #  '';
  #};
}
