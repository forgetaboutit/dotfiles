{...}: {
  programs.wezterm = {
    enable = true;
    enableZshIntegration = true;
    # Config is written in Lua
    extraConfig = ''
      -- Create a configuration object.
      local config = wezterm.config_builder()

      config.font = wezterm.font("MonaspiceNe Nerd Font Mono")
      config.font_rules = {
        {
          italic = true,
          font = wezterm.font("MonaspiceRn Nerd Font Mono")
        },
        {
          intensity = "Half",
          italic = true,
          font = wezterm.font("MonaspiceRn Nerd Font Mono")
        },
        {
          intensity = "Bold",
          italic = true,
          font = wezterm.font("MonaspiceRn Nerd Font Mono")
        },
        {
          intensity = "Normal",
          italic = true,
          font = wezterm.font("MonaspiceRn Nerd Font Mono")
        },
      }

      config.harfbuzz_features = { "ss01", "ss02", "ss03", "ss04", "ss05", "ss06", "ss07", "ss08", "calt", "dlig" }
      config.color_scheme = "Tokyo Night"
      config.audible_bell = "Disabled"

      wezterm.on('update-right-status', function(window, pane)
        local name = window:active_key_table()
        if name then
          name = 'TABLE: ' .. name
        end

        window:set_right_status(name or "")
      end)

      local act = wezterm.action

      config.leader = { key = 'Space', mods = 'CTRL' }
      config.keys = {
        {
          key = 'p',
          mods = 'LEADER',
          action = act.ActivateKeyTable {
            name = 'activate_panes',
            timeout_milliseconds = 3000,
          }
        },
        {
          key = 'c',
          mods = 'LEADER',
          action = act.ActivateKeyTable {
            name = 'control_tabs',
            timeout_milliseconds = 3000,
          }
        },
        {
          key = 'h',
          mods = 'LEADER',
          action = act.ActivatePaneDirection "Left"
        },
        {
          key = 'j',
          mods = 'LEADER',
          action = act.ActivatePaneDirection "Down"
        },
        {
          key = 'k',
          mods = 'LEADER',
          action = act.ActivatePaneDirection "Up"
        },
        {
          key = 'l',
          mods = 'LEADER',
          action = act.ActivatePaneDirection "Right"
        },
        {
          key = 'h',
          mods = 'LEADER',
          action = act.ActivatePaneDirection 'Left'
        },
        {
          key = 'l',
          mods = 'LEADER',
          action = act.ActivatePaneDirection 'Right'
        },
        {
          key = 'w',
          mods = 'LEADER',
          action = act.CloseCurrentTab { confirm = true },
        },
        {
          key = 'c',
          mods = 'LEADER',
          action = act.SpawnTab "DefaultDomain"
        },
        {
          key = 'v',
          mods = 'LEADER',
          action = act.SplitVertical { domain = "CurrentPaneDomain" },
        },
        {
          key = 'x',
          mods = 'LEADER',
          action = act.SplitHorizontal { domain = "CurrentPaneDomain" },
        },
      }

      config.key_tables = {
        activate_pane = {
          {
            key = 'h',
            action = act.ActivatePaneDirection 'Left'
          },
          {
            key = 'l',
            action = act.ActivatePaneDirection 'Right'
          }
        },
        control_tabs = {
          {
            key = 'w',
            action = act.CloseCurrentTab { confirm = true },
          },
          {
            key = 'c',
            action = act.SpawnTab "DefaultDomain"
          },
          {
            key = 'v',
            action = act.SplitVertical { domain = "CurrentPaneDomain" },
          },
          {
            key = 'h',
            action = act.SplitHorizontal { domain = "CurrentPaneDomain" },
          },
        }
      }

      local SOLID_RIGHT_ARROW = wezterm.nerdfonts.pl_left_hard_divider

      --config.tab_bar_style = {
      --  active_tab_left = wezterm.format {
      --    { Background = { Color = '#0b0022' } },
      --    { Foreground = { Color = '#2b2042' } },
      --    { Text = SOLID_LEFT_ARROW },
      --  },
      --  active_tab_right = wezterm.format {
      --    { Background = { Color = '#0b0022' } },
      --    { Foreground = { Color = '#2b2042' } },
      --    { Text = SOLID_RIGHT_ARROW },
      --  },
      --  inactive_tab_left = wezterm.format {
      --    { Background = { Color = '#0b0022' } },
      --    { Foreground = { Color = '#1b1032' } },
      --    { Text = SOLID_LEFT_ARROW },
      --  },
      --  inactive_tab_right = wezterm.format {
      --    { Background = { Color = '#0b0022' } },
      --    { Foreground = { Color = '#1b1032' } },
      --    { Text = SOLID_RIGHT_ARROW },
      --  },
      --}

      config.use_fancy_tab_bar = false
      config.tab_bar_at_bottom = true
      config.disable_default_key_bindings = true
      config.window_background_opacity = 0.9

      -- Wezterm expects a config object
      return config
    '';
  };
}
