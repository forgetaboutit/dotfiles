{
  pkgs,
  inputs,
  ...
}: {
  programs.wezterm = {
    enable = true;
    package = inputs.wezterm.packages.${pkgs.system}.default;
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
        -- Each element holds the text for a cell in a "powerline" style << fade
        local cells = {}

        -- Figure out the cwd and host of the current pane.
        -- This will pick up the hostname for the remote host if your
        -- shell is using OSC 7 on the remote host.
        local cwd_uri = pane:get_current_working_dir()

        if cwd_uri then
          local cwd = ""
          local hostname = ""

          if type(cwd_uri) == 'userdata' then
            -- Running on a newer version of wezterm and we have
            -- a URL object here, making this simple!
            cwd = cwd_uri.file_path
            hostname = cwd_uri.host or wezterm.hostname()
          else
            -- an older version of wezterm, 20230712-072601-f4abf8fd or earlier,
            -- which doesn't have the Url object
            cwd_uri = cwd_uri:sub(8)
            local slash = cwd_uri:find '/'

            if slash then
              hostname = cwd_uri:sub(1, slash - 1)

              -- and extract the cwd from the uri, decoding %-encoding
              cwd = cwd_uri:sub(slash):gsub('%%(%x%x)', function(hex)
                return string.char(tonumber(hex, 16))
              end)
            end
          end

          -- Remove the domain name portion of the hostname
          local dot = hostname:find '[.]'

          if dot then
            hostname = hostname:sub(1, dot - 1)
          end

          if hostname == "" then
            hostname = wezterm.hostname()
          end

          table.insert(cells, cwd)
          table.insert(cells, hostname)
        end

        local date = wezterm.strftime '%Y-%m-%d %H:%M:%S'
        table.insert(cells, date)

        -- An entry for each battery (typically 0 or 1 battery)
        for _, b in ipairs(wezterm.battery_info()) do
          table.insert(cells, string.format('%.0f%%', b.state_of_charge * 100))
        end

        -- The powerline < symbol
        local LEFT_ARROW = utf8.char(0xe0b3)

        -- The filled in variant of the < symbol
        local SOLID_LEFT_ARROW = utf8.char(0xe0b2)

        -- Color palette for the backgrounds of each cell
        local colors = {
          '#3c1361',
          '#52307c',
          '#663a82',
          '#7c5295',
          '#b491c8',
        }

        -- Foreground color for the text across the fade
        local text_fg = '#c0c0c0'

        -- The elements to be formatted
        local elements = {}
        -- How many cells have been formatted
        local num_cells = 0

        -- Translate a cell into elements
        function push(text, is_last)
          local cell_no = num_cells + 1
          table.insert(elements, { Foreground = { Color = text_fg } })
          table.insert(elements, { Background = { Color = colors[cell_no] } })
          table.insert(elements, { Text = ' ' .. text .. ' ' })

          if not is_last then
            table.insert(elements, { Foreground = { Color = colors[cell_no + 1] } })
            table.insert(elements, { Text = SOLID_LEFT_ARROW })
          end

          num_cells = num_cells + 1
        end

        while #cells > 0 do
          local cell = table.remove(cells, 1)
          push(cell, #cells == 0)
        end

        window:set_right_status(wezterm.format(elements))
      end)

      local act = wezterm.action

      config.leader = { key = 'Space', mods = 'CTRL' }
      config.keys = {
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
          action = act.ActivatePaneDirection "Up",
        },
        {
          key = 'l',
          mods = 'LEADER',
          action = act.ActivatePaneDirection "Right",
        },
        {
          key = 'p',
          mods = 'LEADER',
          action = act.ActivateTabRelative(-1),
        },
        {
          key = 'n',
          mods = 'LEADER',
          action = act.ActivateTabRelative(1),
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

      for i = 1, 9 do
        table.insert(config.keys, {
          key = tostring(i),
          mods = 'LEADER',
          action = act.ActivateTab(i - 1),
        })
      end

      config.use_fancy_tab_bar = false
      config.tab_bar_at_bottom = true
      config.disable_default_key_bindings = true
      config.window_background_opacity = 0.95

      config.enable_wayland = true

      -- Wezterm expects a config object
      return config
    '';
  };
}
