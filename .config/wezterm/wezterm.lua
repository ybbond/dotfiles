local wezterm = require('wezterm')
local act = wezterm.action

local isDark = wezterm.gui.get_appearance():find("Dark")

local colors = {
  dark = {
    cursor_fg = 'black',
    tab_bar = {
      active_tab = {
        bg_color = '#2e3440',
        fg_color = '#ffffff',
      },
      inactive_tab = {
        bg_color = '#111222',
        fg_color = '#ffffff',
      },
    },
  },
  light = {
    cursor_fg = 'white',
    tab_bar = {
      active_tab = {
        bg_color = '#ffffff',
        fg_color = '#000000',
      },
      inactive_tab = {
        bg_color = '#CECECE',
        fg_color = '#000000',
      },
    },
  },
}
local window_frame = {
  dark = {
    font = wezterm.font({ family = "Roboto" }),
    font_size = 12.0,
    active_titlebar_bg = "#111111",
    inactive_titlebar_bg = "#111111",
  },
  light = {
    font = wezterm.font({ family = "Roboto" }),
    font_size = 12.0,
    active_titlebar_bg = "#333333",
    inactive_titlebar_bg = "#333333",
  },
}

wezterm.on('augment-command-palette', function(window, pane)
  return {
    {
      brief = 'Rename tab',
      icon = 'md_rename_box',

      action = act.PromptInputLine {
        description = 'Enter new name for tab',
        action = wezterm.action_callback(function(window, pane, line)
          if line then
            window:active_tab():set_title(line)
          end
        end),
      },
    },
  }
end)

return {
  send_composed_key_when_left_alt_is_pressed = false,
  send_composed_key_when_right_alt_is_pressed = true,

  -- color_scheme = 'nord',
  color_scheme = isDark and 'Vs Code Dark+ (Gogh)' or 'Vs Code Light+ (Gogh)',
  font = wezterm.font('JetBrains Mono'),
  font_size = 12.0,
  harfbuzz_features = { 'calt=0' },

  disable_default_key_bindings = true,
  scrollback_lines = 10000,

  colors = isDark and colors.dark or colors.light,
  window_frame = isDark and window_frame.dark or window_frame.light,

  adjust_window_size_when_changing_font_size = false,

  window_decorations = "RESIZE",
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },

  exit_behavior = "Close",

  keys = {
    {key="p",mods="SUPER|CTRL|SHIFT",action=act.ActivateCommandPalette},

    {key="x",mods="SUPER|CTRL|SHIFT",action=act.ActivateCopyMode},
    {key="Space",mods="SUPER|CTRL|SHIFT",action=act.QuickSelect},

    {key="c",mods="SUPER",action=act.CopyTo("Clipboard")},
    {key="v",mods="SUPER",action=act.PasteFrom("Clipboard")},

    {key="f",mods="SUPER",action=act.Search{CaseSensitiveString=""}},
    {key="f",mods="SUPER|SHIFT",action=act.Search{CaseInSensitiveString=""}},

    {key="-",mods="SUPER",action=act.DecreaseFontSize},
    {key="=",mods="SUPER",action=act.IncreaseFontSize},
    {key="0",mods="SUPER",action=act.ResetFontSize},

    {key="w",mods="SUPER",action=act.CloseCurrentTab{confirm=true}},
    {key="t",mods="SUPER",action=act.SpawnTab("CurrentPaneDomain")},

    {key="{",mods="SUPER|SHIFT",action=act.ActivateTabRelative(-1)},
    {key="}",mods="SUPER|SHIFT",action=act.ActivateTabRelative(1)},
    {key="{",mods="SUPER|CTRL|SHIFT",action=act.MoveTabRelative(-1)},
    {key="}",mods="SUPER|CTRL|SHIFT",action=act.MoveTabRelative(1)},

    {key="y",mods="SUPER|CTRL",action=act.ScrollByPage(-1)},
    {key="e",mods="SUPER|CTRL",action=act.ScrollByPage(1)},

    {key="1",mods="SUPER",action=act.ActivateTab(0)},
    {key="2",mods="SUPER",action=act.ActivateTab(1)},
    {key="3",mods="SUPER",action=act.ActivateTab(2)},
    {key="4",mods="SUPER",action=act.ActivateTab(3)},
    {key="5",mods="SUPER",action=act.ActivateTab(4)},
    {key="6",mods="SUPER",action=act.ActivateTab(5)},
    {key="7",mods="SUPER",action=act.ActivateTab(6)},
    {key="8",mods="SUPER",action=act.ActivateTab(7)},
    {key="9",mods="SUPER",action=act.ActivateTab(-1)},
  },
  key_tables = {
    copy_mode = {
      {key="c", mods="CTRL", action=act.CopyMode("Close")},
      {key="g", mods="CTRL", action=act.CopyMode("Close")},
      {key="Escape", mods="NONE", action=act.CopyMode("Close")},

      {key="h", mods="NONE", action=act.CopyMode("MoveLeft")},
      {key="j", mods="NONE", action=act.CopyMode("MoveDown")},
      {key="k", mods="NONE", action=act.CopyMode("MoveUp")},
      {key="l", mods="NONE", action=act.CopyMode("MoveRight")},

      {key='RightArrow', mods='ALT', action=act.CopyMode("MoveForwardWord")},
      {key='f', mods='ALT', action=act.CopyMode("MoveForwardWord")},
      {key='w', mods='NONE', action=act.CopyMode("MoveForwardWord")},

      {key='LeftArrow', mods='ALT', action=act.CopyMode("MoveBackwardWord")},
      {key='b', mods='ALT', action=act.CopyMode("MoveBackwardWord")},
      {key='b', mods='NONE', action=act.CopyMode("MoveBackwardWord")},

      {key='0', mods='NONE', action=act.CopyMode("MoveToStartOfLine")},
      {key='Enter', mods='NONE', action=act.CopyMode("MoveToStartOfNextLine")},

      {key='$', mods='NONE', action=act.CopyMode("MoveToEndOfLineContent")},
      {key='$', mods='SHIFT', action=act.CopyMode("MoveToEndOfLineContent")},
      {key='^', mods='NONE', action=act.CopyMode("MoveToStartOfLineContent")},
      {key='^', mods='SHIFT', action=act.CopyMode("MoveToStartOfLineContent")},

      {key="0",     mods="NONE",  action=act.CopyMode("MoveToStartOfLine")},
      {key="Enter", mods="NONE",  action=act.CopyMode("MoveToStartOfNextLine")},

      {key="$",     mods="NONE",  action=act.CopyMode("MoveToEndOfLineContent")},
      {key="$",     mods="SHIFT", action=act.CopyMode("MoveToEndOfLineContent")},
      {key="^",     mods="NONE",  action=act.CopyMode("MoveToStartOfLineContent")},
      {key="^",     mods="SHIFT", action=act.CopyMode("MoveToStartOfLineContent")},
      {key="m",     mods="ALT",   action=act.CopyMode("MoveToStartOfLineContent")},

      {key=" ", mods="NONE",  action=act.CopyMode{SetSelectionMode="Cell"}},
      {key="v", mods="NONE",  action=act.CopyMode{SetSelectionMode="Cell"}},
      {key="V", mods="NONE",  action=act.CopyMode{SetSelectionMode="Line"}},
      {key="V", mods="SHIFT", action=act.CopyMode{SetSelectionMode="Line"}},
      {key="v", mods="CTRL",  action=act.CopyMode{SetSelectionMode="Block"}},

      {key="G", mods="NONE",  action=act.CopyMode("MoveToScrollbackBottom")},
      {key="G", mods="SHIFT", action=act.CopyMode("MoveToScrollbackBottom")},
      {key="g", mods="NONE",  action=act.CopyMode("MoveToScrollbackTop")},

      {key="H", mods="NONE",  action=act.CopyMode("MoveToViewportTop")},
      {key="H", mods="SHIFT", action=act.CopyMode("MoveToViewportTop")},
      {key="M", mods="NONE",  action=act.CopyMode("MoveToViewportMiddle")},
      {key="M", mods="SHIFT", action=act.CopyMode("MoveToViewportMiddle")},
      {key="L", mods="NONE",  action=act.CopyMode("MoveToViewportBottom")},
      {key="L", mods="SHIFT", action=act.CopyMode("MoveToViewportBottom")},

      {key="u", mods="CTRL", action=act.CopyMode("PageUp")},
      {key="d", mods="CTRL", action=act.CopyMode("PageDown")},
    },
  },

  enable_csi_u_key_encoding = true,
}
