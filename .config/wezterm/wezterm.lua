-- import wezterm
local wezterm = require('wezterm')

return {
  color_scheme = 'nord',
  font = wezterm.font('JetBrains Mono'),

  colors = {
    tab_bar = {
      active_tab = {
        -- bg_color = '#2d3441',
        bg_color = '#2e3440',
        fg_color = '#ffffff',
      },
      inactive_tab = {
        bg_color = '#111222',
        fg_color = '#ffffff',
      },
    },
  },
  window_frame = {
    font = wezterm.font({ family = "Roboto" }),
    font_size = 12.0,
    active_titlebar_bg = "#111111",
    inactive_titlebar_bg = "#111111",
    inactive_tab_edge = "#575757",
  },
  -- window_frame = {
  --   font = wezterm.font({ family = "Roboto", weight = "Bold" }),
  --   font_size = 12.0,
  --   active_titlebar_bg = "#333333",
  --   inactive_titlebar_bg = "#333333",
  --   inactive_tab_edge = "#575757",
  -- },

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
    {key="[",mods="SUPER|SHIFT",action=wezterm.action{MoveTabRelative=-1}},
    {key="]",mods="SUPER|SHIFT",action=wezterm.action{MoveTabRelative=1}},
    {key="u",mods="CTRL|SHIFT",action=wezterm.action{ScrollByPage=-1}},
    {key="d",mods="CTRL|SHIFT",action=wezterm.action{ScrollByPage=1}},
  },

  -- emacs keybindings
  -- https://github.com/wez/wezterm/discussions/808
  -- keys = {
  --   {key=" ",mods="CTRL|ALT",action=wezterm.action{SendString="\x1b[====\x20"}},
  --   {key="g",mods="CTRL|ALT",action=wezterm.action{SendString="\x1b[====\x67"}},
  --   {key=" ",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x20"}},
  --   {key=" ",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x20"}},
  --   {key="0",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x30"}},
  --   {key="1",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x31"}},
  --   {key="2",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x32"}},
  --   {key="3",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x33"}},
  --   {key="4",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x34"}},
  --   {key="5",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x35"}},
  --   {key="6",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x36"}},
  --   {key="7",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x37"}},
  --   {key="8",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x38"}},
  --   {key="9",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x39"}},
  --   {key="a",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x61"}},
  --   {key="b",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x62"}},
  --   {key="c",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x63"}},
  --   {key="d",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x64"}},
  --   {key="e",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x65"}},
  --   {key="f",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x66"}},
  --   {key="g",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x67"}},
  --   {key="h",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x68"}},
  --   {key="i",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x69"}},
  --   {key="j",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x6a"}},
  --   {key="k",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x6b"}},
  --   {key="l",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x6c"}},
  --   {key="m",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x6d"}},
  --   {key="n",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x6e"}},
  --   {key="o",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x6f"}},
  --   {key="p",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x70"}},
  --   {key="q",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x71"}},
  --   {key="r",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x72"}},
  --   {key="s",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x73"}},
  --   {key="t",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x74"}},
  --   {key="u",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x75"}},
  --   {key="v",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x76"}},
  --   {key="w",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x77"}},
  --   {key="x",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x78"}},
  --   {key="y",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x79"}},
  --   {key="z",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x7a"}},
  --   {key="`",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x60"}},
  --   {key="-",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x2d"}},
  --   {key="=",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x3d"}},
  --   {key="[",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x5b"}},
  --   {key="]",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x5d"}},
  --   {key="\\",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x5c"}},
  --   {key=";",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x3b"}},
  --   {key="'",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x27"}},
  --   {key=",",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x2c"}},
  --   {key=".",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x2e"}},
  --   {key="/",mods="SUPER",action=wezterm.action{SendString="\x18\x40\x68\x2f"}},
  --   {key="~",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x7e"}},
  --   {key="_",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x5f"}},
  --   {key="+",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x2b"}},
  --   {key="{",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x7b"}},
  --   {key="}",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x7d"}},
  --   {key="|",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x7c"}},
  --   {key=":",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x3a"}},
  --   {key="\"",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x22"}},
  --   {key="<",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x3c"}},
  --   {key=">",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x3e"}},
  --   {key="?",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x3f"}},
  --   {key=")",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x29"}},
  --   {key="!",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x21"}},
  --   {key="@",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x40"}},
  --   {key="#",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x23"}},
  --   {key="$",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x24"}},
  --   {key="%",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x25"}},
  --   {key="^",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x5e"}},
  --   {key="&",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x26"}},
  --   {key="*",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x2a"}},
  --   {key="(",mods="SUPER",action=wezterm.action{SendString="\x1b[======\x28"}},
  --   {key=" ",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x20"}},
  --   {key="A",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x41"}},
  --   {key="B",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x42"}},
  --   {key="C",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x43"}},
  --   {key="D",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x44"}},
  --   {key="E",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x45"}},
  --   {key="F",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x46"}},
  --   {key="G",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x47"}},
  --   {key="H",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x48"}},
  --   {key="I",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x49"}},
  --   {key="J",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x4a"}},
  --   {key="K",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x4b"}},
  --   {key="L",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x4c"}},
  --   {key="M",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x4d"}},
  --   {key="N",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x4e"}},
  --   {key="O",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x4f"}},
  --   {key="P",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x50"}},
  --   {key="Q",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x51"}},
  --   {key="R",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x52"}},
  --   {key="S",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x53"}},
  --   {key="T",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x54"}},
  --   {key="U",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x55"}},
  --   {key="V",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x56"}},
  --   {key="W",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x57"}},
  --   {key="X",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x58"}},
  --   {key="Y",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x59"}},
  --   {key="Z",mods="SUPER|SHIFT",action=wezterm.action{SendString="\x1b[======\x5a"}},
  --   {key="1",mods="CTRL",action=wezterm.action{SendString="\x18\x40\x63\x31"}},
  --   {key="2",mods="CTRL",action=wezterm.action{SendString="\x18\x40\x63\x32"}},
  --   {key="3",mods="CTRL",action=wezterm.action{SendString="\x18\x40\x63\x33"}},
  --   {key="4",mods="CTRL",action=wezterm.action{SendString="\x18\x40\x63\x34"}},
  --   {key="5",mods="CTRL",action=wezterm.action{SendString="\x18\x40\x63\x35"}},
  --   {key="6",mods="CTRL",action=wezterm.action{SendString="\x18\x40\x63\x36"}},
  --   {key="7",mods="CTRL",action=wezterm.action{SendString="\x18\x40\x63\x37"}},
  --   {key="8",mods="CTRL",action=wezterm.action{SendString="\x18\x40\x63\x38"}},
  --   {key="9",mods="CTRL",action=wezterm.action{SendString="\x18\x40\x63\x39"}},
  --   {key="A",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x41"}},
  --   {key="B",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x42"}},
  --   {key="C",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x43"}},
  --   {key="D",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x44"}},
  --   {key="E",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x45"}},
  --   {key="F",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x46"}},
  --   {key="G",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x47"}},
  --   {key="H",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x48"}},
  --   {key="I",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x49"}},
  --   {key="J",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x4a"}},
  --   {key="K",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x4b"}},
  --   {key="L",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x4c"}},
  --   {key="M",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x4d"}},
  --   {key="N",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x4e"}},
  --   {key="O",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x4f"}},
  --   {key="P",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x50"}},
  --   {key="Q",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x51"}},
  --   {key="R",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x52"}},
  --   {key="S",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x53"}},
  --   {key="T",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x54"}},
  --   {key="U",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x55"}},
  --   {key="V",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x56"}},
  --   {key="W",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x57"}},
  --   {key="X",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x58"}},
  --   {key="Y",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x59"}},
  --   {key="Z",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x5a"}},
  --   {key=" ",mods="CTRL|SHIFT",action=wezterm.action{SendString="\x1b[=\x20"}},
  --   {key="A",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x41"}},
  --   {key="B",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x42"}},
  --   {key="C",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x43"}},
  --   {key="D",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x44"}},
  --   {key="E",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x45"}},
  --   {key="F",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x46"}},
  --   {key="G",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x47"}},
  --   {key="H",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x48"}},
  --   {key="I",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x49"}},
  --   {key="J",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x4a"}},
  --   {key="K",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x4b"}},
  --   {key="L",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x4c"}},
  --   {key="M",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x4d"}},
  --   {key="N",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x4e"}},
  --   {key="O",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x4f"}},
  --   {key="P",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x50"}},
  --   {key="Q",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x51"}},
  --   {key="R",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x52"}},
  --   {key="S",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x53"}},
  --   {key="T",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x54"}},
  --   {key="U",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x55"}},
  --   {key="V",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x56"}},
  --   {key="W",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x57"}},
  --   {key="X",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x58"}},
  --   {key="Y",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x59"}},
  --   {key="Z",mods="CTRL|SHIFT|ALT",action=wezterm.action{SendString="\x1b[==\x5a"}},
  --   {key=" ",mods="SHIFT",action=wezterm.action{SendString="\x18\x40\x53\x20"}},
  --   {key="\\",mods="CTRL|ALT",action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
  --   {key="h",mods="CTRL|ALT|SUPER",action=wezterm.action{ActivatePaneDirection="Left"}},
  --   {key="j",mods="CTRL|ALT|SUPER",action=wezterm.action{ActivatePaneDirection="Down"}},
  --   {key="k",mods="CTRL|ALT|SUPER",action=wezterm.action{ActivatePaneDirection="Up"}},
  --   {key="l",mods="CTRL|ALT|SUPER",action=wezterm.action{ActivatePaneDirection="Right"}},
  -- }
}
