-- inspired by: https://github.com/mthnglac/dotfiles/tree/2495d1ef3f47f023a35f9c2f3102dfde437951c1

-- compatibility bridges
require('utils')

-- vim-plug settings & plugins
require('install-plugins')

-- functions
require('functions')

-- commands
require('commands')

-- keybindings
require('keybindings')

-- vim plugins settings
-- require('plugins')

-- plugin configs
require('evilline')
require('tree')

Command.cmd({
  'colorscheme OceanicNext',
  'syntax enable',
  -- 'colorscheme xcodelighthc',
  'syntax on',
  'filetype plugin on',
  'filetype plugin indent on',
})

Option.g({
  encoding = "UTF-8",

  background = "dark",
  -- background = "light",
  termguicolors = true,

  paste = false,
  pastetoggle="<F2>",

  tabstop = 2,
  softtabstop = 2,
  shiftwidth = 2,
  scrolloff = 2,
  expandtab = true,
  wrap = false,
  linebreak = true,
  list = true,
  listchars = "tab:▷\\ ,trail:◻,nbsp:𐩒",

  mouse = "n",
  showmode = false,

  number = true,
  relativenumber = true,

  ruler = true,
  showmatch = true,
  cursorline = true,

  ignorecase = true,
  smartcase = true,
  incsearch = true,
  hlsearch = true,

  inccommand = "split",
})

-- PLUGINS

  Variable.g({
    -- |nvim-tree|
    nvim_tree_side = 'right',
    nvim_tree_width = 40,

    -- |git-messenger|
    git_messenger_git_command = 'hub',
    git_messenger_no_default_mappings = true,
  })

  -- *vim-sneak*
  vim.g["sneak#absolute_dir"] = 1

  Variable.g({
    -- |dart-vim-plugin|
    dart_format_on_save = 1,

    -- |vim-javascript|
    javascript_plugin_jsdoc = 1,

    -- |vim-jsx-pretty|
    vim_jsx_pretty_colorful_config = 1,
    jsx_ext_required = 0,
  })
