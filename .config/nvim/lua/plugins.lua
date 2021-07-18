-- this file can be loaded by calling `lua require('plugins')` from your init.vim

-- only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]
-- only if your version of Neovim doesn't have https://github.com/neovim/neovim/pull/12632 merged
-- vim._update_package_paths()

return require('packer').startup(function()

  ----------------- START OPTIMIZED FOR NEOVIM -----------------

  -- packer can manage itself
  use 'wbthomason/packer.nvim'

  -- sublime dark theme
  use 'mhartington/oceanic-next'

  -- tabline improvements with barbar
  use {
    'romgrk/barbar.nvim',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require'configs/barbar' end
  }

  -- statusline improvements with galaxyline
  use {
    'glepnir/galaxyline.nvim',
    branch = 'main',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require'configs/galaxyline-theme-evilline' end
  }

  -- file manager using nvim-tree
  use {
    'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require'configs/nvim-tree' end
  }

  -- telescope for fuzzy findings and cool stuffs
  use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    config = function() require'configs/telescope' end
  }

  -- leveraging neovim 0.5.0 treesitter
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  -- trying to use neogit
  -- use {
  --   'TimUntersberger/neogit',
  --   requires = {
  --     'nvim-lua/plenary.nvim',
  --     {
  --       'sindrets/diffview.nvim',
  --       requires = {'kyazdani42/nvim-web-devicons', opt = true},
  --       config = function() require'configs/diffview' end
  --     }
  --   },
  --   config = function() require'configs/neogit' end
  -- }

  use {
    'neovim/nvim-lspconfig',
    config = function() require'configs/nvim-lspconfig' end
  }

  ------------------ END OPTIMIZED FOR NEOVIM ------------------

  use {
    'justinmk/vim-sneak',
    config = function() vim.api.nvim_set_var('sneak#absolute_dir', 1) end
  }

  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  use 'tpope/vim-repeat'

  use 'dart-lang/dart-vim-plugin'
  use {
    'akinsho/flutter-tools.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function() require'configs/flutter-tools' end
  }

  -- use {
  --   'pangloss/vim-javascript',
  --   config = function() vim.api.nvim_set_var('javascript_plugin_jsdoc', 1) end
  -- }
  -- use 'othree/yajs.vim'
  -- use {
  --   'maxmellon/vim-jsx-pretty',
  --   config = function()
  --              vim.api.nvim_set_var('vim_jsx_pretty_colorful_config', 1)
  --              vim.api.nvim_set_var('jsx_ext_required', 0)
  --            end
  -- }
  -- use {
  --   'styled-components/vim-styled-components',
  --   branch = 'main',
  -- }
  -- use 'leafgarland/typescript-vim'
  -- use 'HerringtonDarkholme/yats.vim'

end)

