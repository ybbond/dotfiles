-- this file can be loaded by calling `lua require('plugins')` from your init.vim

-- only required if you have packer configured as `opt`
-- vim.cmd [[packadd packer.nvim]]
-- only if your version of Neovim doesn't have https://github.com/neovim/neovim/pull/12632 merged
-- vim._update_package_paths()

vim.cmd([[autocmd! BufWritePost plugins.lua source <afile> | PackerCompile profile=true]])

return require('packer').startup(function(use)

  ----------------- START OPTIMIZED FOR NEOVIM -----------------

  -- packer can manage itself
  use 'wbthomason/packer.nvim'

  -- sublime dark theme with oceanic-next
  use 'mhartington/oceanic-next'

  -- try lush local theme
  use '~/pbond/mariana'

  -- using built-in lsp with nvim-lspconfig
  use {
    'neovim/nvim-lspconfig',
    requires = 'nvim-lua/lsp-status.nvim',
    config = function() require'configs/nvim-lspconfig' end
  }

  -- file manager using nvim-tree.lua
  use {
    'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require'configs/nvim-tree' end
  }

  -- colorize colors with nvim-colorizer.lua
  -- use {
  --   'norcalli/nvim-colorizer.lua',
  --   config = function() require'colorizer'.setup() end
  -- }

  -- lush
  use 'rktjmp/lush.nvim'

  -- telescope.nvim for fuzzy findings and cool stuffs
  use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    config = function() require'configs/telescope' end
  }

  -- leveraging neovim 0.5.0 nvim-treesitter
  use {
    'nvim-treesitter/nvim-treesitter',
    branch = '0.5-compat',
    run = ':TSUpdate',
    config = function() require'configs/nvim-treesitter' end
  }

  -- vim-commentary extension with treesitter nvim-ts-context-commentstring
  use {
    'JoosepAlviste/nvim-ts-context-commentstring',
    requires = {'nvim-treesitter/nvim-treesitter', 'tpope/vim-commentary'},
  }

  -- qf_helper.nvim for QuickFix and LocList window
  use {
    'stevearc/qf_helper.nvim',
    config = function()
               require'qf_helper'.setup({
                 prefer_loclist = false
               })
             end
  }

  -- completion for neovim with nvim-compe
  use {
    'hrsh7th/nvim-compe',
    config = function() require'configs/nvim-compe' end
  }

  -- gitsigns.nvim on gutter
  use {
    'lewis6991/gitsigns.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function() require'configs/gitsigns-nvim' end
  }

  -- statusline improvements with galaxyline.nvim
  use {
    'glepnir/galaxyline.nvim',
      branch = 'main',
      config = function() require'configs/evilline' end,
      requires = 'kyazdani42/nvim-web-devicons'
  }

  use {
    'romgrk/barbar.nvim',
    config = function()
               vim.g.bufferline = {
                 exclude_name = {'__FLUTTER_DEV_LOG__'},
               }
             end,
    requires = {'kyazdani42/nvim-web-devicons'}
  }

  -- use {
  --   "folke/which-key.nvim",
  --   config = function() require'configs/which-key' end
  -- }

  use {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
               vim.cmd[[hi IndentBlanklineChar guifg=#39414C]]
               require("indent_blankline").setup {
                 char = "⎸",
                 buftype_exclude = {"terminal"}
               }
             end
  }

  ------------------ END OPTIMIZED FOR NEOVIM ------------------

  -- vim-sneak
  use {
    'justinmk/vim-sneak',
    config = function() vim.api.nvim_set_var('sneak#absolute_dir', 1) end
  }

  -- vim-commentary vim-surround and vim-repeat
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  use 'tpope/vim-repeat'

  -- vim-fugitive vim-rhubarb and fugitive-gitlab
  use 'tpope/vim-fugitive'
  use 'tpope/vim-rhubarb'
  use 'shumphrey/fugitive-gitlab.vim'

  -- lua development with lua-dev
  use {
    'folke/lua-dev.nvim',
    config = function() require'configs/lua-dev-nvim' end
  }

  -- Flutter development, with dart-vim-plugin and flutter-tools.nvim
  use 'dart-lang/dart-vim-plugin'
  use {
    'akinsho/flutter-tools.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function() require'configs/flutter-tools' end
  }

  -- TypeScript, JavaScript and ReactJS utilities with nvim-lsp-ts-utils
  use {
    'jose-elias-alvarez/nvim-lsp-ts-utils',
    requires = {
      'neovim/nvim-lspconfig',
      'nvim-lua/plenary.nvim',
      {'jose-elias-alvarez/null-ls.nvim', requires = 'nvim-lua/plenary.nvim'}
    },
    config = function() require'configs/lsp-ts-utils' end
  }

end)

