-- this file can be loaded by calling `lua require('plugins')` from your init.vim

-- only required if you have packer configured as `opt`
-- vim.cmd [[packadd packer.nvim]]
-- only if your version of Neovim doesn't have https://github.com/neovim/neovim/pull/12632 merged
-- vim._update_package_paths()

vim.cmd([[autocmd! BufWritePost plugins.lua source <afile> | PackerCompile profile=true]])

return require('packer').startup(function(use)

  -- packer can manage itself
  use 'wbthomason/packer.nvim'

  -- sublime dark theme with oceanic-next
  use 'mhartington/oceanic-next'

  use {
    'EdenEast/nightfox.nvim',
    config = function() require'configs/nightfox-nvim' end,
  }
  -- try lush local theme
  -- use '~/pbond/mariana'

  -- using built-in lsp with nvim-lspconfig
  use {
    'neovim/nvim-lspconfig',
    requires = 'nvim-lua/lsp-status.nvim',
    config = function() require'configs/nvim-lspconfig' end,
  }

  -- file manager using nvim-tree.lua
  use {
    'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require'configs/nvim-tree' end
  }

  -- colorize colors with nvim-colorizer.lua
  use {
    'norcalli/nvim-colorizer.lua',
    config = function() require'colorizer'.setup() end
  }

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
    -- branch = '0.5-compat',
    run = ':TSUpdate',
    config = function() require'configs/nvim-treesitter' end
  }

  -- vim-commentary extension with treesitter nvim-ts-context-commentstring
  use {
    'JoosepAlviste/nvim-ts-context-commentstring',
    requires = {'nvim-treesitter/nvim-treesitter', 'tpope/vim-commentary'},
  }

  use {
    'folke/trouble.nvim',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require'configs/trouble-nvim' end
  }

  -- lsp signature with lsp_signature.nvim
  -- use {
  --   'ray-x/lsp_signature.nvim',
  -- }

  -- use {
  --   'hrsh7th/nvim-compe',
  --   config = function() require'configs/nvim-compe' end,
  -- }

  use { 'L3MON4D3/LuaSnip' }
  use {
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lua',
      'hrsh7th/cmp-calc',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-buffer',
      'saadparwaiz1/cmp_luasnip',
      -- {'andersevenrud/compe-tmux', branch = 'cmp'},
    },
    config = function() require'configs/nvim-cmp' end,
  }

  -- gitsigns.nvim on gutter
  use {
    'lewis6991/gitsigns.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function() require'configs/gitsigns-nvim' end
  }

  -- lua alternative for fugitive, rhubarb and fugitive-gitlab
  use {
    'dinhhuy258/git.nvim',
    config = function() require('git').setup({
      keymaps = {
        -- Open blame window
        blame = "<Leader>gb",
        -- Close blame window
        quit_blame = "q",
        -- Open blame commit
        blame_commit = "<CR>",
        -- Open file/folder in git repository
        browse = "<Leader>go",
        -- Open pull request of the current branch
        open_pull_request = "<Leader>gp",
        -- Create a pull request with the target branch is set in the `target_branch` option
        create_pull_request = "<Leader>gn",
      },
    }) end,
  }

  -- statusline improvements with galaxyline.nvim
  use {
    'glepnir/galaxyline.nvim',
    branch = 'main',
    config = function() require'configs/evilline' end,
    requires = 'kyazdani42/nvim-web-devicons'
  }
  -- use {
  --   'SmiteshP/nvim-gps',
  --   requires = 'nvim-treesitter/nvim-treesitter',
  --   config = function() require('nvim-gps').setup() end,
  -- }

  -- using packer.nvim
  use {
    'akinsho/bufferline.nvim',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require'configs/bufferline-nvim' end,
  }
  use 'famiu/bufdelete.nvim'

  use {
    "folke/which-key.nvim",
    config = function() require'configs/which-key' end
  }

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

  -- use {
  --   'ggandor/lightspeed.nvim',
  --   config = function() require'configs/lightspeed-nvim' end,
  -- }

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

  -- vim-commentary vim-surround and vim-repeat
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  use 'tpope/vim-repeat'

end)

