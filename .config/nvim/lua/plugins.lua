-- this file can be loaded by calling `lua require('plugins')` from your init.vim

-- only required if you have packer configured as `opt`
-- vim.cmd [[packadd packer.nvim]]
-- only if your version of Neovim doesn't have https://github.com/neovim/neovim/pull/12632 merged
-- vim._update_package_paths()

vim.cmd([[autocmd! BufWritePost plugins.lua source <afile> | PackerCompile profile=true]])

return require('packer').startup(function(use)

  use 'wbthomason/packer.nvim'

  use {
    'petertriho/nvim-scrollbar',
    config = function() require("scrollbar").setup() end,
  }

  use {
    "folke/which-key.nvim",
    config = function() require'configs/which-key' end
  }

  use {
    'EdenEast/nightfox.nvim',
    config = function() require'configs/nightfox-nvim' end,
  }

  use {
    'kyazdani42/nvim-web-devicons',
    config = function() require'nvim-web-devicons'.setup({default = true}) end,
  }

  use {
    'neovim/nvim-lspconfig',
    config = function() require'configs/nvim-lspconfig' end,
  }

  use {
    'andythigpen/nvim-coverage',
    config = function() require('coverage').setup({
      lang = {
        dart = {
          coverage_command = "fvm flutter test --coverage --no-sound-null-safety",
        },
      },
    }) end,
  }

  use {
    'j-hui/fidget.nvim',
    config = function() require'fidget'.setup() end
  }

  use {
    'pianocomposer321/yabs.nvim',
    config = function() require'configs/yabs-nvim' end,
  }

  use {
    'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require'configs/nvim-tree' end
  }

  use {
    'norcalli/nvim-colorizer.lua',
    config = function() require'colorizer'.setup() end
  }

  use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    config = function() require'configs/telescope' end
  }

  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = function()
      require'configs/nvim-treesitter'
      require'nvim-treesitter.configs'.setup {
        ensure_installed = {"http", "json"}, -- for rest.nvim
      }
    end
  }
  use {
    'nvim-treesitter/playground',
    requires = 'nvim-treesitter/nvim-treesitter'
  }

  use {
    'JoosepAlviste/nvim-ts-context-commentstring',
    requires = {'nvim-treesitter/nvim-treesitter', 'tpope/vim-commentary'},
  }

  use { 'hrsh7th/vim-vsnip' }
  use {
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lua',
      'hrsh7th/cmp-calc',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-vsnip',
      {
        'petertriho/cmp-git',
        requires = 'nvim-lua/plenary.nvim',
      },
    },
    config = function() require'configs/nvim-cmp' end,
  }

  -- gitsigns.nvim on gutter
  use {
    'lewis6991/gitsigns.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function() require'configs/gitsigns-nvim' end
  }

  use {
    'famiu/feline.nvim',
    config = function() require'configs/feline' end,
  }

  use {
    "~/poss/rest.nvim",
     -- "NTBBloodbath/rest.nvim",
    requires = { "nvim-lua/plenary.nvim" },
    branch = 'fix/split-config',
    config = function()
      require("rest-nvim").setup({
        result_split_horizontal = true,
        skip_ssl_verification = false,
        highlight = {
          enabled = true,
          timeout = 300,
        },
        -- jump_to_request = true,
        -- result = {
        --   show_url = true,
        --   show_http_info = true,
        --   show_headers = true,
        -- },
        custom_dynamic_variables = {
          ["$date"] = function()
            local os_date = os.date('%Y-%m-%d')
            return os_date
          end,
          ["$clock"] = function()
            local os_date = os.date('%H:%M:%S')
            return os_date
          end
        },
      })

      vim.api.nvim_create_autocmd(
        'FileType',
        {
          pattern = 'http',
          callback = function()
            nmap '<C-F><C-F>' '<Plug>RestNvim<CR>' ({})
            nmap '<C-F><C-P>' '<Plug>RestNvimPreview<CR>' ({})
            nmap '<C-F><C-L>' '<Plug>RestNvimLast<CR>' ({})
          end,
        }
      )
    end
  }

  use {
    'noib3/cokeline.nvim',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require'configs/cokeline-nvim' end,
  }

  use {
    'famiu/bufdelete.nvim',
    config = function ()
      nnoremap 'gx' '<CMD>Bdelete<CR>'
    end
  }

  use {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
               vim.cmd[[hi IndentBlanklineChar guifg=#3B434E]]
               require("indent_blankline").setup {
                 char = "⎸",
                 -- char = "│",
                 -- indent_blankline_char_blankline = "⎸",
                 buftype_exclude = {"terminal"}
               }
             end
  }

  use {
    'guns/vim-sexp',
  }
  use 'tpope/vim-dispatch'
  use 'clojure-vim/vim-jack-in'
  use 'radenling/vim-dispatch-neovim'
  use 'Olical/conjure'

  use '~/poss/dart-vim-plugin'
  use {
    'akinsho/flutter-tools.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function() require'configs/flutter-tools' end,
  }
  use {
    'akinsho/dependency-assist.nvim',
    config = function ()
      require('dependency_assist').setup()
    end
  }

  use {
    'ray-x/go.nvim',
    config = function () require'configs/go-nvim' end,
  }

  use {
    'ggandor/lightspeed.nvim',
    config = function () require'configs/lightspeed' end
  }

  use {
    'tpope/vim-fugitive',
    config = function ()
      nnoremap '<LEADER>gb' '<CMD>Git blame<CR>'
      nnoremap '<LEADER>go' '<CMD>GBrowse<CR>'
    end
  }
  use 'tpope/vim-rhubarb'
  use 'shumphrey/fugitive-gitlab.vim'

  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  use 'tpope/vim-repeat'

end)
