return {
  {
    'petertriho/nvim-scrollbar',
    config = function()
      require('scrollbar').setup()
      require('scrollbar.handlers.search').setup()
      require('scrollbar.handlers.gitsigns').setup()
    end,
  },

  {
    'kevinhwang91/nvim-hlslens',
    config = function()
      nnoremap 'n' [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]]
      nnoremap 'N' [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]]
      nnoremap '*' [[*<Cmd>lua require('hlslens').start()<CR>]]
      nnoremap '#' [[#<Cmd>lua require('hlslens').start()<CR>]]
      nnoremap 'g*' [[g*<Cmd>lua require('hlslens').start()<CR>]]
      nnoremap 'g#' [[g#<Cmd>lua require('hlslens').start()<CR>]]

      nnoremap '<Leader>l' ':noh<CR>'

      nnoremap '<LEADER>*' [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=1<CR><Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]]
      nnoremap '<LEADER>#' [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=0<CR><Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]]

      nnoremap '<LEADER><LEADER>*' [[:let @/=expand('<cword>')<CR>:let v:searchforward=1<CR><Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]]
      nnoremap '<LEADER><LEADER>#' [[:let @/=expand('<cword>')<CR>:let v:searchforward=0<CR><Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]]
    end,
  },

  {
    'folke/which-key.nvim',
    config = function() require'configs/folke-which-key' end
  },

  {
    'EdenEast/nightfox.nvim',
    config = function()
      require'configs/nightfox-nvim'

      vim.cmd[[colorscheme nordfox]]
    end,
  },

  {
    'kyazdani42/nvim-web-devicons',
    config = function() require'nvim-web-devicons'.setup({default = true}) end,
  },

  {
    'kevinhwang91/nvim-ufo',
    dependencies = 'kevinhwang91/promise-async',
    config = function() require 'configs/nvim-ufo' end,
  },

  {
    'luukvbaal/statuscol.nvim',
    config = function()
      require('statuscol').setup({
        setopt = true,
        reeval = true,
        foldfunc = 'builtin',
      })
    end,
  },

  {
    'neovim/nvim-lspconfig',
    config = function() require'configs/nvim-lspconfig' end,
  },

  {
    'andythigpen/nvim-coverage',
    dependencies = 'nvim-lua/plenary.nvim',
    config = function()
      require('coverage.config').setup({
        auto_reload = true,
      })
      require('coverage').setup()
    end,
  },

  {
    'j-hui/fidget.nvim',
    config = function() require'fidget'.setup() end
  },

  {
    'pianocomposer321/yabs.nvim',
    config = function() require'configs/yabs-nvim' end,
  },

  {
    'kyazdani42/nvim-tree.lua',
    dependencies = 'kyazdani42/nvim-web-devicons',
    config = function() require'configs/nvim-tree' end
  },

  {
    'norcalli/nvim-colorizer.lua',
    config = function() require'colorizer'.setup() end
  },

  {
    'nvim-telescope/telescope.nvim',
    dependencies = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    config = function() require'configs/nvim-telescope' end
  },

  {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = function()
      require'configs/nvim-treesitter'
      require'nvim-treesitter.configs'.setup {
        ensure_installed = {
          'http', 'json',  -- for rest.nvim
          'go',
          'lua',
          'dart',
        },
      }
    end
  },
  {
    'nvim-treesitter/playground',
    dependencies = 'nvim-treesitter/nvim-treesitter'
  },

  {
    'cshuaimin/ssr.nvim',
    name = 'ssr',
    config = function()
      require('ssr').setup {
        min_width = 50,
        min_height = 5,
        keymaps = {
          close = 'q',
          next_match = 'n',
          prev_match = 'N',
          replace_all = '<LEADER><CR>',
        },
      }
    end,
  },

  {
    'JoosepAlviste/nvim-ts-context-commentstring',
    dependencies = {'nvim-treesitter/nvim-treesitter', 'numToStr/Comment.nvim'},
  },

  'hrsh7th/vim-vsnip',
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lua',
      'hrsh7th/cmp-calc',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-vsnip',
      {
        'petertriho/cmp-git',
        dependencies = 'nvim-lua/plenary.nvim',
      },
    },
    config = function() require'configs/nvim-cmp' end,
  },

  {
    'lewis6991/gitsigns.nvim',
    dependencies = 'nvim-lua/plenary.nvim',
    config = function() require'configs/gitsigns-nvim' end,
  },

  {
    'famiu/feline.nvim',
    config = function() require'configs/feline' end,
  },

  {
     'NTBBloodbath/rest.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      require('rest-nvim').setup({
        result_split_horizontal = true,
        skip_ssl_verification = false,
        highlight = {
          enabled = true,
          timeout = 300,
        },
        custom_dynamic_variables = {
          ['$date'] = function()
            local os_date = os.date('%Y-%m-%d')
            return os_date
          end,
          ['$clock'] = function()
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
  },

  {
    'noib3/cokeline.nvim',
    dependencies = 'kyazdani42/nvim-web-devicons',
    config = function() require'configs/cokeline-nvim' end,
  },

  {
    'famiu/bufdelete.nvim',
    config = function ()
      nnoremap 'gx' '<CMD>Bdelete<CR>'
    end
  },

  {
    'lukas-reineke/indent-blankline.nvim',
    config = function()
      vim.cmd[[hi IndentBlanklineChar guifg=#3B434E]]
      require('indent_blankline').setup {
        -- char = '⎸',
        char = '▏',
        indent_blankline_char_blankline = '▏',
        -- char = '│',
        -- indent_blankline_char_blankline = '⎸',
        buftype_exclude = {'terminal'},
        show_current_context = true,
        show_current_context_start = false,
      }
    end
  },

  'guns/vim-sexp',
  'tpope/vim-dispatch',
  'clojure-vim/vim-jack-in',
  'radenling/vim-dispatch-neovim',
  'Olical/conjure',

  {
    dir = '~/poss/dart-vim-plugin',
  },
  {
    'akinsho/flutter-tools.nvim',
    dependencies = 'nvim-lua/plenary.nvim',
    config = function() require'configs/flutter-tools' end,
  },
  {
    'akinsho/dependency-assist.nvim',
    config = function ()
      require('dependency_assist').setup({})
    end
  },

  {
    'ray-x/go.nvim',
    dependencies = 'ray-x/guihua.lua',
    config = function () require'configs/go-nvim' end,
  },

  'terrastruct/d2-vim',

  'ellisonleao/glow.nvim',

  {
    'ggandor/lightspeed.nvim',
    config = function () require'configs/lightspeed' end
  },

  {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup({
        toggler = {
          line = 'gcc',
          block = 'gCc',
        },
        opleader = {
          line = 'gc',
          block = 'gC',
        },
        pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
      })
    end,
  },
  {
    'kylechui/nvim-surround',
    config = function()
      require('nvim-surround').setup({})
    end,
  },

  {
    'tpope/vim-fugitive',
    config = function ()
      nnoremap '<LEADER>gb' '<CMD>Git blame<CR>'
      nnoremap '<LEADER>go' '<CMD>GBrowse<CR>'
    end
  },
  'tpope/vim-rhubarb',
  'shumphrey/fugitive-gitlab.vim',

  'tpope/vim-repeat',
}
