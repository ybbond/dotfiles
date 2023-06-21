return {
  {
    'EdenEast/nightfox.nvim',
    config = function()
      require'configs/nightfox-nvim'

      vim.cmd[[colorscheme nordfox]]
    end,
  },

  {
    'folke/which-key.nvim',
    config = function() require'configs/folke-which-key' end
  },

  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build',
  },
  {
    'nvim-telescope/telescope.nvim',
    dependencies = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
    config = function() require'configs/nvim-telescope' end
  },

  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    config = function() require'configs/nvim-treesitter' end
  },
  {
    'nvim-treesitter/playground',
    dependencies = 'nvim-treesitter/nvim-treesitter'
  },

  {
    'nvim-tree/nvim-tree.lua',
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = function() require'configs/nvim-tree' end
  },

  {
    'neovim/nvim-lspconfig',
    config = function() require'configs/nvim-lspconfig' end,
  },
  {
    'seblj/nvim-lsp-extras',
    dependencies = 'neovim/nvim-lspconfig',
    config = function()
      require'nvim-lsp-extras'.setup({
        lightbulb = false,
      })
    end
  },
  {
    'VidocqH/lsp-lens.nvim',
    config = function()
      require'lsp-lens'.setup({
        enable = false,
        include_declaration = false   -- Reference include declaration
      })
    end
  },

  {
    "utilyre/barbecue.nvim",
    name = "barbecue",
    version = "*",
    dependencies = {
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons",
    },
    opts = {
      -- configurations go here
    },
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
    tag = 'legacy',
    config = function() require'fidget'.setup() end
  },

  {
    'pianocomposer321/yabs.nvim',
    config = function() require'configs/yabs-nvim' end,
  },

  {
    'norcalli/nvim-colorizer.lua',
    config = function() require'colorizer'.setup() end
  },

  {
    'cshuaimin/ssr.nvim',
    name = 'ssr',
    config = function()
      nnoremap '<C-S><C-R>' [[<CMD>lua require("ssr").open()<CR>]]
      xnoremap '<C-S><C-R>' [[<CMD>lua require("ssr").open()<CR>]]
      nnoremap '<C-S>R' [[<CMD>lua require("ssr").open()<CR>]]
      xnoremap '<C-S>R' [[<CMD>lua require("ssr").open()<CR>]]

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
    'petertriho/nvim-scrollbar',
    config = function()
      require('scrollbar').setup({
        hide_if_all_visible = true,
        handlers = {
          gitsigns = true,
          search = true,
        },
      })
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
    'kevinhwang91/nvim-ufo',
    dependencies = 'kevinhwang91/promise-async',
    config = function() require 'configs/nvim-ufo' end,
  },

  {
    'luukvbaal/statuscol.nvim',
    commit = '49a3bdab3e9cf23982724c1e888a6296fca4c8b9',
    config = function()
      require('statuscol').setup({
        setopt = true,
        reeval = true,
        foldfunc = 'builtin',
        relculright = true,
      })
    end,
  },

  {
    'freddiehaddad/feline.nvim',
    config = function() require'configs/feline' end,
  },

  {
    'NTBBloodbath/rest.nvim',
    ft = 'http',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      nmap '<C-F><C-F>' '<Plug>RestNvim<CR>' ({})
      nmap '<C-F><C-P>' '<Plug>RestNvimPreview<CR>' ({})
      nmap '<C-F><C-L>' '<Plug>RestNvimLast<CR>' ({})

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
    end
  },

  {
    'romgrk/barbar.nvim',
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = function()
      nmap 'gb' '<Cmd>BufferNext<CR>' ({})
      nmap 'gB' '<Cmd>BufferPrevious<CR>' ({})
      nmap 'g>' '<Cmd>BufferMoveNext<CR>' ({})
      nmap 'g<' '<Cmd>BufferMovePrevious<CR>' ({})
      nmap 'gx' '<Cmd>BufferClose<CR>' ({})
      nmap 'gp' '<Cmd>BufferPin<CR>' ({})
      require('bufferline').setup({
        exclude_ft = {
          '',
        },
        icons = {
          pinned = {
            button = '車',
          },
          diagnostics = {
            [vim.diagnostic.severity.ERROR] = {enabled = true, icon = ''},
            [vim.diagnostic.severity.WARN] = {enabled = true, icon = ''},
            [vim.diagnostic.severity.INFO] = {enabled = true, icon = ''},
            [vim.diagnostic.severity.HINT] = {enabled = true, icon = ''},
          },
        },
        maximum_length = 80,
      })
    end,
  },
  -- {
  --   'noib3/cokeline.nvim',
  --   dependencies = 'nvim-tree/nvim-web-devicons',
  --   config = function() require'configs/cokeline-nvim' end,
  -- },

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

  {
    'guns/vim-sexp',
    ft = {'clojure', 'lisp', 'scheme', 'timl'},
  },
  {
    'clojure-vim/vim-jack-in',
    dependencies = {'tpope/vim-dispatch', 'radenling/vim-dispatch-neovim'},
    ft = 'clojure',
  },
  {
    'Olical/conjure',
    ft = {'clojure', 'fennel', 'janet', 'hy', 'julia', 'racket', 'scheme', 'lua', 'lisp', 'python', 'rust'},
  },

  {
    dir = '~/poss/dart-vim-plugin',
    ft = 'dart',
  },
  {
    'akinsho/flutter-tools.nvim',
    ft = 'dart',
    dependencies = {
      'nvim-lua/plenary.nvim',
      -- 'stevearc/dressing.nvim',
    },
    config = function() require'configs/flutter-tools' end,
  },
  {
    'akinsho/pubspec-assist.nvim',
    dependencies = {
      'plenary.nvim',
    },
    config = function()
      require('pubspec-assist').setup()
    end,
  },

  {
    'ray-x/go.nvim',
    dependencies = {
      'ray-x/guihua.lua',
      'neovim/nvim-lspconfig',
      'nvim-treesitter/nvim-treesitter',
    },
    event = {'CmdlineEnter'},
    ft = {'go', 'gomod'},
    config = function () require'configs/go-nvim' end,
  },

  {
    'terrastruct/d2-vim',
    ft = 'd2',
    config = function ()
      vim.b.included_syntaxes = {}
    end,
  },

  {
    'ellisonleao/glow.nvim',
    config = true,
    cmd = "Glow",
  },

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
