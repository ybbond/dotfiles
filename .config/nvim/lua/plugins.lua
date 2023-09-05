return {
  {
    'Mofiqul/vscode.nvim',
    config = function()
      -- local c = require('vscode.colors').get_colors()
      require'vscode'.setup({
        -- group_overrides = {
        --   CursorLine = { bg=c.vscGray },
        -- },
      })
      require'vscode'.load()
    end,
  },

  {
    'folke/which-key.nvim',
    event = 'VeryLazy',
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    opts = {}
  },

  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build',
  },
  {
    -- 'nvim-telescope/telescope.nvim',
    dir = '~/poss/telescope.nvim',
    dependencies = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
    config = function() require'configs/telescope-nvim' end
  },

  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    config = function()
      require('nvim-treesitter.configs').setup({
        highlight = {enable = true},
        ensure_installed = { 'http', 'json', 'go', 'lua', 'dart', 'zig' },
        context_commentstring = {
          enable = true,
          enable_autocmd = false,
        },
        indent = { enable = false},
        endwise = {
          enable = true,
        },
      })
    end,
  },

  {
    'neovim/nvim-lspconfig',
    config = function() require'configs/nvim-lspconfig' end,
  },

  'hrsh7th/vim-vsnip',
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lua',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-vsnip',
    },
    config = function() require'configs/nvim-cmp' end,
  },

  {
    'mhartington/formatter.nvim',
    config = function()
      require('formatter').setup {
        filetype = {
          dart = {
            require('formatter.filetypes.dart').dartformat,
          },
          ocaml = {
            -- require('formatter.filetypes.ocaml').ocamlformat,
            function()
              -- local util = require('formatter.util')
              local path = string.gsub(vim.api.nvim_buf_get_name(0), vim.loop.cwd()..'/', '')
              return {
                exe = "ocamlformat",
                args = {
                  "--enable-outside-detected-project",
                  -- util.escape_path(util.get_current_buffer_file_path()),
                  path,
                },
                stdin = true,
              }
            end
          },
          go = {
            require('formatter.filetypes.go').gofmt,
          },
          zig = {
            require('formatter.filetypes.zig').zigfmt,
          },
        },
      }
    end,
  },

  {
    'Bekaboo/dropbar.nvim',
    commit = '2bc8fd4bb3a80c08e9647328f1f954d71e1dd431',
  },

  {
    'akinsho/flutter-tools.nvim',
    ft = 'dart',
    dependencies = 'nvim-lua/plenary.nvim',
    config = function()
      require('flutter-tools').setup {
        lsp = {
          color = {
            enabled = true,
            background = true,
            virtual_text = false,
          },
          on_attach = function(_, bufnr)
            require'configs/nvim-lspconfig'.ybbond_lsp_on_attach(_, bufnr)
            vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gA', '<CMD>lua require("telescope").extensions.flutter.commands()<CR>', {noremap=true, silent=true})
          end,
          capabilities = require'configs/nvim-lspconfig'.ybbond_lsp_capabilities,
        },
        dev_log = { enabled = true, notify_errors = true },
        closing_tags = { prefix = ' → ' },
        fvm = true,
      }
      require('telescope').load_extension('flutter')
    end,
  },
  {
    'akinsho/pubspec-assist.nvim',
    dependencies = 'nvim-lua/plenary.nvim',
    config = function() require('pubspec-assist').setup() end,
  },

  {
    'ray-x/go.nvim',
    config = function() require'configs/go-nvim' end,
    ft = {'go', 'gomod'},
  },

  {
    'andythigpen/nvim-coverage',
    config = function()
      require('coverage').setup({
        auto_reload = true,
      })
    end
  },

  {
    'nvim-tree/nvim-tree.lua',
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = function() require'configs/nvim-tree' end,
  },

  -- {
  --   'altermo/ultimate-autopair.nvim',
  --   event = {
  --     'InsertEnter',
  --     'CmdlineEnter',
  --   },
  --   branch = 'v0.6',
  --   opts = {},
  -- },

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
    'petertriho/nvim-scrollbar',
    config = function()
      local c = require('vscode.colors').get_colors()
      require('scrollbar').setup({
        handle = {
          color = c.vscGray,
        },
        marks = {
          Search = { color = c.vscSearchCurrent },
          GitAdd = {
            text = '█',
            -- text = '▂',
          },
          GitChange = {
            text = '█',
            -- text = '▂',
          },
        --   Error = { color = c.vscRed },
        --   Warn = { color = c.vscYellow },
        --   Info = { color = c.vscGreen },
        --   Hint = { color = c.vscBlue },
        --   Misc = { color = c.vscDarkBlue },
        },
        hide_if_all_visible = true,
        handlers = {
          gitsigns = true,
          search = true,
        },
      })
      require('scrollbar.handlers.search').setup()
      require('scrollbar.handlers.gitsigns').setup()
    end,
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
        exclude_ft = { '' },
        icons = {
          pinned = { button = '' },
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

  {
    'nvim-lualine/lualine.nvim',
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = function()
      require('lualine').setup({
        options = {
          theme = 'vscode',
          component_separators = { left = '|', right = '|'},
          section_separators = { left = '', right = ''},
        },
        sections = {
          lualine_a = {'mode'},
          lualine_b = {},
          lualine_c = {
            'filetype',
            {'diagnostics', sources = {'nvim_diagnostic'}},
            {'diagnostics', sources = {'nvim_workspace_diagnostic'}},
          },
          lualine_x = {},
          lualine_y = {'location'},
          lualine_z = {'branch', 'diff'}
        },
      })
    end,
  },

  {
    'lewis6991/gitsigns.nvim',
    dependencies = 'nvim-lua/plenary.nvim',
    config = function() require'configs/gitsigns-nvim' end,
  },
  {
    'FabijanZulj/blame.nvim',
    config = function()
      nnoremap '<LEADER>gb' '<CMD>ToggleBlame window<CR>'
      nnoremap '<LEADER>gB' '<CMD>ToggleBlame virtual<CR>'
    end,
  },

  {
    'numToStr/Comment.nvim',
    dependencies = 'JoosepAlviste/nvim-ts-context-commentstring',
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
}
