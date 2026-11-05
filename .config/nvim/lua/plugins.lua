return {
  {
    'f-person/auto-dark-mode.nvim',
    opts = {
      set_dark_mode = function()
        vim.api.nvim_set_option_value('background', 'dark', {})

        vim.api.nvim_set_hl(0, 'YbbondCoverageCovered', { fg='NvimLightGreen', ctermfg=10 })
        vim.api.nvim_set_hl(0, 'YbbondCoverageUncovered', { fg='NvimLightRed', ctermfg=9 })

        vim.api.nvim_set_hl(0, 'GitSignsAddNr', { bg='NvimLightGreen', fg='NvimDarkGrey1', ctermbg=10, ctermfg=0 })
        vim.api.nvim_set_hl(0, 'GitSignsChangeNr', { bg='NvimLightYellow', fg='NvimDarkGrey1', ctermbg=11, ctermfg=0 })
        vim.api.nvim_set_hl(0, 'GitSignsChangedeleteNr', { bg='NvimLightYellow', fg='NvimDarkGrey1', ctermbg=11, ctermfg=0 })
        vim.api.nvim_set_hl(0, 'GitSignsDeleteNr', { bg='NvimLightRed', fg='NvimDarkGrey1', ctermbg=9, ctermfg=0 })
        vim.api.nvim_set_hl(0, 'GitSignsTopdeleteNr', { bg='NvimLightRed', fg='NvimDarkGrey1', ctermbg=9, ctermfg=0 })
      end,
      set_light_mode = function()
        vim.api.nvim_set_option_value("background", "light", {})

        vim.api.nvim_set_hl(0, 'YbbondCoverageCovered', { fg='NvimDarkGreen', ctermfg=2 })
        vim.api.nvim_set_hl(0, 'YbbondCoverageUncovered', { fg='NvimDarkRed', ctermfg=1 })

        vim.api.nvim_set_hl(0, 'GitSignsAddNr', { bg='NvimDarkGreen', fg='NvimLightGrey1', ctermbg=2, ctermfg=15 })
        vim.api.nvim_set_hl(0, 'GitSignsChangeNr', { bg='NvimDarkYellow', fg='NvimLightGrey1', ctermbg=3, ctermfg=15 })
        vim.api.nvim_set_hl(0, 'GitSignsChangedeleteNr', { bg='NvimDarkYellow', fg='NvimLightGrey1', ctermbg=3, ctermfg=15 })
        vim.api.nvim_set_hl(0, 'GitSignsDeleteNr', { bg='NvimDarkRed', fg='NvimLightGrey1', ctermbg=1, ctermfg=15 })
        vim.api.nvim_set_hl(0, 'GitSignsTopdeleteNr', { bg='NvimDarkRed', fg='NvimLightGrey1', ctermbg=1, ctermfg=15 })
      end,
      update_interval = 3000,
      fallback = 'dark'
    }
  },

  {
    'folke/which-key.nvim',
    dependencies = {{ 'echasnovski/mini.icons', version = false }},
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
    'nvim-telescope/telescope.nvim',
    dependencies = {'nvim-lua/plenary.nvim'},
    config = function() require'configs/telescope-nvim' end
  },

  {
    'nvim-treesitter/nvim-treesitter',
    lazy = false,
    build = ':TSUpdate',
    config = function()
      -- main branch dropped configs.setup, highlighting is Neovim's own; pcall skips filetypes without a parser
      vim.api.nvim_create_autocmd('FileType', {
        callback = function(args) pcall(vim.treesitter.start, args.buf) end,
      })
    end,
  },

  {
    'neovim/nvim-lspconfig',
    config = function() require'configs/nvim-lspconfig' end,
  },

  {
    'b0o/schemastore.nvim',
  },

  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lua',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-buffer',
      -- 'PaterJason/cmp-conjure',
    },
    config = function() require'configs/nvim-cmp' end,
  },

  {
    'carlos-algms/agentic.nvim',
    opts = {
      -- provider = 'gemini-acp'
      -- provider = 'cursor-acp',
      provider = 'claude-agent-acp',
      -- provider = 'codex-acp',
    },
    keys = {
      {
        '<A-i>i',
        function() require('agentic').toggle() end,
        mode = { 'n', 'v', 'i' },
        desc = 'Toggle Agentic Chat',
      },
      {
        '<A-i>u',
        function() require('agentic').add_selection_or_file_to_context() end,
        mode = { 'n', 'v' },
        desc = 'Add file or selection to Agentic to Context',
      },
      {
        '<A-i>e',
        function() require('agentic').add_current_line_diagnostics() end,
        mode = { 'n' },
        desc = 'Add current line diagnostic to Agentic',
      },
      {
        '<A-i>E',
        function() require('agentic').add_buffer_diagnostics() end,
        mode = { 'n' },
        desc = 'Add all buffer diagnostics to Agentic',
      },
      {
        '<A-i>r',
        function() require('agentic').restore_session() end,
        mode = { 'n', 'v', 'i' },
        silent = true,
        desc = 'Agentic restore session',
      },
    },
  },

  {
    'stevearc/conform.nvim',
    event = { 'BufWritePre' },
    cmd = { 'ConformInfo' },
    keys = {
      {
        '<leader>f',
        function()
          require('conform').format({ async = true })
        end,
        mode = '',
        desc = 'Format buffer',
      },
    },
    opts = {
      default_format_opts = { lsp_format = 'fallback' },
      formatters_by_ft = {
        astro = { 'biome', 'prettierd', 'prettier', stop_after_first = true },
        cpp = { 'astyle' },
        go = { 'gofmt' },
        java = { 'spotless_gradle' },
        javascript = { 'biome', 'prettierd', 'prettier', stop_after_first = true },
        javascriptreact = { 'biome', 'prettierd', 'prettier', stop_after_first = true },
        typescript = { 'biome', 'prettierd', 'prettier', stop_after_first = true },
        typescriptreact = { 'biome', 'prettierd', 'prettier', stop_after_first = true },
        zig = { 'zigfmt' },
      },
      formatters = {
        -- only in projects with a biome.json, so prettier projects skip it
        biome = { require_cwd = true },
        astyle = {
          append_args = { '--indent=spaces=2', '--indent-switches' },
        },
      },
    },
  },

  {
    'Bekaboo/dropbar.nvim',
    config = function()
      local dropbar_api = require('dropbar.api')
      vim.keymap.set('n', '<C-t><C-o>', dropbar_api.pick, { desc = 'Pick symbol' })

      local default_enable = require('dropbar.configs').opts.bar.enable
      require('dropbar').setup({
        bar = {
          -- default only attaches with LSP/treesitter; also attach to plain files so the path still shows
          enable = function(buf, win, info)
            return default_enable(buf, win, info)
              or (vim.bo[buf].buftype == ''
                and vim.api.nvim_buf_get_name(buf) ~= ''
                and vim.fn.win_gettype(win) == ''
                and vim.wo[win].winbar == '')
          end,
        },
      })
    end,
  },

  {
    'dmmulroy/tsc.nvim',
    config = function() require('tsc').setup() end
  },

  {
    'ray-x/go.nvim',
    config = function() require'configs/go-nvim' end,
    ft = {'go', 'gomod'},
  },

  {
    'lervag/vimtex',
    lazy = false,
    init = function()
      vim.g.vimtex_view_method = 'skim'
      vim.g.vimtex_view_general_viewer = 'skim'
      vim.g.vimtex_view_skim_sync = 1
    end
  },

  {
    'andythigpen/nvim-coverage',
    config = function()
      require('coverage').setup({
        auto_reload = true,
        signs = {
          -- possible coverage texts:
          -- '▌'
          -- '░'
          -- '▒'
          -- '▓'
          covered = {
            hl = 'YbbondCoverageCovered',
            text = '▒',
          },
          uncovered = {
            hl = 'YbbondCoverageUncovered',
            text = '▒',
          },
        },
      })
    end
  },

  {
    'nvim-tree/nvim-tree.lua',
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = function() require'configs/nvim-tree' end,
  },

  {
    'backdround/improved-ft.nvim',
    config = function()
      require('improved-ft').setup({
        use_default_mappings = true,
        use_relative_repetition = true,
      })
    end
  },

  {
    'kevinhwang91/nvim-hlslens',
    config = function()
      require('hlslens').setup()

      nnoremap 'n' [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]]
      nnoremap 'N' [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]]
      nnoremap '*' [[*<Cmd>lua require('hlslens').start()<CR>]]
      nnoremap '#' [[#<Cmd>lua require('hlslens').start()<CR>]]
      nnoremap 'g*' [[g*<Cmd>lua require('hlslens').start()<CR>]]
      nnoremap 'g#' [[g#<Cmd>lua require('hlslens').start()<CR>]]

      nnoremap '<Leader>l' ':noh<CR>'

      -- keep asterisk and pound to be case sensitive
      nnoremap '<LEADER>*' [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=1<CR><Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]]
      nnoremap '<LEADER>#' [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=0<CR><Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]]

      nnoremap '<LEADER><LEADER>*' [[:let @/=expand('<cword>')<CR>:let v:searchforward=1<CR><Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]]
      nnoremap '<LEADER><LEADER>#' [[:let @/=expand('<cword>')<CR>:let v:searchforward=0<CR><Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]]
    end,
  },

  {
    'petertriho/nvim-scrollbar',
    branch = 'refactor/v2',
    opts = {
      scrollbar = {
        hide_if_all_visible = true,
      },
      providers = {
        cursor = false,
        gitsigns = true,
      },
    },
    -- config = function()
    --   require('scrollbar').setup({
    --     -- gui color is read from the highlight group; color_nr is the cterm color
    --     handle = { highlight = 'Visual', color_nr = 8 },
    --     marks = {
    --       Search = { highlight = 'WarningMsg', color_nr = 3 },
    --       GitAdd = {
    --         text = '█',
    --         -- text = '▂',
    --       },
    --       GitChange = {
    --         text = '█',
    --         -- text = '▂',
    --       },
    --     },
    --     hide_if_all_visible = true,
    --     handlers = {
    --       cursor = false,
    --       gitsigns = true,
    --       search = true,
    --     },
    --   })
    --   require('scrollbar.handlers.search').setup()
    --   require('scrollbar.handlers.gitsigns').setup()
    -- end,
  },

  -- {
  --   'romgrk/barbar.nvim',
  --   dependencies = 'nvim-tree/nvim-web-devicons',
  --   config = function()
  --     nmap 'gb' '<Cmd>BufferNext<CR>' ({})
  --     nmap 'gB' '<Cmd>BufferPrevious<CR>' ({})
  --     nmap 'g>' '<Cmd>BufferMoveNext<CR>' ({})
  --     nmap 'g<' '<Cmd>BufferMovePrevious<CR>' ({})
  --     nmap 'gx' '<Cmd>BufferClose<CR>' ({})
  --     nmap 'gp' '<Cmd>BufferPin<CR>' ({})
  --     require('barbar').setup({
  --       exclude_ft = { '' },
  --       icons = {
  --         pinned = { button = '' },
  --         diagnostics = {
  --           [vim.diagnostic.severity.ERROR] = {enabled = true, icon = ''},
  --           [vim.diagnostic.severity.WARN] = {enabled = true, icon = ''},
  --           [vim.diagnostic.severity.INFO] = {enabled = true, icon = ''},
  --           [vim.diagnostic.severity.HINT] = {enabled = true, icon = ''},
  --         },
  --       },
  --       maximum_length = 80,
  --     })
  --   end,
  -- },

  {
    'nvim-lualine/lualine.nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons',
    },
    config = function()
      require('lualine').setup({
        options = {
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
    'm4xshen/autoclose.nvim',
    config = function()
      require("autoclose").setup({
        options = {
          disable_command_mode = true,
          disabled_filetypes = {
            'TelescopePrompt'
          }
        }
      })
    end
  },

  {
    'kylechui/nvim-surround',
    config = function()
      require('nvim-surround').setup()
    end
  },
}
