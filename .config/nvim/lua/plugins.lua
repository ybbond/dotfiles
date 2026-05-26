return {
  {
    'Mofiqul/vscode.nvim',
    config = function()
      -- local c = require('vscode.colors').get_colors()
      -- require'vscode'.setup({
      --   -- color_overrides = {
      --   --   vscCursorDarkDark = c.vscLeftDark,
      --   -- },
      --   group_overrides = {
      --     CursorLine = { bg = c.vscLeftDark },
      --   },
      -- })
      require'vscode'.load()
    end,
  },
  {
    'cormacrelf/dark-notify',
    config = function()
      require('dark_notify').run({
        schemes = {
          dark = {
            background = 'dark',
          },
          light = {
            background = 'light',
          },
        },
        onchange = function(mode)
          local c = require('vscode.colors').get_colors()
          if mode == 'light' then
            vim.api.nvim_set_hl(0, 'YbbondCoverageCovered', { fg=c.vscBlueGreen })
            vim.api.nvim_set_hl(0, 'YbbondCoverageUncovered', { fg=c.vscRed })

            vim.api.nvim_set_hl(0, 'GitSignsAddNr', { bg=c.vscDiffGreenLight, fg=c.vscCursorLight })
            vim.api.nvim_set_hl(0, 'GitSignsChangeNr', { bg=c.vscDarkYellow, fg=c.vscCursorLight })
            vim.api.nvim_set_hl(0, 'GitSignsChangedeleteNr', { bg=c.vscDarkYellow, fg=c.vscCursorLight })
            vim.api.nvim_set_hl(0, 'GitSignsDeleteNr', { bg=c.vscDiffRedLight, fg=c.vscCursorLight })
            vim.api.nvim_set_hl(0, 'GitSignsTopdeleteNr', { bg=c.vscDiffRedLight, fg=c.vscCursorLight })
          elseif mode == 'dark' then
            vim.api.nvim_set_hl(0, 'YbbondCoverageCovered', { fg=c.vscLightGreen })
            vim.api.nvim_set_hl(0, 'YbbondCoverageUncovered', { fg=c.vscLightRed })

            vim.api.nvim_set_hl(0, 'GitSignsAddNr', { bg=c.vscDiffGreenDark, fg=c.vscCursorDarkDark })
            vim.api.nvim_set_hl(0, 'GitSignsChangeNr', { bg=c.vscYellow, fg=c.vscCursorDarkDark })
            vim.api.nvim_set_hl(0, 'GitSignsChangedeleteNr', { bg=c.vscYellow, fg=c.vscCursorDarkDark })
            vim.api.nvim_set_hl(0, 'GitSignsDeleteNr', { bg=c.vscDiffRedDark, fg=c.vscCursorDarkDark })
            vim.api.nvim_set_hl(0, 'GitSignsTopdeleteNr', { bg=c.vscDiffRedDark, fg=c.vscCursorDarkDark })
          end
        end
      })
    end
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
    dependencies = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
    config = function() require'configs/telescope-nvim' end
  },

  -- {
  --   'nvim-treesitter/nvim-treesitter',
  --   lazy = false,
  --   build = ':TSUpdate',
  --   branch = 'master',
  --   config = function()
  --     require('nvim-treesitter.configs').setup({
  --       highlight = {
  --         enable = true,
  --       },
  --     })
  --   end,
  -- },

  {
    'neovim/nvim-lspconfig',
    config = function() require'configs/nvim-lspconfig' end,
  },

  {
    'b0o/schemastore.nvim',
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
      'PaterJason/cmp-conjure',
    },
    config = function() require'configs/nvim-cmp' end,
  },

  -- {
  --   'mhartington/formatter.nvim',
  --   config = function()
  --     require('formatter').setup {
  --       filetype = {
  --         dart = {
  --           -- require('formatter.filetypes.dart').dartformat,
  --           function(t)
  --             t = t or {}
  --
  --             local args = { "--output show" }
  --             if t.line_length ~= nil then
  --               table.insert(args, "--line-length " .. t.line_length)
  --             end
  --
  --             return {
  --               exe = "fvm dart format",
  --               args = args,
  --               stdin = true,
  --             }
  --           end
  --         },
  --         go = {
  --           require('formatter.filetypes.go').gofmt,
  --         },
  --         astro = {
  --           require('formatter.filetypes.typescript').prettierd,
  --         },
  --         typescript = {
  --           require('formatter.filetypes.typescript').eslint_d,
  --         },
  --         typescriptreact = {
  --           require('formatter.filetypes.typescript').eslint_d,
  --         },
  --       },
  --     }
  --   end,
  -- },

  {
    'carlos-algms/agentic.nvim',
    opts = {
      provider = 'gemini-acp'
    },
    keys = {
      {
        'î',
        function() require('agentic').toggle() end,
        mode = { 'n', 'v', 'i' },
        desc = 'Toggle Agentic Chat',
      },
      {
        'û',
        function() require('agentic').add_selection_or_file_to_context() end,
        mode = { 'n', 'v' },
        desc = 'Add file or selection to Agentic to Context',
      },
      {
        'ê',
        function() require('agentic').add_current_line_diagnostics() end,
        mode = { 'n' },
        desc = 'Add current line diagnostic to Agentic',
      },
      {
        'Ê',
        function() require('agentic').add_buffer_diagnostics() end,
        mode = { 'n' },
        desc = 'Add all buffer diagnostics to Agentic',
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
      formatters_by_ft = {
        astro = { 'prettierd', 'prettier', 'eslint_d', 'eslint', stop_after_first = true },
        cpp = { 'astyle' },
        go = { 'gofmt' },
        java = { 'spotless_gradle' },
        typescript = { 'prettierd', 'prettier', 'eslint_d', 'eslint', stop_after_first = true },
        typescriptreact = { 'prettierd', 'prettier', stop_after_first = true },
        -- typescript = { 'eslint_d' },
        -- typescriptreact = { 'eslint_d' },
        zig = { 'zigfmt' },
      },
      formatters = {
        astyle = {
          append_args = { '--indent=spaces=2', '--indent-switches' },
        },
      },
    },
  },

  {
    'Bekaboo/dropbar.nvim',
  },

  {
    'dmmulroy/tsc.nvim',
    config = function() require('tsc').setup() end
  },

  -- {
  --   'gpanders/nvim-parinfer',
  --   ft = { 'lisp', 'commonlisp' },
  -- },
  -- {
  --   'Olical/conjure',
  --   ft = { 'lisp', 'commonlisp' },
  --   config = function()
  --     require("conjure.main").main()
  --     require("conjure.mapping")["on-filetype"]()
  --     vim.g['conjure#extract#tree_sitter#enabled'] = true
  --   end,
  --   init = function()
  --     -- Set configuration options here
  --     vim.g["conjure#debug"] = true
  --   end,
  -- },

  -- {
  --   'akinsho/flutter-tools.nvim',
  --   -- ft = 'dart',
  --   lazy = false,
  --   dependencies = 'nvim-lua/plenary.nvim',
  --   config = function()
  --     require('flutter-tools').setup {
  --       lsp = {
  --         color = {
  --           enabled = true,
  --           background = true,
  --           virtual_text = false,
  --         },
  --         on_attach = function(_, bufnr)
  --           require'configs/nvim-lspconfig'.ybbond_lsp_on_attach(_, bufnr)
  --           vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gA', '<CMD>lua require("telescope").extensions.flutter.commands()<CR>', {noremap=true, silent=true})
  --         end,
  --         capabilities = require'cmp_nvim_lsp'.default_capabilities()
  --       },
  --       dev_log = { enabled = true, notify_errors = true },
  --       closing_tags = { prefix = ' → ' },
  --       fvm = true,
  --     }
  --     require('telescope').load_extension('flutter')
  --   end,
  -- },
  -- {
  --   'akinsho/pubspec-assist.nvim',
  --   dependencies = 'nvim-lua/plenary.nvim',
  --   config = function() require('pubspec-assist').setup({}) end,
  -- },

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

  -- {
  --   'altermo/ultimate-autopair.nvim',
  --   event = {'InsertEnter', 'CmdlineEnter'},
  --   -- TODO(xxkkbb)
  --   -- event = { 'User EnableAutoPair' },
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
        },
        hide_if_all_visible = true,
        handlers = {
          cursor = false,
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
      require('barbar').setup({
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
    dependencies = {'nvim-tree/nvim-web-devicons', 'Mofiqul/vscode.nvim'},
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
    'dinhhuy258/git.nvim',
    config = function() require('git').setup({
      winbar = true,
    }) end
  },

  {
    'johmsalas/text-case.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim' },
    config = function()
      require('textcase').setup({})
      require('telescope').load_extension('textcase')
    end,
    keys = {
      "<leader>c", -- Default invocation prefix
      { "<leader>c.", "<cmd>TextCaseOpenTelescope<CR>", mode = { "n", "x" }, desc = "Telescope" },
    },
    cmd = {
      -- NOTE: The Subs command name can be customized via the option "substitude_command_name"
      'Subs',
      'TextCaseOpenTelescope',
      'TextCaseOpenTelescopeQuickChange',
      'TextCaseOpenTelescopeLSPChange',
      'TextCaseStartReplacingCommand',
    },
    -- If you want to use the interactive feature of the `Subs` command right away, text-case.nvim
    -- has to be loaded on startup. Otherwise, the interactive feature of the `Subs` will only be
    -- available after the first executing of it or after a keymap of text-case.nvim has been used.
    lazy = false,
  },

  {
    'm4xshen/autoclose.nvim',
    config = function()
      require("autoclose").setup()
    end
  },

  {
    'kylechui/nvim-surround',
    config = function()
      require('nvim-surround').setup()
    end
  },
}
