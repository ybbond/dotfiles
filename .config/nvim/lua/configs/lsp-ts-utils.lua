local lsp_on_attach = require('configs/nvim-lspconfig')
local nvim_lsp = require("lspconfig")

-- enable null-ls integration (optional)
require("null-ls").setup {}

nvim_lsp.tsserver.setup {
  on_attach = function(client, bufnr)
    -- disable tsserver formatting if you plan on formatting via null-ls
    client.resolved_capabilities.document_formatting = false

    local ts_utils = require("nvim-lsp-ts-utils")

    -- defaults
    ts_utils.setup {
      debug = false,
      disable_commands = false,
      enable_import_on_completion = false,

      -- import all
      import_all_timeout = 5000, -- ms
      import_all_priorities = {
        buffers = 4, -- loaded buffer names
        buffer_content = 3, -- loaded buffer content
        local_files = 2, -- git files or files with relative path markers
        same_file = 1, -- add to existing import statement
      },
      import_all_scan_buffers = 100,
      import_all_select_source = false,

      -- eslint
      eslint_enable_code_actions = true,
      eslint_enable_disable_comments = true,
      eslint_bin = "eslint",
      eslint_config_fallback = nil,
      eslint_enable_diagnostics = false,

      -- formatting
      enable_formatting = true,
      formatter = "eslint",
      formatter_config_fallback = "/Users/yohanesbandung/.config/nvim/lua/assets/.prettierrc",

      -- update imports on file move
      update_imports_on_move = false,
      require_confirmation_on_move = false,
      watch_dir = nil,
    }

    -- required to fix code action ranges
    ts_utils.setup_client(client)

    vim.cmd("command -buffer FormatTS lua vim.lsp.buf.formatting()")

    lsp_on_attach(client, bufnr);

    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    --Enable completion triggered by <c-x><c-o>
    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    local opts = { noremap=true, silent=true }

    buf_set_keymap('n', '<LEADER>f', '<CMD>lua vim.lsp.buf.formatting()<CR>', opts)

  end
}

