local lspconfig = require('lspconfig')
local null_ls = require('null-ls')

local ybbond_lsp_on_attach = require'configs/nvim-lspconfig'

lspconfig.tsserver.setup {
  init_options = require("nvim-lsp-ts-utils").init_options,

  on_attach = function(client, bufnr)
    client.resolved_capabilities.document_formatting = false
    client.resolved_capabilities.document_range_formatting = false

    local ts_utils = require("nvim-lsp-ts-utils")

    ts_utils.setup({
      auto_inlay_hints = true,
      inlay_hints_highlight = "Comment",

      enable_import_on_completion = true,
    })

    ybbond_lsp_on_attach(client, bufnr);

    ts_utils.setup_client(client)

    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<LEADER>f', '<CMD>lua vim.lsp.buf.formatting()<CR>', {})
  end,
}

null_ls.setup({
  sources = {
    null_ls.builtins.diagnostics.eslint_d,
    null_ls.builtins.code_actions.eslint_d,
    null_ls.builtins.formatting.eslint_d,
    null_ls.builtins.diagnostics.eslint_d,
  },
  on_attach = ybbond_lsp_on_attach,
})

-- lspconfig["null-ls"].setup {
--   autostart = true,
--   on_attach = ybbond_lsp_on_attach
-- }
