local lsp_on_attach = require('configs/nvim-lspconfig')

require("flutter-tools").setup {
  lsp = {
    on_attach = lsp_on_attach,
  },
}

