local ybbond_lsp_on_attach = require('configs/nvim-lspconfig')

local ybbond_lsp_capabilities = require'cmp_nvim_lsp'.update_capabilities(vim.lsp.protocol.make_client_capabilities())

local ybbond_go_lsp_on_attach = function(client, bufnr)
  ybbond_lsp_on_attach(client, bufnr);

  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end

  local opts = { noremap=true, silent=true }

  buf_set_keymap('n', 'gh', '<CMD>GoDoc<CR>', opts)
  buf_set_keymap('n', 'gt', '<CMD>GoAddTag<CR>', opts)
  buf_set_keymap('n', 'gT', '<CMD>GoRmTag<CR>', opts)
  buf_set_keymap('n', 'gA', '<CMD>GoAlt<CR>', opts)

  buf_set_keymap('n', '<LEADER>f', '<CMD>lua require("go.format").gofmt()<CR>', opts)
  buf_set_keymap('n', '<LEADER>F', '<CMD>lua require("go.format").goimport()<CR>', opts)

end

require('go').setup({
  lsp_cfg = {
    capabilities = ybbond_lsp_capabilities,
  },
  lsp_on_attach = ybbond_go_lsp_on_attach,
  gofmt = 'gopls',
})
