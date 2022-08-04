local ybbond_lsp_on_attach = require'configs/ybbond_lsp_on_attach'
local ybbond_lsp_capabilities = require'configs/ybbond_lsp_capabilities'

local ybbond_go_lsp_on_attach = function(client, bufnr)
  ybbond_lsp_on_attach(client, bufnr);

  local function buf_set_keymap(l, r, c)
    vim.api.nvim_buf_set_keymap(bufnr, l, r, c, { noremap=true, silent=true })
  end

  buf_set_keymap('n', 'gh', '<CMD>GoDoc<CR>')
  buf_set_keymap('n', 'gt', '<CMD>GoAddTag<CR>')
  buf_set_keymap('n', 'gT', '<CMD>GoRmTag<CR>')
  buf_set_keymap('n', 'gA', '<CMD>GoAlt<CR>')

  buf_set_keymap('n', '<LEADER>f', '<CMD>lua require("go.format").gofmt()<CR>')
  buf_set_keymap('n', '<LEADER>F', '<CMD>lua require("go.format").goimport()<CR>')

end

require('go').setup({
  gofmt = 'gopls',
  -- lsp_codelens = false,
})

local lspconfig = require('lspconfig')
lspconfig.gopls.setup{
  capabilities = ybbond_lsp_capabilities,
  on_attach = ybbond_go_lsp_on_attach,
}

