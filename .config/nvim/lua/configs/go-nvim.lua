local ybbond_go_lsp_on_attach = function(_, bufnr)
  require'configs/nvim-lspconfig'.ybbond_lsp_on_attach(_, bufnr);

  local function buf_set_keymap(l, r, c)
    vim.api.nvim_buf_set_keymap(bufnr, l, r, c, { noremap=true, silent=true })
  end

  local function buf_create_command(name, cmd)
    vim.api.nvim_buf_create_user_command(bufnr, name, cmd, {})
  end

  buf_set_keymap('n', 'gh', '<CMD>GoDoc<CR>')
  buf_set_keymap('n', 'gt', '<CMD>GoAddTag<CR>')
  buf_set_keymap('n', 'gT', '<CMD>GoRmTag<CR>')
  buf_set_keymap('n', 'gA', '<CMD>GoAlt<CR>')
  buf_set_keymap('n', 'gi', '<CMD>GoImplements<CR>')

  -- buf_set_keymap('n', '<LEADER>f', '<CMD>lua require("go.format").gofmt()<CR>')
  buf_set_keymap('n', '<LEADER>F', '<CMD>lua require("go.format").goimport()<CR>')

  buf_create_command('GoTestFuncCoverage', 'GoTestFunc -C coverage.out')
  buf_create_command('GoTestFileCoverage', 'GoTestFile -C coverage.out')
end

require('go').setup({
  gofmt = 'gopls',
  -- lsp_cfg = {
  --   capabilities = ybbond_lsp_capabilities,
  -- },
  -- lsp_on_attach = ybbond_go_lsp_on_attach,
  lsp_cfg = true,
  tag_options = 'json=',
  tag_transform = 'camelcase',
})

-- local cfg = require'go.lsp'.config()
-- require('lspconfig').gopls.setup(cfg)

require('lspconfig').gopls.setup{
  capabilities = require'cmp_nvim_lsp'.default_capabilities(),
  on_attach = ybbond_go_lsp_on_attach,
}
