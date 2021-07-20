local lsp_on_attach = require('configs/nvim-lspconfig')

local ybbond_flutter_lsp_on_attach = function(client, bufnr)
  lsp_on_attach(client, bufnr);

  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  buf_set_keymap('n', 'gA', '<CMD>lua require("telescope").extensions.flutter.commands()<CR>', opts)
  buf_set_keymap('n', '<LEADER>f', '<CMD>DartFmt<CR>', opts)
end

require("flutter-tools").setup {
  lsp = {
    on_attach = ybbond_flutter_lsp_on_attach,
  },
  flutter_path = "/Users/yohanesbandung/.tool_binaries/flutter/bin/flutter",
  -- flutter_lookup_cmd = "/Users/yohanesbandung/.tool_binaries/flutter/bin",
}

require('telescope').load_extension('flutter')
