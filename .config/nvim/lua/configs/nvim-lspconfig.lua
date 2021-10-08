local lspconfig = require('lspconfig')
local lspconfig_util = require('lspconfig.util')

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  require('lsp_extensions.workspace.diagnostic').handler, {
    -- signs = {
    --   severity_limit = "Error",
    -- }
  }
)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local ybbond_lsp_on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gd', '<CMD>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'gD', '<CMD>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'g<M-d>', '<CMD>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', 'gh', '<CMD>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gH', '<CMD>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', 'gi', '<CMD>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', 'gs', '<CMD>lua vim.lsp.buf.signature_help()<CR>', opts)
  -- buf_set_keymap('n', 'ga', '<CMD>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'ga', '<CMD>CodeActionMenu<CR>', opts)
  -- gA will be received to language specific actions
  buf_set_keymap('n', 'gr', '<CMD>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '[e', '<CMD>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']e', '<CMD>lua vim.lsp.diagnostic.goto_next()<CR>', opts)

  -- buf_set_keymap('n', '<space>wa', '<CMD>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  -- buf_set_keymap('n', '<space>wr', '<CMD>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  -- buf_set_keymap('n', '<space>wl', '<CMD>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  -- buf_set_keymap('n', '<space>rn', '<CMD>lua vim.lsp.buf.rename()<CR>', opts)
  -- buf_set_keymap('n', '<space>q', '<CMD>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  -- buf_set_keymap("n", "<space>f", "<CMD>lua vim.lsp.buf.formatting()<CR>", opts)

end

lspconfig.clangd.setup{
  filetype = { 'c', 'cpp' },
  filetypes = { 'c', 'cpp' },
  on_attach = ybbond_lsp_on_attach,
}

lspconfig.sourcekit.setup{
  filetype = { 'swift', 'objc', 'objcpp', 'objective-c', 'objective-cpp' },
  filetypes = { 'swift', 'objc', 'objcpp', 'objective-c', 'objective-cpp' },
  on_attach = ybbond_lsp_on_attach,
  -- root_dir = lspconfig_util.root_pattern("compile_commands.json", "compile_flags.txt", ".git") or lspconfig_util.path.dirname
  root_dir = lspconfig_util.path.dirname
}

-- lspconfig.bashls.setup{
--   on_attach = ybbond_lsp_on_attach,
-- }

return ybbond_lsp_on_attach
