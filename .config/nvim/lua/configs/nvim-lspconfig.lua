local M = {}

M.ybbond_lsp_on_attach = function(_, bufnr)
  local function buf_set_keymap(l, r, c)
    vim.api.nvim_buf_set_keymap(bufnr, l, r, c, { noremap=true, silent=true })
  end
  buf_set_keymap('n', 'gd', '<CMD>lua vim.lsp.buf.definition()<CR>')
  buf_set_keymap('n', 'gD', '<CMD>lua vim.lsp.buf.declaration()<CR>')
  buf_set_keymap('n', 'g<A-d>', '<CMD>lua vim.lsp.buf.type_definition()<CR>')
  buf_set_keymap('n', 'gh', '<CMD>lua vim.lsp.buf.hover()<CR>')
  buf_set_keymap('n', 'gH', '<CMD>lua vim.diagnostic.open_float()<CR>')
  buf_set_keymap('n', 'gi', '<CMD>lua vim.lsp.buf.implementation()<CR>')
  buf_set_keymap('n', 'gs', '<CMD>lua vim.lsp.buf.signature_help()<CR>')
  buf_set_keymap('n', 'ga', '<CMD>lua vim.lsp.buf.code_action()<CR>')
  -- buf_set_keymap('n', 'ga', '<CMD>Telescope lsp_code_actions theme=cursor layout_config={height=15}<CR>')
  buf_set_keymap('n', 'gr', '<CMD>lua vim.lsp.buf.references()<CR>')
  buf_set_keymap('n', '[e', '<CMD>lua vim.diagnostic.goto_prev()<CR>')
  buf_set_keymap('n', ']e', '<CMD>lua vim.diagnostic.goto_next()<CR>')
  -- buf_set_keymap('n', '<LEADER>f', '<CMD>lua vim.lsp.buf.formatting()<CR>')
  -- buf_set_keymap('n', '<LEADER>f', [[<CMD>lua require('format-on-save').format()<CR><CMD>lua require('format-on-save').restore_cursors()<CR>]])
  buf_set_keymap('n', '<LEADER>f', '<CMD>Format<CR>')
end

-- return require'cmp_nvim_lsp'.default_capabilities(vim.lsp.protocol.make_client_capabilities())
M.ybbond_lsp_capabilities = require'cmp_nvim_lsp'.default_capabilities()

local lspconfig = require('lspconfig')

local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

local lua_ls_settings = {
  runtime = {
    version = 'LuaJIT',
    path = runtime_path,
  },
  workspace = {
    checkThirdParty = false,
  },
  telemetry = { enable = false, },
}
if vim.fn.getcwd() == os.getenv('HOME') .. '/.config/nvim'
  or string.find(vim.fn.getcwd(), os.getenv('HOME') .. '/poss/nvim') then
  lua_ls_settings['diagnostics'] = { globals = {'vim'}, }
  lua_ls_settings['workspace']['library'] = vim.api.nvim_get_runtime_file("", true)
end

lspconfig.lua_ls.setup{
  capabilities = M.ybbond_lsp_capabilities,
  cmd = {
    os.getenv('HOME') .. "/.tool_binaries/lua-language-server" .. "/bin/lua-language-server",
    "-E",
    os.getenv('HOME') .. "/.tool_binaries/lua-language-server" .. "/main.lua"
  },
  on_attach = M.ybbond_lsp_on_attach,
  filetypes = { "lua" },
  settings = { Lua = lua_ls_settings },
}

lspconfig.tsserver.setup{
  capabilities = M.ybbond_lsp_capabilities,
  on_attach = M.ybbond_lsp_on_attach,
}

lspconfig.ocamllsp.setup{
  capabilities = M.ybbond_lsp_capabilities,
  on_attach = M.ybbond_lsp_on_attach,
}

lspconfig.zls.setup {
  on_attach = M.ybbond_lsp_on_attach,
  capabilities = M.ybbond_lsp_capabilities,
}

-- lspconfig.dartls.setup{
--   capabilities = M.ybbond_lsp_capabilities,
--   on_attach = M.ybbond_lsp_on_attach,
--   init_options = {
--     onlyAnalyzeProjectsWithOpenFiles = false,
--   },
-- }

return M
