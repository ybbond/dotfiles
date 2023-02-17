local lspconfig = require('lspconfig')

local ybbond_lsp_on_attach = require'configs/ybbond_lsp_on_attach'
local ybbond_lsp_capabilities = require'configs/ybbond_lsp_capabilities'

lspconfig.tsserver.setup {
  capabilities = ybbond_lsp_capabilities,
  on_attach = ybbond_lsp_on_attach,
}
lspconfig.eslint.setup {
  capabilities = ybbond_lsp_capabilities,
  on_attach = function(client, bufnr)
    ybbond_lsp_on_attach(client, bufnr)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<LEADER>f', '<CMD>EslintFixAll<CR>', { noremap = true, silent = true })
  end,
  -- https://neovim.discourse.group/t/supressing-eslint-ls-errors/1687/4
  handlers = {
    ['window/showMessageRequest'] = function(_, result, __) return result end
  },
  settings = {
    packageManager = 'yarn',
    format = true,
    codeActionOnSave = {
      enable = false,
      mode = "all"
    },
  },
}

lspconfig.sourcekit.setup{
  capabilities = ybbond_lsp_capabilities,
  filetypes = { "swift", "objective-c", "objective-cpp" },
}

lspconfig.clangd.setup{
  capabilities = ybbond_lsp_capabilities,
  filetypes = { 'c', 'cpp' },
  cmd = {
    -- "clangd",
    -- "/Library/Developer/CommandLineTools/usr/bin/clangd",
    "xcrun", "clangd",
    -- "-I/opt/homebrew/lib",
    -- "-I/opt/homebrew/include",
    "--completion-style=bundled",
    "--clang-tidy",
    "--suggest-missing-includes",
    "--background-index",
    "--header-insertion=iwyu",
  },
  on_attach = ybbond_lsp_on_attach,
}

lspconfig.vls.setup{
  capabilities = ybbond_lsp_capabilities,
  on_attach = ybbond_lsp_on_attach,
}

lspconfig.denols.setup{
  init_options = {
    enable = true,
    unstable = true,
  },
  root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc"),
  capabilities = ybbond_lsp_capabilities,
  on_attach = ybbond_lsp_on_attach,
}

lspconfig.clojure_lsp.setup{
  capabilities = ybbond_lsp_capabilities,
  on_attach = ybbond_lsp_on_attach
}

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
  capabilities = ybbond_lsp_capabilities,
  cmd = {
    os.getenv('HOME') .. "/.tool_binaries/lua-language-server" .. "/bin/lua-language-server",
    "-E",
    os.getenv('HOME') .. "/.tool_binaries/lua-language-server" .. "/main.lua"
  },
  on_attach = ybbond_lsp_on_attach,
  filetypes = { "lua" },
  settings = { Lua = lua_ls_settings },
}
