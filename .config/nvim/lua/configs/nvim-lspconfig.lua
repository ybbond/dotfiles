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
  -- buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gd', '<CMD>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'gD', '<CMD>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'g<A-d>', '<CMD>lua vim.lsp.buf.type_definition()<CR>', opts)
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

-- lspconfig.tsserver.setup {
--   on_attach = ybbond_lsp_on_attach,
-- }
-- lspconfig.eslint.setup {
--   on_attach = function(client, bufnr)
--     ybbond_lsp_on_attach(client, bufnr)
--     vim.api.nvim_buf_set_keymap(bufnr, 'n', '<LEADER>f', '<CMD>EslintFixAll<CR>', { noremap = true, silent = true })
--   end
-- }

lspconfig.clangd.setup{
  filetypes = { 'c', 'cpp' },
  cmd = {
    -- "clangd",

    -- "/Library/Developer/CommandLineTools/usr/bin/clangd",

    "xcrun", "clangd",

    -- "-I/opt/homebrew/lib",
    -- "-I/opt/homebrew/include",
    "--completion-style=bundled",
    "--suggest-missing-includes",
    "--background-index",
  },
  on_attach = ybbond_lsp_on_attach,
}

lspconfig.gopls.setup{
  on_attach = ybbond_lsp_on_attach,
}

lspconfig.zls.setup{
  cmd = {
    os.getenv('HOME').."/poss/zls/zig-out/bin/zls",
  },
  on_attach = ybbond_lsp_on_attach,
}

lspconfig.sourcekit.setup{
  filetypes = { 'swift', 'objc', 'objcpp', 'objective-c', 'objective-cpp' },
  on_attach = ybbond_lsp_on_attach,
  -- root_dir = lspconfig_util.root_pattern("compile_commands.json", "compile_flags.txt", ".git") or lspconfig_util.path.dirname
  root_dir = lspconfig_util.path.dirname
}

require'lspconfig'.clojure_lsp.setup{
  on_attach = ybbond_lsp_on_attach
}

local sumneko_root_path = os.getenv('HOME') .. "/poss/lua-language-server"
local sumneko_binary = sumneko_root_path .. "/bin/macOS/lua-language-server"

local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

lspconfig.sumneko_lua.setup{
  cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"},
  on_attach = ybbond_lsp_on_attach,
  filetypes = { "lua" },
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = 'Lua 5.4.3',
        -- Setup your lua path
        path = runtime_path,
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'},
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  }
}

return ybbond_lsp_on_attach
