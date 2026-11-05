vim.lsp.config('*', {
  capabilities = require'cmp_nvim_lsp'.default_capabilities(),
})

-- keymaps via LspAttach, not on_attach, so lspconfig's own on_attach (LspEslintFixAll, LspTexlab*, LspPyright*) survives
vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local bufnr = args.buf
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
    -- buf_set_keymap('n', '<LEADER>f', '<CMD>lua vim.lsp.buf.formatting()<CR>')
    -- buf_set_keymap('n', '<LEADER>f', [[<CMD>lua require('format-on-save').format()<CR><CMD>lua require('format-on-save').restore_cursors()<CR>]])
    -- buf_set_keymap('n', '<LEADER>f', '<CMD>Format<CR>')
    -- fallback when no plugin (conform) owns <leader>f; checks the mapping so lazy-loaded conform isn't loaded early
    if vim.tbl_isempty(vim.fn.maparg('<LEADER>f', 'n', false, true)) then
      buf_set_keymap('n', '<LEADER>f', '<CMD>lua vim.lsp.buf.format({ async = true })<CR>')
    end

    local client_name = vim.lsp.get_client_by_id(args.data.client_id).name
    if client_name == 'eslint' then
      buf_set_keymap('n', '<LEADER>F', '<CMD>LspEslintFixAll<CR>')
    elseif client_name == 'oxlint' then
      buf_set_keymap('n', '<LEADER>F', '<CMD>LspOxlintFixAll<CR>')
    end
  end,
})

-- local lspconfig = require('lspconfig')

-- Neovim runtime and luv types only; plugin modules stay untyped to keep indexing fast
vim.lsp.config('lua_ls', {
  filetypes = { 'lua' },
  settings = {
    Lua = {
      runtime = { version = 'LuaJIT' },
      workspace = {
        checkThirdParty = false,
        library = { vim.env.VIMRUNTIME, '${3rd}/luv/library' },
      },
      telemetry = { enable = false },
    },
  },
})
vim.lsp.enable('lua_ls')

-- vim.lsp.enable('gopls')

-- vim.lsp.enable('tsc')

vim.lsp.config('jsonls', {
  settings = {
    json = {
      schemas = require('schemastore').json.schemas(),
      validate = { enable = true },
    },
  },
})
vim.lsp.enable('jsonls')

-- -- after TypeScript 7
-- vim.lsp.config('astro', {
--   before_init = function(_, config)
--     local util = require('lspconfig.util')
--     local tsdk = util.get_typescript_server_path(config.root_dir)
--     if tsdk == '' then
--       local npm_root = vim.fn.systemlist('npm root -g')
--       if vim.v.shell_error == 0 and npm_root[1] then
--         tsdk = npm_root[1] .. '/typescript/lib'
--       end
--     end
--     config.init_options = config.init_options or {}
--     config.init_options.typescript = config.init_options.typescript or {}
--     config.init_options.typescript.tsdk = tsdk
--   end,
-- })
vim.lsp.enable('astro')

vim.lsp.enable('cssls')

vim.lsp.enable('eslint')

local oxlint_root_dir = vim.lsp.config.oxlint.root_dir
vim.lsp.config('oxlint', {
  -- upstream calls on_dir(nil) when no .oxlintrc.json is found, which spawns a rootless `oxlint --lsp` in every JS project
  root_dir = function(bufnr, on_dir)
    oxlint_root_dir(bufnr, function(dir)
      if dir then on_dir(dir) end
    end)
  end,
})
vim.lsp.enable('oxlint')

-- vim.lsp.enable('rust_analyzer')

-- vim.lsp.enable('hls')

vim.lsp.config('dartls', {
  init_options = {
    onlyAnalyzeProjectsWithOpenFiles = false,
  },
})
vim.lsp.enable('dartls')

vim.lsp.enable('sourcekit')

vim.lsp.enable('texlab')

-- vim.lsp.enable('zls')

-- vim.lsp.enable('jdtls')

vim.lsp.enable('css_variables')

vim.lsp.enable('basedpyright')

vim.lsp.enable('yamlls')
