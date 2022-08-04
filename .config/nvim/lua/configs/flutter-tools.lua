local ybbond_lsp_on_attach = require'configs/ybbond_lsp_on_attach'
local ybbond_lsp_capabilities = require'configs/ybbond_lsp_capabilities'

-- local lspconfig = require('lspconfig')
-- lspconfig.dartls.setup{
--   capabilities = ybbond_lsp_capabilities,
--   on_attach = ybbond_lsp_on_attach,
--   cmd = {
--     '/Users/yohanesbandung/fvm/versions/2.5.0/bin/dart',
--     '/Users/yohanesbandung/fvm/versions/2.5.0/bin/cache/dart-sdk/bin/snapshots/analysis_server.dart.snapshot',
--     '--lsp',
--   },
-- }

local ybbond_flutter_lsp_on_attach = function(client, bufnr)
  ybbond_lsp_on_attach(client, bufnr);

  local function buf_set_keymap(l, r, c)
    vim.api.nvim_buf_set_keymap(bufnr, l, r, c, { noremap=true, silent=true })
  end

  buf_set_keymap('n', 'gA', '<CMD>lua require("telescope").extensions.flutter.commands()<CR>')
  buf_set_keymap('n', '<LEADER>f', '<CMD>DartFmt<CR>')

  local flutter_command = require('flutter-tools.utils').command

  flutter_command("FlutterAgentStartStaging", [[lua require('flutter-tools.commands').run_command('--flavor=staging --no-sound-null-safety')]])
  flutter_command("FlutterAgentStartStagingNoPub", [[lua require('flutter-tools.commands').run_command('--flavor=staging --no-sound-null-safety --no-pub')]])
  flutter_command("FlutterAgentStartProduction", [[lua require('flutter-tools.commands').run_command('--flavor=production --no-sound-null-safety')]])
  flutter_command("FlutterRikuStartStaging", [[lua require('flutter-tools.commands').run_command('--flavor=mandiristaging --no-sound-null-safety')]])
  flutter_command("FlutterRikuStartProduction", [[lua require('flutter-tools.commands').run_command('--flavor=mandiriproduction --no-sound-null-safety')]])

end

require("flutter-tools").setup {
  lsp = {
    color = {
      enabled = true,
      background = true,
      virtual_text = false,
    },
    on_attach = ybbond_flutter_lsp_on_attach,
    capabilities = ybbond_lsp_capabilities,
  },
  dev_log = {
    enabled = true,
  },

  -- flutter_path = os.getenv('HOME').."/fvm/default/bin/flutter",
  -- flutter_path = os.getenv('HOME').."/flutter/bin/flutter",

  -- debugger = {
  --   enabled = true,
  --   run_via_dap = true,
  --   register_configurations = function (_)
  --     require('dap').configurations.dart = {}
  --     require('dap.ext.vscode').load_launchjs()
  --   end,
  -- },

  closing_tags = {
    prefix = ' → ',
  },

  -- flutter_lookup_cmd = '/Users/yohanesbandung/fvm/versions/2.5.0/bin',
  flutter_path = '/Users/yohanesbandung/fvm/versions/2.5.0/bin/flutter',
  fvm = true,

  widget_guides = {
    enabled = true,
  },
}

require('telescope').load_extension('flutter')
