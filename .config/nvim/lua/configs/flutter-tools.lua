local ybbond_lsp_on_attach = require('configs/nvim-lspconfig')

local ybbond_flutter_lsp_on_attach = function(client, bufnr)
  ybbond_lsp_on_attach(client, bufnr);

  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end

  local opts = { noremap=true, silent=true }

  buf_set_keymap('n', 'gA', '<CMD>lua require("telescope").extensions.flutter.commands()<CR>', opts)
  buf_set_keymap('n', '<LEADER>f', '<CMD>DartFmt<CR>', opts)

  require('flutter-tools.utils').command("FlutterAgentStartStaging", [[lua require('flutter-tools.commands').run_command('--flavor=staging --no-sound-null-safety')]])
  require('flutter-tools.utils').command("FlutterAgentStartStagingNoPub", [[lua require('flutter-tools.commands').run_command('--flavor=staging --no-sound-null-safety --no-pub')]])
  require('flutter-tools.utils').command("FlutterAgentStartProduction", [[lua require('flutter-tools.commands').run_command('--flavor=production --no-sound-null-safety')]])
  require('flutter-tools.utils').command("FlutterRikuStartStaging", [[lua require('flutter-tools.commands').run_command('--flavor=mandiristaging --no-sound-null-safety')]])
  require('flutter-tools.utils').command("FlutterRikuStartProduction", [[lua require('flutter-tools.commands').run_command('--flavor=mandiriproduction --no-sound-null-safety')]])

end

require("flutter-tools").setup {
  lsp = {
    on_attach = ybbond_flutter_lsp_on_attach,
  },
  -- flutter_path = os.getenv('HOME').."/fvm/default/bin/flutter",
  -- flutter_path = os.getenv('HOME').."/flutter/bin/flutter",

  closing_tags = {
    prefix = ' → ',
  },

  fvm = true,

  widget_guides = {
    enabled = true,
  },
}

require('telescope').load_extension('flutter')
