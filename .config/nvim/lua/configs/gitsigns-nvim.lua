require('gitsigns').setup {
  -- signs = {
  --   add          = {hl = 'Ignore', text = '█', numhl='GitSignsAddLn'   , linehl='GitSignsAddNr', word_diff='GitSignsAddWord'},
  --   change       = {hl = 'Ignore', text = '█', numhl='GitSignsChangeLn', linehl='GitSignsChangeNr', word_diff='GitSignsChangeWord'},
  --   delete       = {hl = 'Ignore', text = '▂', numhl='GitSignsDeleteLn', linehl='GitSignsDeleteNr', word_diff='GitSignsDeleteWord'},
  --   topdelete    = {hl = 'Ignore', text = '▀', numhl='GitSignsDeleteLn', linehl='GitSignsDeleteNr', word_diff='GitSignsDeleteWord'},
  --   changedelete = {hl = 'Ignore', text = '█', numhl='GitSignsChangeLn', linehl='GitSignsChangeNr', word_diff='GitSignsChangeWord'},
  -- },
  signcolumn = false,
  numhl = true,
  on_attach = function(bufnr)
    local c = require('vscode.colors').get_colors()
    vim.api.nvim_set_hl(0, 'GitSignsAddNr', { bg=c.vscLightGreen, fg=c.vscCursorDarkDark })
    vim.api.nvim_set_hl(0, 'GitSignsChangeNr', { bg=c.vscYellow, fg=c.vscCursorDarkDark })
    vim.api.nvim_set_hl(0, 'GitSignsChangedeleteNr', { bg=c.vscYellow, fg=c.vscCursorDarkDark })
    vim.api.nvim_set_hl(0, 'GitSignsDeleteNr', { bg=c.vscLightRed, fg=c.vscCursorDarkDark })
    vim.api.nvim_set_hl(0, 'GitSignsTopdeleteNr', { bg=c.vscLightRed, fg=c.vscCursorDarkDark })

    local gs = package.loaded.gitsigns
    local wk = require('which-key')

    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      opts.noremap = true
      opts.silent = true
      vim.keymap.set(mode, l, r, opts)
    end

    -- map('n', ']g', [[&diff ? ']g' : '<cmd>Gitsigns next_hunk<CR>']], ({expr = true}))
    -- map('n', '[g', [[&diff ? '[g' : '<cmd>Gitsigns prev_hunk<CR>']], ({expr = true}))
    map('n', ']c', function()
      if vim.wo.diff then return ']c' end
      vim.schedule(function() gs.next_hunk() end)
      return '<Ignore>'
    end, {expr=true})

    map('n', '[c', function()
      if vim.wo.diff then return '[c' end
      vim.schedule(function() gs.prev_hunk() end)
      return '<Ignore>'
    end, {expr=true})

    wk.add({
      { ']c', desc = 'Gitsigns next hunk or plain ]c' },
      { '[c', desc = 'Gitsigns previous hunk or plain [c' },
    })

    map('n', '<C-g><C-s>',        gs.stage_hunk)
    map('v', '<LEADER>gs',        gs.stage_hunk)
    map('n', '<C-g><C-u>',        gs.undo_stage_hunk)
    map('n', '<C-g><C-r><C-h>',   gs.reset_hunk)
    map('v', '<LEADER>gr',        gs.reset_hunk)
    map('n', '<C-g><C-r><C-b>',   gs.reset_buffer)
    map('n', '<C-g><C-p>',        gs.preview_hunk)
    map('n', '<C-g><C-b>',        function() gs.blame_line{full=true} end)
    map('n', '<C-g><C-\\><C-b>',  function() gs.blame_line{full=true, ignore_whitespace=true} end)

    wk.add({
      { '<C-g><C-s>',        desc = 'Gitsigns stage hunk' },
      { '<LEADER>gs',        desc = 'Gitsigns stage hunk', mode = 'v' },
      { '<C-g><C-u>',        desc = 'Gitsigns UNDO stage hunk' },
      { '<C-g><C-r><C-h>',   desc = 'Gitsigns reset hunk' },
      { '<LEADER>gr',        desc = 'Gitsigns reset hunk', mode = 'v' },
      { '<C-g><C-r><C-b>',   desc = 'Gitsigns reset buffer' },
      { '<C-g><C-p>',        desc = 'Gitsigns preview hunk' },
      { '<C-g><C-b>',        desc = 'Gitsigns blame line' },
      { '<C-g><C-\\><C-b>',  desc = 'Gitsigns blame line WITHOUT whitespace' },
    })

    map({'o', 'x'}, 'ih',                ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>')
  end,
  watch_gitdir = {
    interval = 1000,
    follow_files = true
  },
}
