require('gitsigns').setup {
  signs = {
    add          = {hl = 'Ignore'   , text = '█', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn', word_diff='GitSignsAddWord'},
    change       = {hl = 'Ignore', text = '█', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn', word_diff='GitSignsChangeWord'},
    delete       = {hl = 'Ignore', text = '▂', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn', word_diff='GitSignsDeleteWord'},
    topdelete    = {hl = 'Ignore', text = '▀', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn', word_diff='GitSignsDeleteWord'},
    changedelete = {hl = 'Ignore', text = '█', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn', word_diff='GitSignsChangeWord'},
  },
  signcolumn = false,
  numhl = true,
  on_attach = function(bufnr)
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
    map('n', ']g', function()
      if vim.wo.diff then return ']g' end
      vim.schedule(function() gs.next_hunk() end)
      return '<Ignore>'
    end, {expr=true})

    map('n', '[g', function()
      if vim.wo.diff then return '[g' end
      vim.schedule(function() gs.prev_hunk() end)
      return '<Ignore>'
    end, {expr=true})

    wk.register({
      [']'] = { g = 'Gitsigns next hunk or plain ]g' },
      ['['] = { g = 'Gitsigns previous hunk or plain [g' },
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

    wk.register({
      ['<C-g>'] = {
        ['<C-s>'] = 'Gitsigns stage hunk',
        ['<C-u>'] = 'Gitsigns UNDO stage hunk',
        ['<C-r>'] = {
          ['<C-h>'] = 'Gitsigns reset hunk',
          ['<C-b>'] = 'Gitsigns reset buffer',
        },
        ['<C-p>'] = 'Gitsigns preview hunk',
        ['<C-b>'] = 'Gitsigns blame line',
        ['<C-\\>'] = {
          ['<C-b>'] = 'Gitsigns blame line WITHOUT whitespace',
        },
      },
    },
    {
      buffer = bufnr,
    })

    wk.register({
      g = {
        s = { 'Gitsigns stage hunk' },
        r = { 'Gitsigns reset hunk' },
      },
    },
    {
      prefix = '<leader>',
      mode = 'v',
      buffer = bufnr,
    })

    map({'o', 'x'}, 'ih',                ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>')
  end,
  watch_gitdir = {
    interval = 1000,
    follow_files = true
  },
}
