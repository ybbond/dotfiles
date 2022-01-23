require('gitsigns').setup {
  signs = {
    add          = {hl = 'Ignore'   , text = '█', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn', word_diff='GitSignsAddWord'},
    change       = {hl = 'Ignore', text = '█', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn', word_diff='GitSignsChangeWord'},
    delete       = {hl = 'Ignore', text = '▂', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn', word_diff='GitSignsDeleteWord'},
    topdelete    = {hl = 'Ignore', text = '▀', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn', word_diff='GitSignsDeleteWord'},
    changedelete = {hl = 'Ignore', text = '█', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn', word_diff='GitSignsChangeWord'},
  },
  word_diff = false, -- requires diff_opts.internal
  signcolumn = false,
  numhl = true,
  linehl = false,
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns

    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      opts.noremap = true
      opts.silent = true
      vim.keymap.set(mode, l, r, opts)
    end

    map('n', ']c',         [[&diff ? ']c' : '<cmd>Gitsigns next_hunk<CR>']],         ({expr = true}))
    map('n', '[c',         [[&diff ? '[c' : '<cmd>Gitsigns prev_hunk<CR>']],         ({expr = true}))
    map('n', ']g',         [[&diff ? ']g' : '<cmd>Gitsigns next_hunk<CR>']],         ({expr = true}))
    map('n', '[g',         [[&diff ? '[g' : '<cmd>Gitsigns prev_hunk<CR>']],         ({expr = true}))
    map('n', '<C-g><C-]>', [[&diff ? '<C-g><C-]>' : '<cmd>Gitsigns next_hunk<CR>']], ({expr = true}))
    map('n', '<C-g><C-[>', [[&diff ? '<C-g><C-[>' : '<cmd>Gitsigns prev_hunk<CR>']], ({expr = true}))

    map('n', '<C-g><C-s>',        gs.stage_hunk)
    map('v', '<LEADER>gs',        gs.stage_hunk)
    map('n', '<C-g><C-u>',        gs.undo_stage_hunk)
    map('n', '<C-g><C-r><C-h>',   gs.reset_hunk)
    map('v', '<LEADER>gr',        gs.reset_hunk)
    map('n', '<C-g><C-r><C-b>',   gs.reset_buffer)
    map('n', '<C-g><C-p>',        gs.preview_hunk)
    map('n', '<C-g><C-b>',        function() gs.blame_line{full=true} end)
    map('n', '<C-g><C-\\><C-b>',  function() gs.blame_line{full=true, ignore_whitespace=true} end)

    map({'o', 'x'}, 'ih',                ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>')
  end,
  watch_gitdir = {
    interval = 1000,
    follow_files = true
  },
  sign_priority = 6,
  update_debounce = 100,
  status_formatter = nil, -- Use default
  diff_opts = {
    internal = true,
  },
}
