require('gitsigns').setup {
  signs = {
    -- add          = {hl = 'GitSignsAdd'   , text = '│', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
    -- change       = {hl = 'GitSignsChange', text = '│', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    -- delete       = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    -- topdelete    = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    -- changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
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
  keymaps = {
    -- Default keymap options
    noremap = true,
    buffer = true,

    ['n ]c'] = { expr = true, "&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'"},
    ['n [c'] = { expr = true, "&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'"},
    ['n ]g'] = { expr = true, "&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'"},
    ['n [g'] = { expr = true, "&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'"},
    ['n <C-g><C-]>'] = { expr = true, "&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'"},
    ['n <C-g><C-[>'] = { expr = true, "&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'"},

    ['n <C-g><C-s>'] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
    ['v <LEADER>gs'] = '<cmd>lua require"gitsigns".stage_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ['n <C-g><C-u>'] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
    ['n <C-g><C-r><C-h>'] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
    ['v <LEADER>gr'] = '<cmd>lua require"gitsigns".reset_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ['n <C-g><C-r><C-b>'] = '<cmd>lua require"gitsigns".reset_buffer()<CR>',
    ['n <C-g><C-p>'] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
    ['n <C-g><C-b>'] = '<cmd>lua require"gitsigns".blame_line(true)<CR>',

    -- Text objects
    ['o ih'] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>',
    ['x ih'] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>'
  },
  watch_index = {
    interval = 1000,
    follow_files = true
  },
  -- current_line_blame_opts.delay = 1000,
  -- current_line_blame = false,
  -- current_line_blame_delay = 1000,
  -- current_line_blame_position = 'eol',
  sign_priority = 6,
  update_debounce = 100,
  status_formatter = nil, -- Use default
  diff_opts = {
    -- algorithm = 'minimal',
    internal = true,
  },
}
