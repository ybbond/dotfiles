local previewers = require('telescope.previewers')
local previewers_utils = require('telescope.previewers.utils')

local actions = require('telescope.actions')

nnoremap '<C-t><C-p>'        [[<CMD>lua require("telescope.builtin").find_files({ find_command={"fd","-E=.git","--hidden","-t=f"}}) hidden=true<CR>]]
nnoremap '<C-t><C-\\><C-p>'  [[<CMD>lua require("telescope.builtin").find_files({ find_command={"fd","-E=.git","--hidden","-t=f","--no-ignore"}}) hidden=true<CR>]]
nnoremap '<C-t><C-i>'        [[<CMD>Telescope live_grep hidden=true<CR>]]
nnoremap '<C-t><C-\\><C-i>'  [[<CMD>Telescope live_grep hidden=true grep_open_files=true<CR>]]
nnoremap '<C-t><C-n>'        [[<CMD>lua require("telescope.builtin").live_grep({ find_command={"rg","--no-heading","--hidden","-g='!.git/**'","--with-filename","--line-number","--column","--smart-case","--ignore","--regexp"}})<CR>]]
nnoremap '<C-t><C-\\><C-n>'  [[<CMD>lua require("telescope.builtin").live_grep({ find_command={"rg","--no-heading","--hidden","-g='!.git/**'",'--with-filename',"--line-number","--column","--smart-case","--no-ignore","--regexp"}})<CR>]]

nnoremap '<C-t><C-s>'        [[<CMD>Telescope grep_string<CR>]]
nnoremap '<C-t><C-r>'        [[<CMD>Telescope registers<CR>]]
nnoremap '<C-t><C-b>'        [[<CMD>Telescope buffers<CR>]]
nnoremap '<C-t><C-h>'        [[<CMD>Telescope help_tags<CR>]]
nnoremap '<C-t><C-m>'        [[<CMD>Telescope marks<CR>]]
nnoremap '<C-t><C-a>'        [[<CMD>Telescope commands<CR>]]

nnoremap '<C-t><C-w>'        [[<CMD>Telescope diagnostics<CR>]]
nnoremap '<C-t><C-d>'        [[<CMD>Telescope diagnostics bufnr=0<CR>]]

nnoremap '<C-g><C-g>'        [[<CMD>Telescope git_status<CR>]]

-- https://github.com/nvim-telescope/telescope.nvim/issues/623
local max_size = 20000

local new_maker = function(filepath, bufnr, opts)
  opts = opts or {}

  filepath = vim.fn.expand(filepath)
  -- deprecated https://github.com/neovim/neovim/pull/22846
  -- vim.loop.fs_stat(filepath, function(_, stat)
  vim.uv.fs_stat(filepath, function(_, stat)
    if not stat then return end
    if stat.size > max_size then
      local cmd = {"head", "-c", max_size, filepath}
      previewers_utils.job_maker(cmd, bufnr, opts)
    else
      previewers.buffer_previewer_maker(filepath, bufnr, opts)
    end
  end)
end

require('telescope').setup{
  defaults = {
    sorting_strategy = "ascending",
    layout_strategy = "vertical",
    scroll_strategy = "cycle",
    layout_config = {
      horizontal = {
        mirror = false,
      },
      vertical = {
        mirror = true,
        prompt_position = 'top',
      },
    },
    file_sorter =  require'telescope.sorters'.get_fuzzy_file,
    -- generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
    generic_sorter =  require'telescope.sorters'.get_fzy_sorter,
    buffer_previewer_maker = new_maker,
    mappings = {
      i = {
        ['œ'] = actions.send_selected_to_qflist + actions.open_qflist,
      }
    }
  },
}

require('telescope').load_extension('fzf')
