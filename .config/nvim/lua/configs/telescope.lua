local previewers = require('telescope.previewers')
local previewers_utils = require('telescope.previewers.utils')

local max_size = 20000

-- https://github.com/nvim-telescope/telescope.nvim/issues/623
local new_maker = function(filepath, bufnr, opts)
  opts = opts or {}

  filepath = vim.fn.expand(filepath)
  vim.loop.fs_stat(filepath, function(_, stat)
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
  },
}

require('telescope').load_extension('flutter')
