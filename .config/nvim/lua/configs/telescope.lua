local trouble = require("trouble.providers.telescope")

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
    mappings = {
      i = {
        ["<C-q>"] = trouble.open_with_trouble,
        ["<M-q>"] = trouble.open_selected_with_trouble,
        ["<A-q>"] = trouble.open_selected_with_trouble,
      },
      n = {
        ["<C-q>"] = trouble.open_with_trouble,
        ["<M-q>"] = trouble.open_selected_with_trouble,
        ["<A-q>"] = trouble.open_selected_with_trouble,
      },
    },
    file_sorter =  require'telescope.sorters'.get_fuzzy_file,
    -- generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
    generic_sorter =  require'telescope.sorters'.get_fzy_sorter,
  },
}

require('telescope').load_extension('flutter')
