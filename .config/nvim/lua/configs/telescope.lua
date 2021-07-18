require('telescope').setup{
  defaults = {
    vimgrep_arguments = {
      'rg',
      -- '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case'
    },
    sorting_strategy = "ascending",
    layout_strategy = "vertical",
    layout_config = {
      horizontal = {
        mirror = false,
      },
      vertical = {
        mirror = true,
      },
    },
  }
}
--require('telescope').load_extension('coc')

