vim.g.nvim_tree_quit_on_open = 1

require'nvim-tree'.open_on_directory = function () end

require'nvim-tree'.setup {
  disable_netrw = false,
  hijack_netrw = true,
  open_on_setup = false,
  open_on_tab = false,
  update_cwd = false,
  diagnostics = {
    enable = true,
  },
  update_focused_file = {
    enable = true,
    update_cwd = false,
  },
  view = {
    hide_root_folder = true,
    width = 40,
    side = 'right',
    auto_resize = false,
  },
}
