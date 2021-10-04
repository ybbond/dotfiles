-- require'nvim-tree'.open_on_directory = function() end
require'nvim-tree'.setup {
  disable_netrw = false,
  hijack_netrw = true,
  open_on_setup = false,
  open_on_tab = true,
  update_cwd = false,
  lsp_diagnostics = true,
  update_focused_file = {
    enable = true,
    update_cwd = false,
  },
  view = {
    width = 40,
    side = 'right',
    auto_resize = false,
  }
}
