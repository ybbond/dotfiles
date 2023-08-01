vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

nnoremap '<C-s><C-b>' '<CMD>NvimTreeToggle<CR>'
nnoremap '<C-s>b'     '<CMD>NvimTreeToggle<CR>'
nnoremap '<C-s><C-f>'  '<CMD>NvimTreeFindFile<CR>'
nnoremap '<C-s>f'     '<CMD>NvimTreeFindFile<CR>'

require'nvim-tree'.setup {
  git = {
    enable = true,
  },
  -- hijack_unnamed_buffer_when_opening = true,
  actions = {
    open_file = {
      resize_window = false,
      quit_on_open = false,
    },
  },
  hijack_directories = {
  --   enable = true,
    auto_open = false,
  },
  diagnostics = {
    enable = true,
    show_on_dirs = true,
  },
  update_focused_file = {
    enable = true,
    update_cwd = false,
  },
  system_open = {
    cmd = 'open',
  },
  renderer = {
    root_folder_label = false,
  },
  view = {
    adaptive_size = true,
    width = 40,
    side = 'right',
  },
}
