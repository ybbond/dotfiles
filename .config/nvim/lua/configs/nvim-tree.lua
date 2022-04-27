require'nvim-tree'.open_on_directory = function () end

nnoremap '<C-s><C-b>' '<CMD>NvimTreeToggle<CR>'
nnoremap '<C-s>b'     '<CMD>NvimTreeToggle<CR>'
nnoremap '<C-s><C-f>'  '<CMD>NvimTreeFindFile<CR>'
nnoremap '<C-s>f'     '<CMD>NvimTreeFindFile<CR>'

require'nvim-tree'.setup {
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
  view = {
    hide_root_folder = true,
    width = 40,
    side = 'right',
    mappings = {
      custom_only = true,
      list = {
        { key = {"<CR>", "o", "<2-LeftMouse>"}, action = "edit" },
        { key = {"O"},                          action = "edit_no_picker" },
        { key = {"<2-RightMouse>", "<C-]>"},    action = "cd" },
        { key = "<",                            action = "prev_sibling" },
        { key = ">",                            action = "next_sibling" },
        { key = "P",                            action = "parent_node" },
        { key = "<BS>",                         action = "close_node" },
        { key = "<Tab>",                        action = "preview" },
        { key = "K",                            action = "first_sibling" },
        { key = "J",                            action = "last_sibling" },
        { key = "I",                            action = "toggle_ignored" },
        { key = "H",                            action = "toggle_dotfiles" },
        { key = "R",                            action = "refresh" },
        { key = "a",                            action = "create" },
        { key = "d",                            action = "remove" },
        { key = "D",                            action = "trash" },
        { key = "r",                            action = "rename" },
        { key = "<C-r>",                        action = "full_rename" },
        { key = "x",                            action = "cut" },
        { key = "c",                            action = "copy" },
        { key = "p",                            action = "paste" },
        { key = "y",                            action = "copy_name" },
        { key = "Y",                            action = "copy_path" },
        { key = "gy",                           action = "copy_absolute_path" },
        { key = {"[c", "[g"},                   action = "prev_git_item" },
        { key = {"]c", "]g"},                   action = "next_git_item" },
        { key = "-",                            action = "dir_up" },
        { key = "s",                            action = "system_open" },
        { key = "q",                            action = "close" },
        { key = "g?",                           action = "toggle_help" },
      },
    },
  },
}
