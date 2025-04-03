vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

nnoremap '<C-k><C-b>' '<CMD>NvimTreeToggle<CR>'
nnoremap '<C-k>b'     '<CMD>NvimTreeToggle<CR>'
nnoremap '<C-k><C-f>'  '<CMD>NvimTreeFindFile<CR>'
nnoremap '<C-k>f'     '<CMD>NvimTreeFindFile<CR>'

require'nvim-tree'.setup {
  git = {
    enable = true,
  },
  -- hijack_unnamed_buffer_when_opening = true,
  actions = {
    change_dir = {
      enable = false,
    },
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
    enable = false,
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
  on_attach = function(bufnr)
    local api = require 'nvim-tree.api'
    local function opts(desc)
      return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
    end

    -- api.config.mappings.default_on_attach(bufnr)

    -- https://github.com/nvim-tree/nvim-tree.lua/blob/master/doc/nvim-tree-lua.txt#L2111
    vim.keymap.set('n', 'g?',   api.tree.toggle_help,               opts('Help'))
    vim.keymap.set('n', 'q',    api.tree.close,                     opts('Close'))
    vim.keymap.set('n', 'gr',   api.tree.reload,                    opts('Refresh'))

    vim.keymap.set('n', 'gh',   api.node.show_info_popup,           opts('Info'))

    vim.keymap.set('n', '<CR>', api.node.open.edit,                 opts('Open'))
    vim.keymap.set('n', 'o',    api.node.open.edit,                 opts('Open'))
    vim.keymap.set('n', 'O',    api.node.run.system,                opts('Open/Run with System'))

    vim.keymap.set('n', 'x',    api.fs.cut,                         opts('Cut'))
    vim.keymap.set('n', 'c',    api.fs.copy.node,                   opts('Copy'))
    vim.keymap.set('n', 'p',    api.fs.paste,                       opts('Paste'))

    vim.keymap.set('n', 'a',    api.fs.create,                      opts('Create File Or Directory'))
    vim.keymap.set('n', 'd',    api.fs.trash,                       opts('Trash'))

    vim.keymap.set('n', 'r',    api.fs.rename,                      opts('Rename'))
    vim.keymap.set('n', 'R',    api.fs.rename_basename,             opts('Rename: Basename'))

    vim.keymap.set('n', 's',    api.tree.search_node,               opts('Search'))
    vim.keymap.set('n', 'F',    api.live_filter.clear,              opts('Clean Filter'))
    vim.keymap.set('n', 'f',    api.live_filter.start,              opts('Filter'))

    vim.keymap.set('n', 'H',    api.tree.toggle_hidden_filter,      opts('Toggle Filter: Dotfiles'))
    vim.keymap.set('n', 'I',    api.tree.toggle_gitignore_filter,   opts('Toggle Filter: Git Ignore'))
    vim.keymap.set('n', 'J',    api.node.navigate.sibling.last,     opts('Last Sibling'))
    vim.keymap.set('n', 'K',    api.node.navigate.sibling.first,    opts('First Sibling'))

    vim.keymap.set('n', 'y',    api.fs.copy.filename,               opts('Copy Name'))
    vim.keymap.set('n', 'Y',    api.fs.copy.relative_path,          opts('Copy Relative Path'))

    vim.keymap.set('n', '[c',   api.node.navigate.git.prev,         opts('Prev Git'))
    vim.keymap.set('n', ']c',   api.node.navigate.git.next,         opts('Next Git'))
    vim.keymap.set('n', ']e',   api.node.navigate.diagnostics.next, opts('Next Diagnostic'))
    vim.keymap.set('n', '[e',   api.node.navigate.diagnostics.prev, opts('Prev Diagnostic'))
  end,
}
