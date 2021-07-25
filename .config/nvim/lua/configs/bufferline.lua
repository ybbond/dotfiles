require('bufferline').setup {
  options = {
    numbers = 'ordinal',
    number_style = '',
    close_command = 'BufDel %d',
    right_mouse_command = nil,
    middle_mouse_command = 'BufDel %d',
    diagnostics = 'nvim_lsp',
    max_name_length = 30,
  }
}
