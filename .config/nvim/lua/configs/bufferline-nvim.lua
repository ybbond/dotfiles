require("bufferline").setup{
  options = {
    numbers = 'ordinal',
    number_style = "",
    diagnostics = "nvim_lsp",
    close_command = function(bufnum)
      require('bufdelete').bufdelete(bufnum, true)
    end,
    right_mouse_command =  "buffer %d",
    middle_mouse_command = function(bufnum)
      require('bufdelete').bufdelete(bufnum, true)
    end,
    max_name_length = 40,
    custom_filter = function(buf_number)
      -- filter out by buffer name
      if vim.fn.bufname(buf_number) ~= "__FLUTTER_DEV_LOG__" then
        return true
      end
    end,
  }
}
