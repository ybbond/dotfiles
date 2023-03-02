vim.api.nvim_create_autocmd(
  'TextYankPost',
  {
    callback = function()
      vim.highlight.on_yank({ timeout = 400 })
    end,
  }
)

vim.api.nvim_create_autocmd(
  {'BufNewFile', 'BufRead'},
  {
    pattern = '*.v',
    callback = function()
      vim.o.filetype = 'vlang'
    end,
  }
)

vim.api.nvim_create_autocmd(
  {'BufNewFile', 'BufRead'},
  {
    pattern = '*.c,*.cpp,*.cc,*.h,*.py,*.go',
    callback = function()
      vim.bo.tabstop = 4
      vim.bo.shiftwidth = 4
      vim.bo.expandtab = false
    end,
  }
)

vim.api.nvim_create_autocmd(
  {'BufNewFile', 'BufRead'},
  {
    pattern = '*.md,*.mmd,*.txt,*.markdown,*.multimarkdown',
    callback = function()
      vim.wo.wrap = true
    end
  }
)

vim.api.nvim_create_autocmd(
  'FileType',
  {
    pattern = 'qf',
    callback = function()
      vim.api.nvim_buf_set_keymap(0, 'n', 'q', '<CMD>cclose<CR>', {noremap = true})
    end,
  }
)

vim.api.nvim_create_user_command(
  'RelativeNumberToggle',
  function ()
    if vim.g.use_relative_number then
      vim.g.use_relative_number = false
      vim.o.relativenumber = false
    elseif not vim.g.use_relative_number then
      vim.g.use_relative_number = true
      vim.o.relativenumber = true
    end
  end,
  {}
)
vim.api.nvim_create_augroup('NumberToggle', {})
vim.api.nvim_create_autocmd(
  {'BufEnter', 'FocusGained', 'InsertLeave'},
  {
    group = 'NumberToggle',
    pattern = '*',
    callback = function()
      if vim.opt.number:get() == true and vim.g.use_relative_number then
        vim.opt.relativenumber = true
      end
    end,
  }
)
vim.api.nvim_create_autocmd(
  {'BufLeave', 'FocusLost', 'InsertEnter'},
  {
    group = 'NumberToggle',
    pattern = '*',
    callback = function()
      if vim.opt.number:get() == true and vim.g.use_relative_number then
        vim.opt.relativenumber = false
      end
    end,
  }
)
