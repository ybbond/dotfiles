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
    pattern = '*.c,*.cpp,*.cc,*.h,*.py',
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

vim.g.lightspeed_last_motion = ''
vim.api.nvim_create_augroup('LightspeedLastMotion', {})
vim.api.nvim_create_autocmd(
  'User',
  {
    group = 'LightspeedLastMotion',
    pattern = 'LightspeedSxEnter',
    callback = function () vim.g.lightspeed_last_motion = 'sx' end,
  }
)
vim.api.nvim_create_autocmd(
  'User',
  {
    group = 'LightspeedLastMotion',
    pattern = 'LightspeedFtEnter',
    callback = function () vim.g.lightspeed_last_motion = 'ft' end,
  }
)

vim.api.nvim_create_augroup('NumberToggle', {})
vim.api.nvim_create_autocmd(
  {'BufEnter', 'FocusGained', 'InsertLeave'},
  {
    group = 'NumberToggle',
    pattern = '*',
    callback = function()
      if vim.opt.number:get() == true then
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
      if vim.opt.number:get() == true then
        vim.opt.relativenumber = false
      end
    end,
  }
)
