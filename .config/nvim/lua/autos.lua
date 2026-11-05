-- https://github.com/nvim-telescope/telescope.nvim/issues/2027#issuecomment-1561836585
vim.api.nvim_create_autocmd("WinLeave", {
  callback = function()
    if vim.bo.ft == "TelescopePrompt" and vim.fn.mode() == "i" then
      vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "i", false)
    end
  end,
})

vim.api.nvim_create_autocmd(
  'TextYankPost',
  {
    callback = function()
      vim.hl.on_yank({ timeout = 400 })
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
    pattern = '*.puml',
    callback = function()
      vim.o.filetype = 'plantuml'
    end,
  }
)

vim.api.nvim_create_autocmd(
  {'BufNewFile', 'BufRead'},
  {
    pattern = '*.snap',
    callback = function()
      vim.o.filetype = 'snap'
    end,
  }
)

vim.api.nvim_create_autocmd(
  {'BufNewFile', 'BufRead'},
  {
    pattern = 'Fastfile',
    callback = function()
      vim.o.filetype = 'ruby'
    end,
  }
)

vim.api.nvim_create_autocmd(
  {'BufNewFile', 'BufRead'},
  {
    pattern = '.env*,.ocamlformat',
    callback = function()
      vim.o.filetype = 'config'
    end,
  }
)

vim.api.nvim_create_autocmd(
  {'BufNewFile', 'BufRead'},
  {
    pattern = '*.lock,*.pbxproj',
    callback = function()
      vim.o.filetype = 'lock'
    end,
  }
)

-- go's ftplugin sets noexpandtab/shiftwidth=0 (python's sets 4 spaces itself), so only the tab width is ours
vim.api.nvim_create_autocmd(
  'FileType',
  {
    pattern = 'go',
    callback = function()
      vim.bo.tabstop = 4
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

-- TODO(xxkkbb)
-- vim.api.nvim_create_autocmd(
--   {'InsertEnter', 'CmdlineEnter'},
--   {
--     callback = function()
--       if vim.bo.filetype ~= 'lisp' and vim.bo.filetype ~= 'commonlisp' and vim.bo.ft ~= "TelescopePrompt" then
--         vim.api.nvim_exec_autocmds('User', {
--           pattern = 'EnableAutoPair',
--         })
--         print 'test'
--       end
--     end
--   }
-- )

vim.api.nvim_create_user_command(
  'RelativeNumberToggle',
  function()
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

vim.api.nvim_create_autocmd(
  'ColorScheme',
  {
    callback = function()
      -- let the terminal paint the background in truecolor too, like cterm mode does
      local normal = vim.api.nvim_get_hl(0, { name = 'Normal' })
      normal.bg = nil
      vim.api.nvim_set_hl(0, 'Normal', normal)

      -- diagnostics as a tinted background instead of an underline, same palette as built-in DiffAdd/DiffText;
      -- truecolor keeps the syntax fg, cterm forces fg because its backgrounds are saturated
      local dark = vim.o.background == 'dark'
      for severity, c in pairs({
        Error = { 'Red', 9, 1 },
        Warn = { 'Yellow', 11, 3 },
        Info = { 'Cyan', 14, 6 },
        Hint = { 'Blue', 12, 4 },
        Ok = { 'Green', 10, 2 },
      }) do
        vim.api.nvim_set_hl(0, 'DiagnosticUnderline' .. severity, {
          bg = (dark and 'NvimDark' or 'NvimLight') .. c[1],
          ctermbg = dark and c[2] or c[3],
          ctermfg = dark and 0 or 15,
        })
      end
    end,
  }
)

-- hide the startup [No Name] / `nvim .` directory buffer from buffer lists (Telescope buffers, :ls)
vim.api.nvim_create_autocmd(
  'VimEnter',
  {
    callback = function(data)
      local name = vim.api.nvim_buf_get_name(data.buf)
      local is_dir = vim.fn.isdirectory(name) == 1
      if is_dir or (name == '' and not vim.bo[data.buf].modified) then
        vim.bo[data.buf].buflisted = false
      end
      -- wipe the directory buffer once left, which also drops it from the jumplist
      -- ([No Name] needs no wipe: the first opened file reuses it)
      if is_dir then
        vim.bo[data.buf].bufhidden = 'wipe'
      end
    end,
  }
)

-- after the autocmd above so it applies on first load; also sets g:colors_name
-- so a 'background' change re-fires ColorScheme (lualine/scrollbar refresh)
vim.cmd.colorscheme('default')
