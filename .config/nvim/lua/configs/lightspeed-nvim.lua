function Repeat_FT(reverse)
  local ls = require'lightspeed'
  ls.ft['instant-repeat?'] = true
  ls.ft:to(reverse, ls.ft['prev-t-like?'])
end

vim.api.nvim_set_keymap('n', ';', '<cmd>lua Repeat_FT(false)<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('x', ';', '<cmd>lua Repeat_FT(false)<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', ',', '<cmd>lua Repeat_FT(true)<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('x', ',', '<cmd>lua Repeat_FT(true)<cr>', {noremap = true, silent = true})
