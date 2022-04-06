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

function _G.lightspeed_repeat_semicolon()
  return vim.g.lightspeed_last_motion == 'sx'
     and replaceTermcodes'<Plug>Lightspeed_;_sx'
      or replaceTermcodes'<Plug>Lightspeed_;_ft'
end
map ';' [[v:lua.lightspeed_repeat_semicolon()]] ({expr = true, noremap = false})
function _G.lightspeed_repeat_comma()
  return vim.g.lightspeed_last_motion == 'sx'
     and replaceTermcodes'<Plug>Lightspeed_,_sx'
      or replaceTermcodes'<Plug>Lightspeed_,_ft'
end
map ',' [[v:lua.lightspeed_repeat_comma()]] ({expr = true, noremap = false})


require'lightspeed'.setup({
  jump_to_unique_chars = false,
  limit_ft_matches = 50,
  exit_after_idle_msecs = {
    unlabeled = 1000,
    labeled = 1000,
  },
  safe_labels = {
    "s", "f", "n", "u", "t", "F", "L",
    "N", "H", "G", "M", "U", "T", "Z",
  },
  labels = {
    "s", "f", "n", "j", "k", "l", "o",
    "d", "w", "e", "h", "m", "v", "g",
    "u", "t", "c", "z", "F", "L", "N",
    "H", "G", "M", "U", "T", "?", "Z",
  },
})
