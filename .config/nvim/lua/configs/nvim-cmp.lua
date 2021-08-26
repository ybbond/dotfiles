local cmp = require('cmp')

cmp.setup {
  -- reference: https://github.com/timbedard/dotfiles/blob/main/config/nvim/lua/plugins.lua
  min_length = 0, -- allow for `from package import _` in Python

  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-u>'] = cmp.mapping.scroll_docs(-4),
    ['<C-d>'] = cmp.mapping.scroll_docs(4),
    ['<C-y>'] = cmp.mapping.scroll_docs(-4),
    ['<C-e>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-g>'] = cmp.mapping.close(),
    ['<TAB>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Insert,
      select = true,
    })
  },

  sources = {
    {name = 'nvim_lsp'},
    {name = 'nvim_lua'},
    {name = 'calc'},
    {name = 'path'},
    {name = 'buffer'},
  },
}
