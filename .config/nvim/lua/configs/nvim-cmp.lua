local luasnip = require("luasnip")
local cmp = require('cmp')

cmp.setup{
  -- reference: https://github.com/timbedard/dotfiles/blob/main/config/nvim/lua/plugins.lua
  min_length = 1,
  sources = {
    {name = 'nvim_lsp'},
    {name = 'nvim_lua'},
    {name = 'luasnip'},
    {name = 'calc'},
    {name = 'path'},
    {name = 'buffer'},
  },
  formatting = {
    format = function(entry, vim_item)
      vim_item.menu = ({
        nvim_lsp = "[LSP]",
        nvim_lua = "[Lua]",
        luasnip = "[LuaSnip]",
        calc = "[Calc]",
        path = "[Path]",
        buffer = "[Buffer]",
      })[entry.source.name]
      return vim_item
    end,
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  completion = {
    completeopt = 'menu,menuone,noinsert,noselect',
    --completeopt = 'menu,menuone,noinsert',
    keyword_length = 1,
  },
  mapping = {
    ['<C-p>'] = function(fallback) if cmp.visible() then cmp.select_prev_item() else fallback() end end,
    ['<C-n>'] = function(fallback) if cmp.visible() then cmp.select_next_item() else fallback() end end,
    ['<C-u>'] = function(fallback) if cmp.visible() then cmp.scroll_docs(-4) else fallback() end end,
    ['<C-d>'] = function(fallback) if cmp.visible() then cmp.scroll_docs(4) else fallback() end end,
    ['<C-y>'] = function(fallback) if cmp.visible() then cmp.scroll_docs(-4) else fallback() end end,
    ['<C-e>'] = function(fallback) if cmp.visible() then cmp.scroll_docs(4) else fallback() end end,
    ['<TAB>'] = function(fallback)
      if cmp.visible() then
        cmp.confirm({
          behavior = cmp.ConfirmBehavior.Insert,
          select = true,
        })
      else
        fallback()
      end
    end,
    ['<C-Space>'] = function(fallback) if cmp.visible() then cmp.complete() else fallback() end end,
    ['<C-g>'] = function(fallback) if cmp.visible() then cmp.close() else fallback() end end,
  },
  experimental = {
    native_menu = true,
  },
}
