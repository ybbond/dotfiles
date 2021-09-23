local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
  local col = vim.fn.col '.' - 1
  return col == 0 or vim.fn.getline('.'):sub(col, col):match '%s' ~= nil
end

local luasnip = require("luasnip")

local cmp = require('cmp')

cmp.setup {
  -- reference: https://github.com/timbedard/dotfiles/blob/main/config/nvim/lua/plugins.lua
  min_length = 1, -- allow for `from package import _` in Python

  snippet = {
    -- expand = nil,
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },

  completion = {
    completeopt = 'menu,menuone,noinsert,noselect',
    keyword_length = 1,
  },

  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    -- ['<C-u>'] = cmp.mapping.scroll_docs(-4),
    -- ['<C-d>'] = cmp.mapping.scroll_docs(4),
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    -- ['<C-y>'] = cmp.mapping.scroll_docs(-4),
    -- ['<C-e>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-g>'] = cmp.mapping.close(),
    ['<TAB>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Insert,
      select = true,
    }),

    -- -- supertab-like mapping
    -- ["<C-Tab>"] = cmp.mapping(function(fallback)
    --   if vim.fn.pumvisible() == 1 then
    --     vim.fn.feedkeys(t("<C-n>"), "n")
    --   elseif luasnip.expand_or_jumpable() then
    --     vim.fn.feedkeys(t("<Plug>luasnip-expand-or-jump"), "")
    --   elseif check_back_space() then
    --     vim.fn.feedkeys(t("<Tab>"), "n")
    --   else
    --     fallback()
    --   end
    -- end, {
    --   "i",
    --   "s",
    -- }),
    -- ["<C-S-Tab>"] = cmp.mapping(function(fallback)
    --   if vim.fn.pumvisible() == 1 then
    --     vim.fn.feedkeys(t("<C-p>"), "n")
    --   elseif luasnip.jumpable(-1) then
    --     vim.fn.feedkeys(t("<Plug>luasnip-jump-prev"), "")
    --   else
    --     fallback()
    --   end
    -- end, {
    --   "i",
    --   "s",
    -- }),
  },

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
      -- fancy icons and a name of kind
      -- vim_item.kind = require("lspkind").presets.default[vim_item.kind]
      --   .. " "
      --   .. vim_item.kind
      -- set a name for each source
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

  -- experimental = {
  --   ghost_text = true,
  -- },
}
