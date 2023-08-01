local cmp = require('cmp')

-- local has_words_before = function()
--   local line, col = unpack(vim.api.nvim_win_get_cursor(0))
--   return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
-- end


cmp.setup{
  -- reference: https://github.com/timbedard/dotfiles/blob/main/config/nvim/lua/plugins.lua
  min_length = 1,
  sources = {
    {name = 'nvim_lsp'},
    {name = 'nvim_lua'},
    {name = 'vsnip'},
    {name = 'path'},
    {name = 'buffer'},
  },

  formatting = {
    format = function(entry, vim_item)
      vim_item.menu = ({
        nvim_lsp = "[LSP]",
        nvim_lua = "[Lua]",
        vsnip = "[vsnip]",
        path = "[Path]",
        buffer = "[Buffer]",
      })[entry.source.name]
      return vim_item
    end,
  },

  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  completion = {
    -- completeopt = 'menu,menuone,noinsert,noselect',
    completeopt = 'menu,menuone,noinsert',
    -- autocomplete = false,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item({behavior = cmp.SelectBehavior.Insert}),
    ['<C-n>'] = cmp.mapping.select_next_item({behavior = cmp.SelectBehavior.Insert}),
    ['<Up>'] = cmp.mapping.select_prev_item({behavior = cmp.SelectBehavior.Select}),
    ['<Down>'] = cmp.mapping.select_next_item({behavior = cmp.SelectBehavior.Select}),
    ['<C-k>'] = cmp.mapping.scroll_docs(-4),
    ['<C-j>'] = cmp.mapping.scroll_docs(4),
    -- ['<C-y>'] = cmp.mapping.scroll_docs(-4),
    -- ['<C-e>'] = cmp.mapping.scroll_docs(4),
    -- ['<C-y>'] = cmp.config.disable,
    ['<C-y>'] = function (_)
      if cmp.visible() then
        cmp.abort()
        cmp.close()
      end
      vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<C-y>', true, true, true), 'n', true)
    end,
    ['<C-e>'] = cmp.config.disable,

    -- ['<Tab>'] = cmp.mapping(cmp.mapping.confirm({behavior = cmp.ConfirmBehavior.Replace, select = false}), { 'i', 'c' }),
    -- ['<S-Tab>'] = cmp.mapping(cmp.mapping.confirm({behavior = cmp.ConfirmBehavior.Insert, select = false}), { 'i', 'c' }),
    ['<Tab>'] = cmp.mapping(cmp.mapping.confirm({behavior = cmp.ConfirmBehavior.Replace, select = false}), {'i'}),
    ['<S-Tab>'] = cmp.mapping(cmp.mapping.confirm({behavior = cmp.ConfirmBehavior.Insert, select = false}), {'i'}),

    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-g>'] = cmp.mapping.close(),

    ['<CR>'] = function (_)
      if cmp.visible() then
        cmp.abort()
        cmp.close()
      end
      vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<CR>', true, true, true), 'n', true)
      -- vim.api.nvim_feedkeys('<CR>', 'n', true)
    end,
  },
  view = {
    entries = "native",
  },
}
