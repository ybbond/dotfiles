local colors = {
  bg = '#2e3440',
  -- black = '#3b4252',
  black = '#000000',
  skyblue = '#50B0F0',
  cyan = '#88c0d0',
  fg = '#b9bfca',
  green = '#a3be8c',
  oceanblue = '#536c9e',
  magenta = '#b48ead',
  orange = '#c9826b',
  red = '#bf616a',
  violet = '#9E93E8',
  -- white = '#e5e9f0',
  white = '#ffffff',
  yellow = '#ebcb8b',
}

local components = require('feline.presets')['noicon']

components.active[1][1] = {
  provider = '▊ ',
  hl = {
    fg = '#62B3B2'
  }
}

components.active[1][3] = {
  provider = 'file_info',
  hl = {
    fg = 'black',
    bg = '#62B3B2',
  },
  left_sep = '',
  right_sep = ' ',
  icon = ''
}

components.active[1][4] = {
  provider = 'file_size',
  left_sep = ' ',
  right_sep = ' ',
  enabled = function() return vim.fn.getfsize(vim.fn.expand('%:p')) > 0 end,
}

components.active[1][5] = {
  provider = 'position',
  left_sep = {
    str = ' ',
    -- hl = {
      -- fg = 'NONE',
      -- bg = '#62B3B2',
    -- },
  },
  right_sep = {
    str = ' ',
    -- hl = {
      -- fg = 'NONE',
      -- bg = '#62B3B2',
    -- },
  },
  hl = {
    fg = 'black',
    bg = '#62B3B2',
  },
}

table.insert(components.active[1], {
  provider = 'lsp_client_names',
  -- hl = { fg = 'black', bg = '#62B3B2' },
  hl = { fg = 'white', bg = 'NONE' },
  icon = '',
  left_sep = function()
    local val = {}
    if require('feline.providers.lsp').is_lsp_attached() then val.str = ' ' else val.str = '' end
    return val
  end,
  right_sep = function()
    local val = {}
    if require('feline.providers.lsp').is_lsp_attached() then val.str = ' ' else val.str = '' end
    return val
  end,
})

table.insert(components.active[1], {
  provider = '',
  hl = { fg = 'NONE', bg = 'NONE' },
  icon = '',
})


-- local current_treesitter_context = function()
--   if not packer_plugins["nvim-treesitter"] or packer_plugins["nvim-treesitter"].loaded == false then
--     return " "
--   end
--   local f = require'nvim-treesitter'.statusline({
--     indicator_size = 300,
--     type_patterns = {"class", "function", "method", "interface", "type_spec", "table", "if_statement", "for_statement", "for_in_statement"}
--   })
--   local fun_name = string.format("%s", f) -- convert to string, it may be a empty ts node
--   if fun_name == "vim.NIL" then
--     return " "
--   end
--   return " " .. fun_name
-- end

components.active[2][1] = {
  provider = function()
		return require("nvim-gps").get_location()
	end,
	enabled = function()
		return require("nvim-gps").is_available()
  end,
  -- provider = function() return current_treesitter_context() end,
  right_sep = ' ',
}

components.active[2][5] = {
  provider = 'git_branch',
  hl = {
    fg = 'white',
    bg = 'black',
    style = 'bold'
  },
  right_sep = {hl = {fg = 'NONE', bg = 'black'}, str = ' '},
  icon = ' '
}

components.active[2][6] = {
  provider = ' :D ',
  hl = {
    fg = 'bg',
    bg = '#62B3B2',
    style = 'bold',
  },
}


components.inactive[1][1] = {
  provider = 'file_info',
  hl = {
    fg = 'black',
    bg = '#62B3B2',
    style = 'bold',
  },
  left_sep = '',
  right_sep = ' ',
  icon = '',
}


components.inactive[2] = {
  {
    provider = 'file_type',
    hl = {
      fg = 'black',
      bg = '#62B3B2',
      style = 'bold',
    },
    left_sep = {
      str = ' ',
      hl = {
        fg = 'NONE',
        bg = '#62B3B2',
      },
    },
    right_sep = {
      {
        str = ' ',
        hl = {
          fg = 'NONE',
          bg = '#62B3B2',
        },
      },
      ' ',
    },
  },
}


require('feline').setup({
  preset = 'noicon',
  colors = colors,
  components = components,
})
