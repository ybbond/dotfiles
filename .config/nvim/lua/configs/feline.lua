local colors = {
  black = '#3b4252',
  white = '#e5e9f0',
  red = '#bf616a',
  green = '#a3be8c',
  yellow = '#ebcb8b',
  cyan = '#88c0d0',
  magenta = '#b48ead',
  orange = '#c9826b',
  bg = '#2e3440',
  fg = '#b9bfca',
  skyblue = '#8cafd2',
  oceanblue = '#668aab',
  violet = '#9d7495',

  -- magenta = '#bc8cff',
  -- violet = '#9E93E8',
}

local components = require('feline.presets')['noicon']

local diag_severity = vim.diagnostic.severity

local function diagnostics(severity)
  local count = require'feline.providers.lsp'.get_diagnostics_count(severity)
  return count ~= 0 and tostring(count) or ''
end

local function workspace_diag(severity)
  local count = vim.diagnostic.get(nil, {severity = severity})
  return vim.tbl_count(count)
end

components.active[1] = {
  {
    provider = '▊ ',
    hl = {
      fg = colors.cyan
    }
  },

  {
    provider = 'vi_mode',
    hl = function()
      return {
        name = require('feline.providers.vi_mode').get_mode_highlight_name(),
        fg = require('feline.providers.vi_mode').get_mode_color(),
        style = 'bold'
      }
    end,
    right_sep = ' ',
    icon = ''
  },

  {
    provider = 'file_size',
    left_sep = {
      str = ' ',
      hl = {
        fg = 'NONE',
        bg = colors.cyan,
      },
    },
    hl = {
      fg = colors.bg,
      bg = colors.cyan,
    },
    enabled = function() return vim.fn.getfsize(vim.fn.expand('%:p')) > 0 end,
  },

  {
    provider = 'position',
    left_sep = {
      str = ' ',
      hl = {
        fg = 'NONE',
        bg = colors.cyan,
      },
    },
    hl = {
      fg = colors.bg,
      bg = colors.cyan,
    },
  },

  {
    provider = 'lsp_client_names',
    -- hl = { fg = 'black', bg =  colors.cyan},
    hl = { fg = '#ffffff', bg = 'NONE' },
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
  },

  {
    provider = 'f(',
    hl = {
      fg = colors.white,
      bg = colors.bg,
    },
    left_sep = '',
    right_sep = '',
    enabled = function()
      return diagnostics(diag_severity.ERROR) ~= ''
          or diagnostics(diag_severity.WARN) ~= ''
          or diagnostics(diag_severity.INFO) ~= ''
          or diagnostics(diag_severity.HINT) ~= ''
    end,
  },

  {
    provider = function() return diagnostics(diag_severity.ERROR) .. '' end,
    hl = { fg = colors.red, bg = 'NONE' },
    icon = '',
    enabled = function() return diagnostics(diag_severity.ERROR) ~= '' end,
  },
  {
    provider = function() return diagnostics(diag_severity.WARN) .. '' end,
    hl = { fg = colors.yellow, bg = 'NONE' },
    icon = '',
    enabled = function() return diagnostics(diag_severity.WARN) ~= '' end,
  },
  {
    provider = function() return diagnostics(diag_severity.HINT) .. '' end,
    hl = { fg = colors.skyblue, bg = 'NONE' },
    icon = '',
    enabled = function() return diagnostics(diag_severity.HINT) ~= '' end,
  },
  {
    provider = function() return diagnostics(diag_severity.INFO) .. '' end,
    hl = { fg = colors.cyan, bg = 'NONE' },
    icon = '',
    enabled = function() return diagnostics(diag_severity.INFO) ~= '' end,
  },

  {
    provider = ')',
    hl = {
      fg = colors.white,
      bg = colors.bg,
    },
    left_sep = '',
    right_sep = ' ',
    enabled = function()
      return diagnostics(diag_severity.ERROR) ~= ''
          or diagnostics(diag_severity.WARN) ~= ''
          or diagnostics(diag_severity.INFO) ~= ''
          or diagnostics(diag_severity.HINT) ~= ''
    end,
  },

  {
    provider = 'w(',
    hl = {
      fg = colors.white,
      bg = colors.bg,
    },
    left_sep = '',
    right_sep = '',
    enabled = function()
      local ws_errors = workspace_diag(diag_severity.ERROR)
      local ws_warnings = workspace_diag(diag_severity.WARN)
      local ws_hints = workspace_diag(diag_severity.HINT)
      local ws_infos = workspace_diag(diag_severity.INFO)
      return ws_errors ~= 0
          or ws_warnings ~= 0
          or ws_hints ~= 0
          or ws_infos ~= 0
    end,
  },

  {
    provider = function()
      local ws_errors = workspace_diag(diag_severity.ERROR)
      return ws_errors ~= 0
        and ''..ws_errors..''
         or ''
    end,
    hl = { fg = colors.red, bg = 'NONE' },
  },
  {
    provider = function()
      local ws_warnings = workspace_diag(diag_severity.WARN)
      return ws_warnings ~= 0
        and ''..ws_warnings..''
         or ''
    end,
    hl = { fg = colors.yellow, bg = 'NONE' },
  },
  {
    provider = function()
      local ws_hints = workspace_diag(diag_severity.HINT)
      return ws_hints ~= 0
        and ''..ws_hints..''
         or ''
    end,
    hl = { fg = colors.skyblue, bg = 'NONE' },
  },
  {
    provider = function()
      local ws_infos = workspace_diag(diag_severity.INFO)
      return ws_infos ~= 0
        and ''..ws_infos..''
         or ''
    end,
    hl = { fg = colors.cyan, bg = 'NONE' },
  },

  {
    provider = ')',
    hl = {
      fg = colors.white,
      bg = colors.bg,
    },
    left_sep = '',
    right_sep = '',
    enabled = function()
      local ws_errors = workspace_diag(diag_severity.ERROR)
      local ws_warnings = workspace_diag(diag_severity.WARN)
      local ws_hints = workspace_diag(diag_severity.HINT)
      local ws_infos = workspace_diag(diag_severity.INFO)
      return ws_errors ~= 0
          or ws_warnings ~= 0
          or ws_hints ~= 0
          or ws_infos ~= 0
    end,
  },

  {
    provider = '',
    hl = { fg = 'NONE', bg = 'NONE' },
    icon = '',
  },
}

components.active[2] = {
  {
    provider = 'git_diff_added',
    hl = {
      fg = colors.green,
      bg = colors.bg,
    },
    icon = ' +'
  },
  {
    provider = 'git_diff_changed',
    hl = {
      fg = colors.yellow,
      bg = colors.bg,
    },
    icon = ' ~'
  },
  {
    provider = 'git_diff_removed',
    hl = {
      fg = colors.red,
      bg = colors.bg,
    },
    right_sep = {
      str = ' ',
      hl = {
        fg = 'NONE',
        bg = colors.bg,
      }
    },
    icon = ' -'
  },

  {
    provider = 'git_branch',
    hl = {
      fg = colors.white,
      bg = colors.bg,
      style = 'bold'
    },
    right_sep = {hl = {fg = 'NONE', bg = colors.bg}, str = ' '},
    icon = ' '
  },

  {
    provider = ' :D ',
    hl = {
      fg = 'bg',
      bg = colors.cyan,
      style = 'bold',
    },
  },
}


components.inactive[1] = {
  provider = 'file_info',
  hl = {
    fg = colors.bg,
    bg = colors.cyan,
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
      fg = colors.bg,
      bg = colors.cyan,
      style = 'bold',
    },
    left_sep = {
      str = ' ',
      hl = {
        fg = 'NONE',
        bg = colors.cyan,
      },
    },
    right_sep = {
      {
        str = ' ',
        hl = {
          fg = 'NONE',
          bg = colors.cyan,
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
