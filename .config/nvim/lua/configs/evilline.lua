local gl = require('galaxyline')
local colors = require('galaxyline.theme').default
local condition = require('galaxyline.condition')
local gls = gl.section
gl.short_line_list = {'NvimTree','vista','dbui','packer'}

gls.left[1] = {
  BlockSeparator = {
    provider = function() return '▊ ' end,
    highlight = {'#62B3B2','#2e3440', 'bold'} -- previously, #2e3440 was #343D46
  },
}

gls.left[2] = {
  ViMode = {
    provider = function()
      local mode_color = {n = colors.red, i = colors.green,v=colors.blue,
                          [''] = colors.blue,V=colors.blue,
                          c = colors.magenta,no = colors.red,s = colors.orange,
                          S=colors.orange,[''] = colors.orange,
                          ic = colors.yellow,R = colors.violet,Rv = colors.violet,
                          cv = colors.red,ce=colors.red, r = colors.cyan,
                          rm = colors.cyan, ['r?'] = colors.cyan,
                          ['!']  = colors.red,t = colors.red}
      vim.api.nvim_command('hi GalaxyViMode guifg='..mode_color[vim.fn.mode()])
      local alias = {n = 'NORMAL',i = 'INSERT',v= 'VISUAL',
                     [''] = 'VISUAL BLOCK',V= 'VISUAL LINE',
                     c= 'COMMAND',no= 'O-PENDING CHAR',s= 'SELECT',
                     S= 'SELECT LINE',['']='SELECT BLOCK',
                     ic= 'ICOMPLETION',R= 'REPLACE',Rv= 'VIRTUAL REPLACE',
                     cv= 'EX-MODE',ce= 'NORMAL EX-MODE',r= 'HIT-ENTER PROMPT',
                     rm= '-- MORE --',['r?']= ':CONFIRM QUERY',
                     ['!']= 'SHELL',t = 'TERMINAL'}
      return alias[vim.fn.mode()]
    end,
    icon = function() return vim.fn.mode().. ' ' end,
    separator = '  ',
    highlight = {colors.darkblue,colors.purple,'bold'},
  },
  -- ViMode = {
  --   provider = function()
  --     -- auto change color according the vim mode
  --     local mode_color = {n = colors.red, i = colors.green,v=colors.blue,
  --                         [''] = colors.blue,V=colors.blue,
  --                         c = colors.magenta,no = colors.red,s = colors.orange,
  --                         S=colors.orange,[''] = colors.orange,
  --                         ic = colors.yellow,R = colors.violet,Rv = colors.violet,
  --                         cv = colors.red,ce=colors.red, r = colors.cyan,
  --                         rm = colors.cyan, ['r?'] = colors.cyan,
  --                         ['!']  = colors.red,t = colors.red}
  --     vim.api.nvim_command('hi GalaxyViMode guifg='..mode_color[vim.fn.mode()])
  --     return '  '
  --   end,
  --   highlight = {colors.red,'#343D46','bold'},
  -- },
}

gls.left[3] = {
  FileSize = {
    provider = 'FileSize',
    condition = condition.buffer_not_empty,
    highlight = {colors.fg,'#2e3440'}
  }
}
gls.left[4] ={
  FileIcon = {
    provider = 'FileIcon',
    condition = condition.buffer_not_empty,
    highlight = {require('galaxyline.provider_fileinfo').get_file_icon_color,'#2e3440'},
  },
}

gls.left[5] = {
  FileName = {
    provider = 'FileName',
    condition = condition.buffer_not_empty,
    highlight = {colors.magenta,'#2e3440','bold'}
  }
}

gls.left[6] = {
  LineInfo = {
    provider = 'LineColumn',
    separator = ' ',
    separator_highlight = {'NONE','#2e3440'},
    highlight = {colors.fg,'#2e3440'},
  },
}

-- gls.left[7] = {
--   PerCent = {
--     provider = 'LinePercent',
--     separator = ' ',
--     separator_highlight = {'NONE','#343D46'},
--     highlight = {colors.fg,'#343D46','bold'},
--   }
-- }

gls.left[7] = {
  PlainSeparator = {
    provider = function() return ' ' end,
    highlight = {'NONE','#2e3440'}
  },
  ShowLspClient = {
    provider = 'GetLspClient',
    condition = function ()
      local tbl = {['dashboard'] = true,['']=true}
      if tbl[vim.bo.filetype] then
        return false
      end
      return true
    end,
    highlight = {colors.cyan,'#2e3440','bold'}
  },
}

gls.left[8] = {
  DiagnosticError = {
    provider = 'DiagnosticError',
    -- icon = '  ',
    icon = '  ',
    highlight = {colors.red,'#2e3440'}
  }
}
gls.left[9] = {
  DiagnosticWarn = {
    provider = 'DiagnosticWarn',
    -- icon = '  ',
    icon = '  ',
    highlight = {colors.yellow,'#2e3440'},
  }
}

gls.left[10] = {
  DiagnosticHint = {
    provider = 'DiagnosticHint',
    -- icon = '  ',
    icon = '  ',
    highlight = {colors.cyan,'#2e3440'},
  }
}

gls.left[11] = {
  DiagnosticInfo = {
    provider = 'DiagnosticInfo',
    -- icon = '  ',
    icon = '  ',
    highlight = {colors.blue,'#2e3440'},
  }
}

-- gls.right[1] = {
--   FileEncode = {
--     provider = 'FileEncode',
--     condition = condition.hide_in_width,
--     separator = ' ',
--     separator_highlight = {'NONE','#343D46'},
--     highlight = {colors.green,'#343D46','bold'}
--   }
-- }

-- gls.right[2] = {
--   FileFormat = {
--     provider = 'FileFormat',
--     condition = condition.hide_in_width,
--     separator = ' ',
--     separator_highlight = {'NONE','#343D46'},
--     highlight = {colors.green,'#343D46','bold'}
--   }
-- }

-- gls.right[3] = {
--   GitBranch = {
--     separator = ' ',
--     separator_highlight = {'NONE','#343D46'},
--     provider = 'GitBranch',
--     icon = '  ',
--     condition = condition.check_git_workspace,
--     highlight = {colors.violet,'#343D46','bold'},
--   },
--   PlainSeparator = {
--     provider = function() return ' ' end,
--     highlight = {'NONE','#343D46'}
--   },
-- }

gls.right[1] = {
  DiffAdd = {
    provider = 'DiffAdd',
    condition = condition.hide_in_width,
    icon = '  ',
    highlight = {colors.green,'#2e3440'},
  }
}
gls.right[2] = {
  DiffModified = {
    provider = 'DiffModified',
    condition = condition.hide_in_width,
    icon = ' 柳',
    highlight = {colors.blue,'#2e3440'},
  }
}
gls.right[3] = {
  DiffRemove = {
    provider = 'DiffRemove',
    condition = condition.hide_in_width,
    icon = '  ',
    highlight = {colors.red,'#2e3440'},
  }
}

gls.right[4] = {
  PlainSeparator = {
    provider = function() return ' ' end,
    highlight = {'NONE','#2e3440'}
  },
}

gls.right[5] = {
  PlainSeparator1 = {
    provider = function() return ' ' end,
    highlight = {'NONE','#2e3440'}
  },
  BufferType = {
    provider = 'FileTypeName',
    separator = ' ',
    separator_highlight = {'NONE','#2e3440'},
    highlight = {colors.blue,'#2e3440','bold'}
  },
  PlainSeparator2 = {
    provider = function() return ' ' end,
    highlight = {'NONE','#2e3440'}
  },
}

gls.right[6] = {
  Smiley = {
    separator = ' ',
    separator_highlight = {'#62B3B2','#62B3B2'},
    provider = function() return ':D ' end,
    highlight = {'#000000','#62B3B2'}
  },
}

gls.short_line_left[1] = {
  BufferType = {
    provider = 'FileTypeName',
    separator = ' ',
    separator_highlight = {'NONE','#2e3440'},
    highlight = {colors.blue,'#2e3440','bold'}
  }
}

gls.short_line_left[2] = {
  SFileName = {
    provider =  'SFileName',
    condition = condition.buffer_not_empty,
    highlight = {colors.fg,'#2e3440','bold'}
  }
}

gls.short_line_right[1] = {
  BufferIcon = {
    provider= 'BufferIcon',
    highlight = {colors.fg,'#2e3440'}
  }
}

