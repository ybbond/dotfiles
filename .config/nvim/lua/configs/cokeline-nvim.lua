local colors = {
  black = '#3b4252',
  white = '#e5e9f0',
  red = '#bf616a',
  red_dark = '#700f18',
  green = '#a3be8c',
  green_dark = '#097d17',
  yellow = '#ebcb8b',
  yellow_dark = '#754e01',
  cyan = '#88c0d0',
  teal = '#034254',
  magenta = '#b48ead',
  orange = '#c9826b',
  bg = '#2e3440',
  fg = '#b9bfca',
  skyblue = '#8cafd2',
  blue_dark = '#092c4f',
  oceanblue = '#668aab',
  violet = '#9d7495',

  -- magenta = '#bc8cff',
  -- violet = '#9E93E8',
}

require('cokeline').setup({
  cycle_prev_next_mappings = true,

  default_hl = {
    focused = {
      fg = '#000000',
      bg = '#ffffff',
    },
    unfocused = {
      fg = '#ffffff',
      bg = '#3b4252',
    },
  },

  components = {
    {
      text = function(buffer) return ' ' .. buffer.devicon.icon end,
      hl = {
        fg = function(buffer) return buffer.devicon.color end,
      },
    },
    {
      text = function(buffer) return ' ' .. buffer.index .. ': ' end,
      hl = {
        fg = function(buffer)
          return buffer.is_focused
            and '#000000'
             or '#ffffff'
        end,
      },
    },
    {
      text = function(buffer) return buffer.unique_prefix end,
      hl = {
        fg = function(buffer)
          return buffer.is_focused
            and '#4c4c4c'
             or '#acb8ca'
        end,
        style = 'italic',
      },
    },
    {
      text = function(buffer) return buffer.filename end,
      hl = {
        fg = function(buffer)
          return buffer.is_focused
            and '#000000'
             or '#ffffff'
        end,
      },
    },
    {
      text = ' ',
      hl = {
        fg = function(buffer)
          return buffer.is_focused
            and '#000000'
             or '#ffffff'
        end,
      },
    },
    {
      text = function(buffer)
        if buffer.lsp.errors ~= 0 then
          -- return '[E:' .. buffer.lsp.errors .. ']'
          return buffer.lsp.errors .. ' '
        end
        return ''
      end,
      hl = {
        fg = function(buffer)
          return buffer.is_focused
            and colors.red_dark
             or colors.red
        end,
      },
    },
    {
      text = function(buffer)
        if buffer.lsp.warnings ~= 0 then
          -- return '[W:' .. buffer.lsp.warnings .. ']'
          return buffer.lsp.warnings .. ' '
        end
        return ''
      end,
      hl = {
        fg = function(buffer)
          return buffer.is_focused
            and colors.yellow_dark
             or colors.yellow
        end,
      },
    },
    {
      text = function(buffer)
        if buffer.lsp.hints ~= 0 then
          -- return '[H:' .. buffer.lsp.hints .. ']'
          return buffer.lsp.hints .. ' '
        end
        return ''
      end,
      hl = {
        fg = function(buffer)
          return buffer.is_focused
            and colors.blue_dark
             or colors.skyblue
        end,
      },
    },
    {
      text = function(buffer)
        if buffer.lsp.infos ~= 0 then
          -- return '[I:' .. buffer.lsp.infos .. ']'
          return buffer.lsp.infos .. ' '
        end
        return ''
      end,
      hl = {
        fg = function(buffer)
          return buffer.is_focused
            and colors.teal
             or colors.cyan
        end,
      },
    },
    {
      text = function(buffer)
        return buffer.is_modified
          and '●'
           or ''
      end,
      hl = {
        fg = function(buffer)
          local focused_color = buffer.is_modified
            and colors.green_dark
             or '#000000'
          local not_focused_color = buffer.is_modified
            and colors.green
             or '#ffffff'
          return buffer.is_focused
            and focused_color
             or not_focused_color
        end,
      },
      delete_buffer_on_left_click = function(buffer)
        return buffer.is_modified
          and false
           or true
      end,
    },
    {
      text = ' ',
      hl = {
        fg = function(buffer)
          return buffer.is_focused
            and '#000000'
             or '#ffffff'
        end,
      },
    },
  },
})
