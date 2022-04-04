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
  buffers = {
    filter_valid = function(buffer)
      -- if buffer.type == 'quickfix' then
      --   return false
      if buffer.filetype == '' then
        return false
      -- elseif buffer.path == '.' then
      --   return false
      end
      return true
    end,
    focus_on_delete = 'prev',
    new_buffers_position = 'next',
  },

  default_hl = {
    fg = function (buffer)
      return
        buffer.is_focused
        and '#000000'
         or '#ffffff'
    end,
    bg = function (buffer)
      return
        buffer.is_focused
        and '#ffffff'
         or '#000000'
    end,
  },

  components = {
    {
      text = function(buffer) return ' ' .. buffer.devicon.icon end,
      fg = function(buffer) return buffer.devicon.color end,
    },
    {
      text = function(buffer) return ' ' .. buffer.index .. ': ' end,
      fg = function(buffer)
        return buffer.is_focused
          and '#000000'
           or '#ffffff'
      end,
    },
    {
      text = function(buffer) return buffer.unique_prefix end,
      fg = function(buffer)
        return buffer.is_focused
          and '#4c4c4c'
           or '#acb8ca'
      end,
      style = 'italic',
    },
    {
      text = function(buffer) return buffer.filename end,
      fg = function(buffer)
        return buffer.is_focused
          and '#000000'
           or '#ffffff'
      end,
    },
    {
      text = ' ',
      fg = function(buffer)
        return buffer.is_focused
          and '#000000'
           or '#ffffff'
      end,
    },
    {
      text = function(buffer)
        if buffer.diagnostics.errors ~= 0 then
          -- return '[E:' .. buffer.diagnostics.errors .. ']'
          return buffer.diagnostics.errors .. ' '
        end
        return ''
      end,
      fg = function(buffer)
        return buffer.is_focused
          and colors.red_dark
           or colors.red
      end,
    },
    {
      text = function(buffer)
        if buffer.diagnostics.warnings ~= 0 then
          -- return '[W:' .. buffer.diagnostics.warnings .. ']'
          return buffer.diagnostics.warnings .. ' '
        end
        return ''
      end,
      fg = function(buffer)
        return buffer.is_focused
          and colors.yellow_dark
           or colors.yellow
      end,
    },
    {
      text = function(buffer)
        if buffer.diagnostics.hints ~= 0 then
          -- return '[H:' .. buffer.diagnostics.hints .. ']'
          return buffer.diagnostics.hints .. ' '
        end
        return ''
      end,
      fg = function(buffer)
        return buffer.is_focused
          and colors.blue_dark
           or colors.skyblue
      end,
    },
    {
      text = function(buffer)
        if buffer.diagnostics.infos ~= 0 then
          -- return '[I:' .. buffer.diagnostics.infos .. ']'
          return buffer.diagnostics.infos .. ' '
        end
        return ''
      end,
      fg = function(buffer)
        return buffer.is_focused
          and colors.teal
           or colors.cyan
      end,
    },
    {
      text = function(buffer)
        return buffer.is_modified
          and '●'
           or ''
      end,
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
      delete_buffer_on_left_click = function(buffer)
        return buffer.is_modified
          and false
           or true
      end,
    },
    {
      text = ' ',
      fg = function(buffer)
        return buffer.is_focused
          and '#000000'
           or '#ffffff'
      end,
    },
  },
})
