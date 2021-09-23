require("bufferline").setup{
  options = {
    numbers = function(opts) return string.format('[%s]', opts.ordinal) end,
    diagnostics = "nvim_lsp",
    close_command = function(bufnum)
      require('bufdelete').bufdelete(bufnum, true)
    end,
    right_mouse_command =  "buffer %d",
    middle_mouse_command = function(bufnum)
      require('bufdelete').bufdelete(bufnum, true)
    end,
    max_name_length = 60,
    max_prefix_length = 25,
    tab_size = 5,
    name_formatter = function(buf)  -- buf contains a "name", "path" and "bufnr"
      -- remove extension from markdown files for example
      if buf.name:match('__FLUTTER_DEV_LOG__') then
        return '-log-'
      end
    end,
    -- custom_filter = function(buf_number)
    --   -- filter out by buffer name
    --   if vim.fn.bufname(buf_number) ~= "__FLUTTER_DEV_LOG__" then
    --     return true
    --   end
    -- end,
    custom_areas = {
      -- right = function()
      --   local result = {}
      --   local error = vim.lsp.diagnostic.get_count(0, [[Error]])
      --   local warning = vim.lsp.diagnostic.get_count(0, [[Warning]])
      --   local info = vim.lsp.diagnostic.get_count(0, [[Information]])
      --   local hint = vim.lsp.diagnostic.get_count(0, [[Hint]])

      --   if error ~= 0 then
      --     table.insert(result, {text = "  " .. error, guifg = "#EC5241"})
      --   end

      --   if warning ~= 0 then
      --     table.insert(result, {text = "  " .. warning, guifg = "#EFB839"})
      --   end

      --   if hint ~= 0 then
      --     table.insert(result, {text = "  " .. hint, guifg = "#A3BA5E"})
      --   end

      --   if info ~= 0 then
      --     table.insert(result, {text = "  " .. info, guifg = "#7EA9A7"})
      --   end
      --   return result
      -- end,
      right = function()
        local result = {}
        local ws_errors = require('lsp_extensions.workspace.diagnostic').get_count(0, 'Error')
        local ws_warnings = require('lsp_extensions.workspace.diagnostic').get_count(0, 'Warning')
        local ws_infos = require('lsp_extensions.workspace.diagnostic').get_count(0, 'Information')
        local ws_hints = require('lsp_extensions.workspace.diagnostic').get_count(0, 'Hint')

        if ws_errors ~= 0 then
          table.insert(result, {text = " E " .. ws_errors, guifg = "#bf616a"})
        end

        if ws_warnings ~= 0 then
          table.insert(result, {text = " W " .. ws_warnings, guifg = "#ebcb8b"})
        end

        if ws_hints ~= 0 then
          table.insert(result, {text = " H " .. ws_hints, guifg = "#88c0d0"})
        end

        if ws_infos ~= 0 then
          table.insert(result, {text = " I " .. ws_infos, guifg = "#50B0F0"})
        end
        return result
      end,
    }
  }
}
