local new_menu = require('configs/flutter-tools-custom-telescope-menu')

local has_telescope, telescope = pcall(require, "telescope")
if not has_telescope then
  error("This plugin requires telescope.nvim (https://github.com/nvim-telescope/telescope.nvim)")
end

return telescope.register_extension({
  exports = {
    commands = new_menu.commands,
  },
})
