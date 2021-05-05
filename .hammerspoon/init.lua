local hyper = {'cmd', 'alt', 'ctrl', 'shift'}

-- Load and create a new switcher
local HyperKey = hs.loadSpoon("HyperKey")
hyperKey = HyperKey:new(hyper)

-- Bind some applications to keys
hyperKey
  :bind('l'):toApplication('/Applications/FSNotes.app')

-- Bind some functions to keys
local reloadHammerspoon = function()
  -- hs.application.launchOrFocus("Hammerspoon")
  hs.reload()
end

hyperKey
  :bind('r'):toFunction("Reload Hammerspoon", reloadHammerspoon)

--------------------------------------------------------------------
--                     screen manipulation
--------------------------------------------------------------------

-- -- option + ctrl + h to make active window half to the left
-- hs.hotkey.bind({"alt", "ctrl"}, "H", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   f.x = max.x
--   f.y = max.y
--   f.w = max.w / 2
--   f.h = max.h
--   win:setFrame(f)
-- end)

-- -- option + ctrl + l to make active window half to the right
-- hs.hotkey.bind({"alt", "ctrl"}, "L", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   f.x = max.x + (max.w / 2)
--   f.y = max.y
--   f.w = max.w / 2
--   f.h = max.h
--   win:setFrame(f)
-- end)

-- -- option + ctrl + j to make active window two-third to the left
-- hs.hotkey.bind({"alt", "ctrl"}, "J", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   f.x = max.x
--   f.y = max.y
--   f.w = max.w * (2/3)
--   f.h = max.h
--   win:setFrame(f)
-- end)

-- -- option + ctrl + k to make active window two-third to the right
-- hs.hotkey.bind({"alt", "ctrl"}, "K", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   f.x = max.x + (max.w * (1/3))
--   f.y = max.y
--   f.w = max.w * (2/3)
--   f.h = max.h
--   win:setFrame(f)
-- end)

-- -- option + ctrl + i to make active window one-third to the right
-- hs.hotkey.bind({"alt", "ctrl"}, "I", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   f.x = max.x + (max.w * (2/3))
--   f.y = max.y
--   f.w = max.w * (1/3)
--   f.h = max.h
--   win:setFrame(f)
-- end)

-- -- option + ctrl + u to make active window one-third to the left
-- hs.hotkey.bind({"alt", "ctrl"}, "U", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   f.x = max.x
--   f.y = max.y
--   f.w = max.w * (1/3)
--   f.h = max.h
--   win:setFrame(f)
-- end)

-- -- option + ctrl + s to make active window three-fourth to the left
-- hs.hotkey.bind({"alt", "ctrl"}, "S", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   f.x = max.x
--   f.y = max.y
--   f.w = max.w * (3/4)
--   f.h = max.h
--   win:setFrame(f)
-- end)

-- -- option + ctrl + d to make active window aligned right
-- hs.hotkey.bind({"alt", "ctrl"}, "D", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   f.x = max.x + max.w - f.w
--   f.y = max.y
--   win:setFrame(f)
-- end)

-- -- option + ctrl + c to make active window centered
-- hs.hotkey.bind({"alt", "ctrl"}, "C", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   -- f.x = max.x + (max.w * (1/6))
--   -- f.y = max.y + (max.h * (1/6))
--   -- f.w = max.w * (2/3)
--   -- f.h = max.h * (2/3)
--   -- f.x = max.x + (max.w * (1/8))
--   -- f.y = max.y + (max.h * (1/8))
--   -- f.w = max.w * (3/4)
--   -- f.h = max.h * (3/4)
--   f.x = max.x + (max.w * (3/16))
--   f.y = max.y + (max.h * (2/16))
--   f.w = max.w * (5/8)
--   f.h = max.h * (6/8)
--   win:setFrame(f)
-- end)

-- -- option + ctrl + c to make active window centered smaller
-- hs.hotkey.bind({"alt", "ctrl", "shift"}, "C", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   f.x = max.x + (max.w * (5/16))
--   f.y = max.y + (max.h * (2/16))
--   f.w = max.w * (3/8)
--   f.h = max.h * (6/8)
--   win:setFrame(f)
-- end)

-- -- option + ctrl + enter to make active window maximized
-- hs.hotkey.bind({"alt", "ctrl"}, "Return", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   f.x = max.x
--   f.y = max.y
--   f.w = max.w
--   f.h = max.h
--   win:setFrame(f)
-- end)

-- -- option + ctrl + N to make active window centered tall
-- hs.hotkey.bind({"alt", "ctrl"}, "N", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()

--   f.x = max.x + (max.w * (4/16))
--   f.w = max.w * (4/8)
--   f.y = max.y
--   f.h = max.h
--   win:setFrame(f)
-- end)

-- -- layoutWatcher = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, function(e)
-- --     local flags = e:getFlags()
-- --     if flags.cmd and not (flags.alt or flags.shift or flags.ctrl or flags.fn) then
-- --         local keyCode = e:getKeyCode()
-- --         if keyCode == 0x37 then
-- --             hs.alert.show("left cmd key")
-- --         elseif keyCode == 0x36 then
-- --             hs.alert.show("right cmd key")
-- --         end
-- --     end
-- -- end):start()

hs.alert.show("Config loaded")