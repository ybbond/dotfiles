------------------------------------------------------------
--                    CUSTOMIZATIONS
------------------------------------------------------------

vim.o.compatible = false
vim.o.encoding = 'UTF-8'

vim.opt.termguicolors = true
vim.o.background = 'dark'

-- vim.cmd('colorscheme OceanicNext')
-- vim.cmd('colorscheme mariana')

-- moved to configs/nightfox-nvim.lua
-- vim.cmd('colorscheme nightfox')

-- vim.cmd('syntax enable')
-- vim.cmd('syntax on')
vim.cmd('filetype plugin on')
vim.cmd('filetype plugin indent on')

vim.o.paste = false
vim.o.pastetoggle = '<F2>'

vim.o.hidden = true

-- used by which-key.nvim
vim.o.timeoutlen = 100

vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.scrolloff = 2
vim.o.expandtab = true
vim.o.wrap = false
vim.o.linebreak = true
vim.o.list = true
vim.o.listchars = [[tab:▷\ ,trail:◻,nbsp:𐩒]]

vim.o.mouse = 'n'
vim.o.showmode = false

vim.o.number = true
vim.o.relativenumber = true

vim.o.ruler = true
vim.o.showmatch = true
vim.o.cursorline = true

vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.incsearch = true
vim.o.hlsearch = true

vim.o.inccommand = 'split'

-- originally used by |vim-fugitive|
vim.o.diffopt = vim.o.diffopt .. ',vertical'


------------------------------------------------------------
--                   DEFAULT KEYBINDINGS
------------------------------------------------------------


-- https://oroques.dev/notes/neovim-init/#mappings
-- local function map(mode, lhs, rhs, opts)
--   local options = {noremap = true}
--   if opts then options = vim.tbl_extend('force', options, opts) end
--   vim.api.nvim_set_keymap(mode, lhs, rhs, options)
-- end


local function replaceTermcodes(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end

function _G.smart_wrap_nav_bindings(ifTrue,ifFalse)
  return vim.o.wrap == true and replaceTermcodes(ifTrue) or replaceTermcodes(ifFalse)
end

vim.api.nvim_set_keymap('n', 'j', [[v:lua.smart_wrap_nav_bindings("gj","j")]], {expr = true, noremap = true})
vim.api.nvim_set_keymap('n', 'k', [[v:lua.smart_wrap_nav_bindings("gk","k")]], {expr = true, noremap = true})
vim.api.nvim_set_keymap('n', '0', [[v:lua.smart_wrap_nav_bindings("g0","0")]], {expr = true, noremap = true})
vim.api.nvim_set_keymap('n', '$', [[v:lua.smart_wrap_nav_bindings("g$","$")]], {expr = true, noremap = true})

-- resource the neovim configurations
vim.api.nvim_set_keymap('', '<LEADER>%', ':luafile ~/.config/nvim/init.lua<CR>', {noremap = true})

-- unhighlight search
vim.api.nvim_set_keymap('n', '<LEADER><SPACE>', ':nohlsearch<CR>', {noremap = true, silent = true})

vim.api.nvim_set_keymap('', '<LEADER>w', ':set wrap!<CR>', {})
vim.api.nvim_set_keymap('', '<LEADER>s', ':set spell! spelllang=en_us<CR>', {noremap = true})

-- resize window
vim.api.nvim_set_keymap('', '<A-h>', '<C-w><', {silent = true})
vim.api.nvim_set_keymap('', '<A-k>', '<C-w>-', {silent = true})
vim.api.nvim_set_keymap('', '<A-j>', '<C-w>+', {silent = true})
vim.api.nvim_set_keymap('', '<A-l>', '<C-w>>', {silent = true})

-- keep asterisk and pound to be case sensitive
vim.api.nvim_set_keymap('n', '<LEADER>*', [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=1<CR>n]], {noremap = true})
vim.api.nvim_set_keymap('n', '<LEADER>#', [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=0<CR>n]], {noremap = true})

-- why does vim's Y behavior different?
-- but in neovim HEAD~, it's fixed!
-- vim.api.nvim_set_keymap('n', 'Y', 'y$', {noremap = true})

-- copy, paste and copy whole file to clipboard
vim.api.nvim_set_keymap('', '<LEADER>cs', '"+y', {})
vim.api.nvim_set_keymap('', '<LEADER>v', ':r !pbpaste<CR><CR>', {})
vim.api.nvim_set_keymap('', '<LEADER>ca', ':%w !pbcopy<CR><CR>', {})

-- identify syntax below cursor with <LEADER>h
-- replaced by nvim-treesitter one below
-- vim.api.nvim_set_keymap('', '<LEADER>h', [[:echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>]], {})

-- rebind vim's ga to be <LEADER>a
vim.api.nvim_set_keymap('n', '<LEADER>a', 'ga', {noremap = true})

-- rebind vim's gx to be <LEADER>x
-- vim.api.nvim_set_keymap('n', '<LEADER>x', 'gx', {noremap = true})

-- remap <C-d> to Delete
vim.api.nvim_set_keymap('i', '<C-d>', '<DEL>', {noremap = true})

-- remap jk and <C-c> to escape
vim.api.nvim_set_keymap('i', 'jk', '<ESC>', {noremap = true})
vim.api.nvim_set_keymap('i', '<C-c>', '<ESC>', {noremap = true})

-- make emacs navigation available on EX-mode

vim.api.nvim_set_keymap('c', '<C-a>', '<Home>', {noremap = true})
vim.api.nvim_set_keymap('c', '<C-e>', '<End>', {noremap = true})
vim.api.nvim_set_keymap('c', '<C-b>', '<Left>', {noremap = true})
vim.api.nvim_set_keymap('c', '<C-f>', '<Right>', {noremap = true})
vim.api.nvim_set_keymap('c', '<C-M-b>', '<S-Left>', {noremap = true})
vim.api.nvim_set_keymap('c', '<C-M-f>', '<S-Right>', {noremap = true})


------------------------------------------------------------
--                       RESOURCES
------------------------------------------------------------


-- require('plenary.reload').reload_module('configs')
require('plenary.reload').reload_module('ybbond-compat')

require'plugins'
-- require'configs'
require'ybbond-compat'



------------------------------------------------------------
--                   PLUGINS KEYBINDINGS
------------------------------------------------------------

-- bufferline
vim.api.nvim_set_keymap('n', 'gb', '<CMD>BufferLineCycleNext<CR>',{noremap = true})
vim.api.nvim_set_keymap('n', 'gB', '<CMD>BufferLineCyclePrev<CR>',{noremap = true})
vim.api.nvim_set_keymap('n', 'g>', '<CMD>BufferLineMoveNext<CR>', {noremap = true})
vim.api.nvim_set_keymap('n', 'g<', '<CMD>BufferLineMovePrev<CR>', {noremap = true})
vim.api.nvim_set_keymap('n', 'gx', '<CMD>Bdelete<CR>',            {noremap = true})

-- nvim-tree.lua
vim.api.nvim_set_keymap('n', '<LEADER>e', '<CMD>NvimTreeToggle<CR>',   {noremap = true})
vim.api.nvim_set_keymap('n', '<LEADER>r', '<CMD>NvimTreeFindFile<CR>', {noremap = true})

-- telescope.nvim
vim.api.nvim_set_keymap('n', '<C-t><C-p>',        [[<CMD>lua require("telescope.builtin").find_files{ find_command={"fd","-E=.git","--hidden","-t=f"}} hidden=true<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-\\><C-p>',  [[<CMD>lua require("telescope.builtin").find_files{ find_command={"fd","-E=.git","--hidden","-t=f","--no-ignore"}} hidden=true<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-i>',        [[<CMD>Telescope live_grep hidden=true<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-\\><C-i>',  [[<CMD>Telescope live_grep hidden=true grep_open_files=true<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-n>',        [[<CMD>lua require("telescope.builtin").live_grep{ find_command={"rg","--no-heading","--hidden","-g='!.git/**'","--with-filename","--line-number","--column","--smart-case","--ignore","--regexp"}}<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-\\><C-n>',  [[<CMD>lua require("telescope.builtin").live_grep{ find_command={"rg","--no-heading","--hidden","-g='!.git/**'",'--with-filename',"--line-number","--column","--smart-case","--no-ignore","--regexp"}}<CR>]], {noremap = true})

vim.api.nvim_set_keymap('n', '<C-t><C-s>',    [[<CMD>Telescope grep_string<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-r>',    [[<CMD>Telescope registers<CR>]],   {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-b>',    [[<CMD>Telescope buffers<CR>]],     {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-h>',    [[<CMD>Telescope help_tags<CR>]],   {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-m>',    [[<CMD>Telescope marks<CR>]],       {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-a>',    [[<CMD>Telescope commands<CR>]],    {noremap = true})
vim.api.nvim_set_keymap('n', '<C-t><C-t>',    [[<CMD>Telescope treesitter<CR>]],  {noremap = true})

vim.api.nvim_set_keymap('n', '<C-g><C-g>',    [[<CMD>Telescope git_status<CR>]],  {noremap = true})

-- local ts_utils = require("nvim-treesitter.ts_utils")

-- function Test()
--   local curr_node = ts_utils.get_node_at_cursor(0)
--   -- local prev_node = ts_utils.get_previous_node(curr_node, true, false)

--   print(ts_utils.get_node_text(curr_node, 0)[1])
--   -- print(ts_utils.get_node_text(prev_node, 0)[1])
-- end

-- nvim-treesitter
-- vim.api.nvim_set_keymap('', '<LEADER>f', '<CMD>lua Test()<CR>', {noremap = true})
vim.api.nvim_set_keymap('', '<LEADER>h', '<CMD>TSHighlightCapturesUnderCursor<CR>', {noremap = true})

-- vim-sneak
vim.api.nvim_set_keymap('n', 'f', '<Plug>Sneak_f', {})
vim.api.nvim_set_keymap('n', 'F', '<Plug>Sneak_F', {})
vim.api.nvim_set_keymap('n', 't', '<Plug>Sneak_t', {})
vim.api.nvim_set_keymap('n', 'T', '<Plug>Sneak_T', {})

-- trouble.nvim
vim.api.nvim_set_keymap("n", "<leader>xx", "<cmd>Trouble<cr>", {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<leader>xw", "<cmd>Trouble lsp_workspace_diagnostics<cr>", {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<leader>xd", "<cmd>Trouble lsp_document_diagnostics<cr>", {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<leader>xl", "<cmd>Trouble loclist<cr>", {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<leader>xq", "<cmd>Trouble quickfix<cr>", {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<leader>xr", "<cmd>Trouble lsp_references<cr>", {silent = true, noremap = true})
