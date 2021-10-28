------------------------------------------------------------
--                    CUSTOMIZATIONS
------------------------------------------------------------

vim.o.compatible = false
vim.o.encoding = 'UTF-8'

vim.opt.termguicolors = true
vim.o.background = 'dark'

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
-- vim.o.scrolloff = 2
vim.o.expandtab = true
vim.o.wrap = false
vim.o.linebreak = true
vim.o.list = true
--  vim.o.listchars = [[tab:▷\ ,trail:◻,nbsp:𐩒]]
vim.o.listchars = [[tab:> ,trail:-,nbsp:+]]

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

-- fold settings
-- https://www.reddit.com/r/neovim/comments/psl8rq/sexy_folds/
-- vim.wo.foldmethod = "expr"
-- vim.wo.foldexpr = "nvim_treesitter#foldexpr()"
-- vim.wo.foldtext =
    -- [[substitute(getline(v:foldstart),'\\t',repeat('\ ',&tabstop),'g').'...'.trim(getline(v:foldend)) ]]
-- vim.wo.fillchars = "fold:\\"
-- vim.wo.foldnestmax = 3
-- vim.wo.foldminlines = 1


------------------------------------------------------------
--                   DEFAULT KEYBINDINGS
------------------------------------------------------------

local function base_map(lhs)
  return function(mode)
    return function(rhs)
      vim.api.nvim_set_keymap(mode, lhs, rhs, {noremap = true, silent = true})
    end
  end
end

local function base_map_opt(lhs)
  return function(mode)
    return function(rhs)
      return function(opts)
        vim.api.nvim_set_keymap(mode, lhs, rhs, opts or {noremap = true, silent = true})
      end
    end
  end
end

local function noremap(lhs) return base_map(lhs)('') end
local function nnoremap(lhs) return base_map(lhs)('n') end
local function inoremap(lhs) return base_map(lhs)('i') end
local function cnoremap(lhs) return base_map(lhs)('c') end

local function noremapopt(lhs) return base_map_opt(lhs)('') end
local function nnoremapopt(lhs) return base_map_opt(lhs)('n') end

local function replaceTermcodes(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end

function _G.smart_wrap_nav_bindings(ifTrue,ifFalse)
  return vim.o.wrap == true and replaceTermcodes(ifTrue) or replaceTermcodes(ifFalse)
end

nnoremapopt 'j' [[v:lua.smart_wrap_nav_bindings("gj","j")]] ({expr = true, noremap = true})
nnoremapopt 'k' [[v:lua.smart_wrap_nav_bindings("gk","k")]] ({expr = true, noremap = true})
nnoremapopt '0' [[v:lua.smart_wrap_nav_bindings("g0","0")]] ({expr = true, noremap = true})
nnoremapopt '$' [[v:lua.smart_wrap_nav_bindings("g$","$")]] ({expr = true, noremap = true})

-- resource the neovim configurations
noremap '<LEADER>%' ':luafile ~/.config/nvim/init.lua<CR> | :NightfoxLoad nordfox<CR>'

-- unhighlight search
nnoremap '<LEADER><SPACE>' ':nohlsearch<CR>'

noremap '<LEADER>w' ':set wrap!<CR>'
noremap '<LEADER>s' ':set spell! spelllang=en_us<CR>'

-- resize window
noremap '<A-h>' '<C-w><'
noremap '<A-k>' '<C-w>-'
noremap '<A-j>' '<C-w>+'
noremap '<A-l>' '<C-w>>'

-- keep asterisk and pound to be case sensitive
nnoremap '<LEADER>*' [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=1<CR>n]]
nnoremap '<LEADER>#' [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=0<CR>n]]

-- why does vim's Y behavior different?
nnoremap 'Y' 'y$'

-- copy, paste and copy whole file to clipboard
noremapopt '<LEADER>cs'   '"+y' ({})
noremapopt '<LEADER>v'    ':r !pbpaste<CR><CR>' ({})
noremapopt '<LEADER>ca'   ':%w !pbcopy<CR><CR>' ({})

-- identify syntax below cursor with <LEADER>h
-- replaced by nvim-treesitter one below
-- noremap '<LEADER>h' [[:echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>]]

nnoremap '<LEADER>a' 'ga'
-- nnoremap '<LEADER>x' 'gx'

inoremap '<C-d>' '<DEL>'

inoremap 'jk'    '<ESC>'
inoremap '<C-c>' '<ESC>'

-- make emacs navigation available on EX-mode

cnoremap '<C-a>'   '<Home>'
cnoremap '<C-e>'   '<End>'
cnoremap '<C-b>'   '<Left>'
cnoremap '<C-f>'   '<Right>'
cnoremap '<C-M-b>' '<S-Left>'
cnoremap '<C-M-f>' '<S-Right>'


------------------------------------------------------------
--                       RESOURCES
------------------------------------------------------------


require('plenary.reload').reload_module('plugins')
require('plenary.reload').reload_module('ybbond-compat')


require'plugins'
require'ybbond-compat'


------------------------------------------------------------
--                   PLUGINS KEYBINDINGS
------------------------------------------------------------

-- bufferline
-- nnoremap 'gb' '<CMD>BufferLineCycleNext<CR>'
-- nnoremap 'gB' '<CMD>BufferLineCyclePrev<CR>'
-- nnoremap 'g>' '<CMD>BufferLineMoveNext<CR>'
-- nnoremap 'g<' '<CMD>BufferLineMovePrev<CR>'

-- bufdelete
nnoremap 'gx' '<CMD>Bdelete<CR>'

-- cokeline
nnoremap 'gb' '<CMD>lua require("cokeline").focus({step = 1})<CR>'
nnoremap 'gB' '<CMD>lua require("cokeline").focus({step = -1})<CR>'
nnoremap 'g>' '<CMD>lua require("cokeline").switch({step = 1})<CR>'
nnoremap 'g<' '<CMD>lua require("cokeline").switch({step = -1})<CR>'

-- nvim-tree.lua
nnoremap '<LEADER>e' '<CMD>NvimTreeToggle<CR>'
nnoremap '<LEADER>r' '<CMD>NvimTreeFindFile<CR>'

-- telescope.nvim
nnoremap '<C-t><C-p>'        [[<CMD>lua require("telescope.builtin").find_files{ find_command={"fd","-E=.git","--hidden","-t=f"}} hidden=true<CR>]]
nnoremap '<C-t><C-\\><C-p>'  [[<CMD>lua require("telescope.builtin").find_files{ find_command={"fd","-E=.git","--hidden","-t=f","--no-ignore"}} hidden=true<CR>]]
nnoremap '<C-t><C-i>'        [[<CMD>Telescope live_grep hidden=true<CR>]]
nnoremap '<C-t><C-\\><C-i>'  [[<CMD>Telescope live_grep hidden=true grep_open_files=true<CR>]]
nnoremap '<C-t><C-n>'        [[<CMD>lua require("telescope.builtin").live_grep{ find_command={"rg","--no-heading","--hidden","-g='!.git/**'","--with-filename","--line-number","--column","--smart-case","--ignore","--regexp"}}<CR>]]
nnoremap '<C-t><C-\\><C-n>'  [[<CMD>lua require("telescope.builtin").live_grep{ find_command={"rg","--no-heading","--hidden","-g='!.git/**'",'--with-filename',"--line-number","--column","--smart-case","--no-ignore","--regexp"}}<CR>]]

nnoremap '<C-t><C-s>'        [[<CMD>Telescope grep_string<CR>]]
nnoremap '<C-t><C-r>'        [[<CMD>Telescope registers<CR>]]
nnoremap '<C-t><C-b>'        [[<CMD>Telescope buffers<CR>]]
nnoremap '<C-t><C-h>'        [[<CMD>Telescope help_tags<CR>]]
nnoremap '<C-t><C-m>'        [[<CMD>Telescope marks<CR>]]
nnoremap '<C-t><C-a>'        [[<CMD>Telescope commands<CR>]]
nnoremap '<C-t><C-t>'        [[<CMD>Telescope treesitter<CR>]]

nnoremap '<C-g><C-g>'        [[<CMD>Telescope git_status<CR>]]

--fugitive
nnoremap '<LEADER>gb' '<CMD>Git blame<CR>'
nnoremap '<LEADER>go' '<CMD>GBrowse<CR>'

-- nvim-treesitter
noremap '<LEADER>h' '<CMD>TSHighlightCapturesUnderCursor<CR>'

-- vim-sneak
nnoremap 'f' '<Plug>Sneak_f'
nnoremap 'F' '<Plug>Sneak_F'
nnoremap 't' '<Plug>Sneak_t'
nnoremap 'T' '<Plug>Sneak_T'

-- trouble.nvim
nnoremap "<LEADER>xx" "<CMD>Trouble<CR>"
nnoremap "<LEADER>xw" "<CMD>Trouble lsp_workspace_diagnostics<CR>"
nnoremap "<LEADER>xd" "<CMD>Trouble lsp_document_diagnostics<CR>"
nnoremap "<LEADER>xl" "<CMD>Trouble loclist<CR>"
nnoremap "<LEADER>xq" "<CMD>Trouble quickfix<CR>"
--nnoremap "<LEADER>xr" "<cmd>Trouble lsp_references<cr>"
nnoremap "<LEADER>xr" "<CMD>TroubleRefresh<CR>"
