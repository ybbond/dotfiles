------------------------------------------------------------
--                    CUSTOMIZATIONS
------------------------------------------------------------

-- for filetype.nvim
vim.g.did_load_filetypes = 1

vim.o.compatible = false
vim.o.encoding = 'UTF-8'

vim.opt.termguicolors = true
vim.o.background = 'dark'

vim.cmd('filetype plugin on')
vim.cmd('filetype plugin indent on')

vim.o.paste = false
vim.o.pastetoggle = '<F2>'

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
-- vim.o.listchars = [[tab:▷\ ,trail:◻,nbsp:𐩒]]
vim.o.listchars = [[tab:> ,trail:-,nbsp:+]]
-- vim.o.showbreak = [[↪\ ]]
-- vim.o.listchars = [[tab:→\ ,eol:↲,nbsp:␣,trail:•,extends:⟩,precedes:⟨]]

vim.o.laststatus = 3

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
local function vnoremap(lhs) return base_map(lhs)('v') end

local function map(lhs) return base_map_opt(lhs)('') end
local function nmap(lhs) return base_map_opt(lhs)('n') end

local function replaceTermcodes(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end

function _G.smart_wrap_nav_bindings(ifTrue,ifFalse)
  return vim.o.wrap == true and replaceTermcodes(ifTrue) or replaceTermcodes(ifFalse)
end

nmap 'j' [[v:lua.smart_wrap_nav_bindings("gj","j")]] ({expr = true, noremap = true})
nmap 'k' [[v:lua.smart_wrap_nav_bindings("gk","k")]] ({expr = true, noremap = true})
nmap '0' [[v:lua.smart_wrap_nav_bindings("g0","0")]] ({expr = true, noremap = true})
nmap '$' [[v:lua.smart_wrap_nav_bindings("g$","$")]] ({expr = true, noremap = true})

-- resource the neovim configurations
noremap '<LEADER>%' ':luafile ~/.config/nvim/init.lua<CR>'

-- unhighlight search
nnoremap '<LEADER><SPACE>' ':nohlsearch<CR>'

noremap '<LEADER>w' ':set wrap!<CR>'
noremap '<LEADER>s' ':set spell! spelllang=en_us<CR>'

-- resize window
noremap 'Ó' '<C-w><' -- Alt Shift h
noremap '' '<C-w>-' -- Alt Shift k
noremap 'Ô' '<C-w>+' -- Alt Shift j
noremap 'Ò' '<C-w>>' -- Alt Shift l

-- keep asterisk and pound to be case sensitive
nnoremap '<LEADER>*' [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=1<CR>n]]
nnoremap '<LEADER>#' [[:let @/='\C\<' . expand('<cword>') . '\>'<CR>:let v:searchforward=0<CR>n]]

-- copy, paste and copy whole file to clipboard
map '<LEADER>cs'   '"+y' ({})
map '<LEADER>v'    ':r !pbpaste<CR><CR>' ({})
map '<LEADER>ca'   ':%w !pbcopy<CR><CR>' ({})

-- identify syntax below cursor with <LEADER>h
-- replaced by nvim-treesitter one below
-- noremap '<LEADER>h' [[:echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>]]

nnoremap '<LEADER>a' 'ga'
-- nnoremap '<LEADER>x' 'gx'

inoremap '<C-d>' '<DEL>'

inoremap 'jk'    '<ESC>'
inoremap '<C-c>' '<ESC>'

-- -- make emacs navigation available on EX-mode
-- -- done in ybbond-compat
-- cnoremap '<C-a>'   '<Home>'
-- cnoremap '<C-e>'   '<End>'
-- cnoremap '<C-b>'   '<Left>'
-- cnoremap '<C-f>'   '<Right>'
-- cnoremap '<C-A-b>' '<S-Left>'
-- cnoremap '<C-A-f>' '<S-Right>'

-- <C-R><C-W> command line mode, insert word under cursor


------------------------------------------------------------
--                       RESOURCES
------------------------------------------------------------


-- require('plenary.reload').reload_module('plugins')
-- require('plenary.reload').reload_module('ybbond-compat')


require'plugins'
require'ybbond-compat'
require'autos'


------------------------------------------------------------
--                   PLUGINS KEYBINDINGS
------------------------------------------------------------

-- bufdelete
nnoremap 'gx' '<CMD>Bdelete<CR>'

-- cokeline
nmap 'gb' '<Plug>(cokeline-focus-next)' ({})
nmap 'gB' '<Plug>(cokeline-focus-prev)' ({})
nmap 'g>' '<Plug>(cokeline-switch-next)' ({})
nmap 'g<' '<Plug>(cokeline-switch-prev)' ({})

-- nvim-tree.lua
nnoremap '<C-s><C-b>' '<CMD>NvimTreeToggle<CR>'
nnoremap '<C-s>b'     '<CMD>NvimTreeToggle<CR>'
nnoremap '<C-s><C-f>'  '<CMD>NvimTreeFindFile<CR>'
nnoremap '<C-s>f'     '<CMD>NvimTreeFindFile<CR>'

-- telescope.nvim
nnoremap '<C-t><C-p>'        [[<CMD>lua require("telescope.builtin").find_files({ find_command={"fd","-E=.git","--hidden","-t=f"}}) hidden=true<CR>]]
nnoremap '<C-t><C-\\><C-p>'  [[<CMD>lua require("telescope.builtin").find_files({ find_command={"fd","-E=.git","--hidden","-t=f","--no-ignore"}}) hidden=true<CR>]]
nnoremap '<C-t><C-i>'        [[<CMD>Telescope live_grep hidden=true<CR>]]
nnoremap '<C-t><C-\\><C-i>'  [[<CMD>Telescope live_grep hidden=true grep_open_files=true<CR>]]
nnoremap '<C-t><C-n>'        [[<CMD>lua require("telescope.builtin").live_grep({ find_command={"rg","--no-heading","--hidden","-g='!.git/**'","--with-filename","--line-number","--column","--smart-case","--ignore","--regexp"}})<CR>]]
nnoremap '<C-t><C-\\><C-n>'  [[<CMD>lua require("telescope.builtin").live_grep({ find_command={"rg","--no-heading","--hidden","-g='!.git/**'",'--with-filename',"--line-number","--column","--smart-case","--no-ignore","--regexp"}})<CR>]]

nnoremap '<C-t><C-s>'        [[<CMD>Telescope grep_string<CR>]]
nnoremap '<C-t><C-r>'        [[<CMD>Telescope registers<CR>]]
nnoremap '<C-t><C-b>'        [[<CMD>Telescope buffers<CR>]]
nnoremap '<C-t><C-h>'        [[<CMD>Telescope help_tags<CR>]]
nnoremap '<C-t><C-m>'        [[<CMD>Telescope marks<CR>]]
nnoremap '<C-t><C-a>'        [[<CMD>Telescope commands<CR>]]

nnoremap '<C-t><C-w>'        [[<CMD>Telescope diagnostics<CR>]]
nnoremap '<C-t><C-d>'        [[<CMD>Telescope diagnostics bufnr=0<CR>]]

nnoremap '<C-t><C-t>'        [[<CMD>Telescope yabs current_language_tasks<CR>]]

nnoremap '<C-g><C-g>'        [[<CMD>Telescope git_status<CR>]]


--fugitive
nnoremap '<LEADER>gb' '<CMD>Git blame<CR>'
nnoremap '<LEADER>go' '<CMD>GBrowse<CR>'

-- nvim-treesitter
noremap '<LEADER>h' '<CMD>TSHighlightCapturesUnderCursor<CR>'

-- nvim-dap
nnoremap '<LEADER>db' '<CMD>lua require"dap".toggle_breakpoint()<CR>'
nnoremap '<LEADER>dc' '<CMD>lua require"dap".continue()<CR>'
-- nvim-dap-ui
nnoremap '<LEADER>dus' '<CMD>lua require("dapui").setup()<CR>'
nnoremap '<LEADER>duo' '<CMD>lua require("dapui").open()<CR>'
nnoremap '<LEADER>duc' '<CMD>lua require("dapui").close()<CR>'
nnoremap '<LEADER>dut' '<CMD>lua require("dapui").toggle()<CR>'
nnoremap '<LEADER>due' '<CMD>lua require("dapui").eval()<CR>'
vnoremap '<LEADER>due' '<CMD>lua require("dapui").eval()<CR>'

vim.cmd[[colorscheme nordfox]]
