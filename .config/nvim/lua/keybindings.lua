Keybind.g({
  -- COMMAND

  -- handled in functions.lua
  -- { 'n', 'j', 'gj', { noremap = true } },
  -- { 'n', 'k', 'gk', { noremap = true } },
  -- { 'n', '$', 'g$', { noremap = true } },
  -- { 'n', '0', 'g0', { noremap = true } },

  -- keep asterisk and pound to be case sensitive
  { 'n', '<leader>*', ":let @/='\\C\\<' . expand('<cword>') . '\\>'<CR>:let v:searchforward=1<CR>n", { noremap = true } },
  { 'n', '<leader>#', ":let @/='\\C\\<' . expand('<cword>') . '\\>'<CR>:let v:searchforward=0<CR>n", { noremap = true } },


  -- reload all opened buffer
  { 'n', '<leader>br', ':bufdo e<CR>', { noremap = true } },

  -- Copy, Paste and Copy Whole File to clipboard
  { 'v', '<leader>c', '"+y' },
  { 'n', '<leader>v', ':r !pbpaste<CR><CR>' },
  { 'n', '<leader>x', ':%w !pbcopy<CR><CR>' },

  -- Toggle wrap
  { 'n', '<leader>w', ':set wrap!<CR>', { noremap = true } },

  -- Unhighlight search
  { 'n', '<LEADER><SPACE>', ':nohlsearch<CR>', { noremap = true } },

  -- Toggle check spelling
  { 'n', '<leader>s', ':set spell! spelllang=en_us<CR>', { noremap = true } },

  -- resize pane
  { 'n', '<A-h>', '<C-w><' },
  { 'n', '<A-k>', '<C-W>-' },
  { 'n', '<A-j>', '<C-W>+' },
  { 'n', '<A-l>', '<C-w>>' },

  -- source
  --{ 'n', '<leader>%', ':source ~/.config/nvim/init.vim<CR>', { noremap = true } },
  { 'n', '<leader>%', ':luafile ~/.config/nvim/init.lua<CR>', { noremap = true } },

  -- inspect unicode from ga to <leader>a
  { 'n', '<leader>a', 'ga', { noremap = true } },

  -- identify syntax highlighting below cursor
  { 'n', '<leader>h', ':echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . \'> trans<\'\
 . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"\
 . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>' },


  -- INSERT

  -- Remap <c-d> to delete
  { 'i', '<C-d>', '<Del>', { noremap = true } },

  -- Remap escape to j + k
  { 'i', 'jk', '<ESC>', { noremap = true } },
  { 'i', '<C-c>', '<ESC>', { noremap = true } },

  -- PLUGINS

  -- |vim-sneak|
    { 'n', 'f', '<Plug>Sneak_f' },
    { 'n', 'F', '<Plug>Sneak_F' },
    { 'n', 't', '<Plug>Sneak_t' },
    { 'n', 'T', '<Plug>Sneak_T' },

  -- |coc.nvim|
    { 'n', 'gd', '<Plug>(coc-definition)' },
    { 'n', '<2-LeftMouse>', '<Plug>(coc-definition)' },
    { 'n', 'gh', ':call ShowDocumentation()<CR>' },
    { 'n', '<2-LeftMouse>', ':call ShowDocumentation()<CR>' },
    { 'n', '<leader>gd', '<Plug>(coc-diagnostics-info)' },
    { 'n', '[c', '<Plug>(coc-git-prevchunk)' },
    { 'n', ']c', '<Plug>(coc-git-nextchunk)' },
    { 'n', '<C-w>g', '<Plug>(coc-git-chunkinfo)' },
    { 'n', '<C-w><C-g>', '<Plug>(coc-git-chunkinfo)' },
    { 'n', ']C', '<Plug>(coc-diagnostic-next)' },
    { 'n', '[C', '<Plug>(coc-diagnostic-prev)' },
    { 'n', 'gi', '<Plug>(coc-implementation)' },
    { 'n', 'gr', '<Plug>(coc-references)' },
    { 'n', 'ga', ':call CocAction("codeAction")<CR>' },
    { 'x', 'gs',  '<Plug>(coc-codeaction-selected)' },
    { 'n', 'gs',  '<Plug>(coc-codeaction-selected)' },

  -- |git-messenger|
    { 'n', '<C-w>m', '<Plug>(git-messenger)' },
    { 'n', '<C-w><C-m>', '<Plug>(git-messenger)' },

  -- |telescope.nvim|
    { 'n', '<C-p>', '<CMD>Telescope find_files<CR>', { noremap = true } },
    { 'n', '<C-i>', '<CMD>Telescope live_grep<CR>', { noremap = true } },
    { 'n', '<C-s>', '<CMD>Telescope grep_string<CR>', { noremap = true } },
    { 'n', '<C-g>', '<CMD>Telescope git_status<CR>', { noremap = true } },
    -- { 'n', '<C-b>', '<CMD>Telescope' buffers<CR>, { noremap = true } },
    -- { 'n', '<C-t>', '<CMD>Telescope' help_tags<CR>, { noremap = true } },
    -- { 'n', '<C-t>', '<CMD>Telescope treesitter<CR>', { noremap = true } },
    { 'n', '<C-t>', '<CMD>Telescope coc document_symbols<CR>', { noremap = true } },

  -- |barbar.nvim|
    -- Move to previous/next
    { 'n',    'gB',    ':BufferPrevious<CR>', { noremap = true } },
    { 'n',    'gb',    ':BufferNext<CR>', { noremap = true } },
    -- Re-order to previous/next
    { 'n',    '<A-<>', ':BufferMovePrevious<CR>', { noremap = true } },
    { 'n',    '<A->>', ':BufferMoveNext<CR>', { noremap = true } },
    -- Goto buffer in position...
    { 'n',    '<A-1>', ':BufferGoto 1<CR>', { noremap = true } },
    { 'n',    '<A-2>', ':BufferGoto 2<CR>', { noremap = true } },
    { 'n',    '<A-3>', ':BufferGoto 3<CR>', { noremap = true } },
    { 'n',    '<A-4>', ':BufferGoto 4<CR>', { noremap = true } },
    { 'n',    '<A-5>', ':BufferGoto 5<CR>', { noremap = true } },
    { 'n',    '<A-6>', ':BufferGoto 6<CR>', { noremap = true } },
    { 'n',    '<A-7>', ':BufferGoto 7<CR>', { noremap = true } },
    { 'n',    '<A-8>', ':BufferGoto 8<CR>', { noremap = true } },
    { 'n',    '<A-9>', ':BufferLast<CR>', { noremap = true } },
    -- Close buffer
    { 'n',    'gx',    ':BufferClose<CR>', { noremap = true } },
    -- Wipeout buffer
    --                 :BufferWipeout<CR>
    -- Close commands
    --                 :BufferCloseAllButCurrent<CR>
    --                 :BufferCloseBuffersLeft<CR>
    --                 :BufferCloseBuffersRight<CR>
    -- Magic buffer-picking mode
    { 'n',    '<A-s>', ':BufferPick<CR>', { noremap = true } },

  -- |nvim-tree|
    { 'n', '<LEADER>e', ':NvimTreeToggle<CR>', { noremap = true } },
    { 'n', '<LEADER>r', ':NvimTreeFindFile<CR>', { noremap = true } },
})
