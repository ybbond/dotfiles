Command.cmd({
  'au BufNewFile,BufRead *.(c|v|vv|py) setlocal tabstop=4',
  'au BufNewFile,BufRead *.(c|v|vv|py) setlocal shiftwidth=4',
  'au BufNewFile,BufRead *.(c|v|vv|py) setlocal set noexpandtab',
})

Augroup.cmds({
    -- highlight on yank!!!
    highlight_yank = {
        {"TextYankPost", "* silent! lua require'vim.highlight'.on_yank({timeout = 400})"},
    },
})

