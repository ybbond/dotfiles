vim.api.nvim_exec2(
[[
fun! YbbondOtherSetups()
  cnoremap <C-a>   <Home>
  cnoremap <C-e>   <End>
  cnoremap <C-b>   <Left>
  cnoremap <C-f>   <Right>
  cnoremap <C-A-b> <S-Left>
  cnoremap <C-A-f> <S-Right>

  " vimdiff
  " au VimEnter * if &diff | execute 'windo set wrap' | execute 'windo set nofoldenable' | endif
  if &diff
    set wrap
    set nofoldenable
    nnoremap gr :diffupdate<CR>
  endif

  if &diff
    map <LEADER>s :call IwhiteToggle()<CR>
    function! IwhiteToggle()
      if &diffopt =~ 'iwhite'
        set diffopt-=iwhite
      else
        set diffopt-=internal
        set diffopt+=iwhite
      endif
    endfunction
  endif

endfun
call YbbondOtherSetups()
]], {output = true})
