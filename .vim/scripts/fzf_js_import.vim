" Function based on unmerged PR from here https://github.com/junegunn/fzf.vim/pull/628
function! s:generate_relative_js(path)
  let target = getcwd() . '/' . (join(a:path))
  let base = expand('%:p:h')

  let prefix = ""
  while stridx(target, base) != 0
    let base = substitute(system('dirname ' . base), '\n\+$', '', '')
    let prefix = '../' . prefix
  endwhile

  if prefix == ''
    let prefix = './'
  endif

  let relative = prefix . substitute(target, base . '/', '', '')

  let withJsTrunc = substitute(relative, '\.[tj]sx\=$', "", "")

  return withJsTrunc

endfunction

function! JsFzfImport()
  return fzf#vim#complete#path(
        \ "fd",
        \ fzf#wrap({ 'reducer': function('s:generate_relative_js')})
        \ )
endfunction

inoremap <expr> <c-x><c-h> JsFzfImport()

