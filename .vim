let g:hasktag_dir_list = ["src", "test"]
let g:vim_ignore = ["cabal-dev", "dist"]

function! RegenHaskTags()
  let dir_list = join(g:hasktag_dir_list)
  silent execute "!hasktags -c " . dir_list . " -f tags"
endfunction

command! RegenHaskTags call RegenHaskTags()

map <leader><C-t> :RegenHaskTags<CR>:FufRenewCache<CR>:redraw!<CR>
