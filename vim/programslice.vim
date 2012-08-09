"======================================================================
" vim-programslice - A Python filetype plugin to slice python programs
"
" Language:    Python (ft=python)
" Maintainer:  Róman Joost <roman@bromeco.de>
"
"======================================================================


" Do initialize below once.
if exists("g:programslice_ftplugin_loaded")
    finish
endif
let g:programslice_ftplugin_loaded = 1
" The highlight group we link against to mark depending lines
if !exists('g:programslice_dependent_lines')
    let g:programslice_dependent_lines = 'WarningMsg'
endif

" Highlight group.
" This group is used to highlight the sliced lines, which depend on the
" starting line.
"
execute 'highlight link ProgramSlice ' . g:programslice_dependent_lines


" Cleares all highlighted lines
"
function! s:clear_slice_matches()
    let matches = getmatches()
    for matchId in matches
        if matchId['group'] == 'ProgramSlice'
            call matchdelete(matchId['id'])
        endif
    endfor
endfunction
exe 'command! -buffer -nargs=0 ClearSliceMatches :call s:clear_slice_matches()'

" Runs the slice from the current line
"
function! s:slice_buffer()
python << EOF
import vim
from programslice import slice_string

currentlineno, col = vim.current.window.cursor
contents = vim.current.buffer[:]
contents = '\n'.join(contents) + '\n'
vimenc = vim.eval('&encoding')
if vimenc:
    contents = contents.decode(vimenc)
lines = slice_string(currentlineno, contents, vim.current.buffer.name)
for line in lines:
    vim.command(r"let s:mID = matchadd('ProgramSlice', '\%" + str(line) + r"l\n\@!')")
EOF
endfunction
command! -nargs=0 SliceBuffer :call s:slice_buffer()
