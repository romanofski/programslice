"======================================================================
" vim-programslice - A Python filetype plugin to slice python programs
"
" Language:    Python (ft=python)
" Maintainer:  Róman Joost <roman@bromeco.de>
" Version:     0.1
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"
"=============================================================================


" Do initialize below once.
if exists("g:programslice_ftplugin_loaded")
    finish
endif
let g:programslice_ftplugin_loaded = 1

" Highlight group.
" This group is used to highlight the sliced lines, which depend on the
" starting line.
execute 'highlight link ProgramSlice SpellBad'


" Initialize.
python << EOF
import vim
from programslice import slice_vim_buffer
EOF


" Cleares all highlighted lines
function! s:ClearProgramSlice()
    let matches = getmatches()
    for matchId in matches
        if matchId['group'] == 'ProgramSlice'
            call matchdelete(matchId['id'])
        endif
    endfor
    if exists('b:programslice_matchedlines')
        unlet b:programslice_matchedlines
    endif
endfunction

" Runs the slice from the current line
function! RunProgramSlice()
    if exists('b:programslice_matchedlines')
        call s:ClearProgramSlice()
    endif
    let b:programslice_matchedlines = {}
python << EOF

currentlineno, col = vim.current.window.cursor
contents = vim.current.buffer[:]
contents = '\n'.join(contents) + '\n'
vimenc = vim.eval('&encoding')
if vimenc:
    contents = contents.decode(vimenc)
lines = slice_vim_buffer(currentlineno, contents, vim.current.buffer.name)
for line in lines:
    vim.command(r"let s:mID = matchadd('ProgramSlice', '\%" + str(line) + r"l\n\@!')")
EOF
endfunction
