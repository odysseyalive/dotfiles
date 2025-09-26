" lightline config
set laststatus=2

if g:patched_fonts
    " lightline for patched fonts
    let g:lightline = {
        \ 'active': {
        \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ] ]
        \ },
        \ 'component_function': {
        \   'fugitive': 'LightlineFugitive',
        \   'filename': 'LightlineFilename'
        \ },
        \ 'separator': { 'left': '', 'right': '' },
        \ 'subseparator': { 'left': '', 'right': '' }
        \ }
    function! LightlineModified()
        return &ft =~# 'help\|vimfiler' ? '' : &modified ? '+' : &modifiable ? '' : '-'
    endfunction
    function! LightlineReadonly()
        return &ft !~? 'help\|vimfiler' && &readonly ? '' : ''
    endfunction
    function! LightlineFilename()
        return (LightlineReadonly() !=# '' ? LightlineReadonly() . ' ' : '') .
        \ (&ft ==# 'vimfiler' ? vimfiler#get_status_string() :
        \  &ft ==# 'unite' ? unite#get_status_string() :
        \  &ft ==# 'vimshell' ? vimshell#get_status_string() :
        \ expand('%') !=# '' ? expand('%') : '[No Name]') .
        \ (LightlineModified() !=# '' ? ' ' . LightlineModified() : '')
    endfunction
    function! LightlineFugitive()
        if &ft !~? 'vimfiler' && exists('*FugitiveHead')
            let branch = FugitiveHead()
            return branch !=# '' ? ' '.branch : ''
        endif
        return ''
    endfunction
else
    " lightline for non-patched fonts
    let g:lightline = {
            \ 'active': {
            \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ] ]
            \ },
            \ 'component_function': {
            \   'fugitive': 'LightlineFugitive',
            \   'filename': 'LightlineFilename'
            \ }
            \ }
    function! LightlineModified()
        return &ft =~# 'help\|vimfiler' ? '' : &modified ? '+' : &modifiable ? '' : '-'
    endfunction
    function! LightlineReadonly()
        return &ft !~? 'help\|vimfiler' && &readonly ? 'RO' : ''
    endfunction
    function! LightlineFilename()
        return (LightlineReadonly() !=# '' ? LightlineReadonly() . ' ' : '') .
        \ (&ft ==# 'vimfiler' ? vimfiler#get_status_string() :
        \  &ft ==# 'unite' ? unite#get_status_string() :
        \  &ft ==# 'vimshell' ? vimshell#get_status_string() :
        \ expand('%') !=# '' ? expand('%') : '[No Name]') .
        \ (LightlineModified() !=# '' ? ' ' . LightlineModified() : '')
    endfunction
    function! LightlineFugitive()
        if &ft !~? 'vimfiler' && exists('*FugitiveHead')
            return FugitiveHead()
        endif
        return ''
    endfunction
endif
