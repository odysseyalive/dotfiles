" Make it beautiful - colors and fonts

"tell the term has 256 colors
if (has("termguicolors") && $COLORTERM !~ "rxvt")
    set termguicolors
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

if has("gui_running")
  " Show tab number (useful for Cmd-1, Cmd-2.. mapping)
  " For some reason this doesn't work as a regular set command,
  " (the numbers don't show up) so I made it a VimEnter event
  autocmd VimEnter * set guitablabel=%N:\ %t\ %M

  set lines=60
  set columns=190

  if has("gui_gtk2")
    set guifont=Inconsolata\ XL\ 12,Inconsolata\ 15,Monaco\ 12
  else
    set guifont=Inconsolata\ XL:h17,Inconsolata:h20,Monaco:h17
  endif
else
  let g:CSApprox_loaded = 1
endif

" Light/Dark Theme Switch
let g:gruvboxtheme="light"
let g:gruvbox_termcolors=256
let g:gruvbox_contrast_dark="soft"
let g:gruvbox_contrast_light="soft"
let g:gruvbox_invert_selection=0
function! ToggleGruvbox()
    if g:gruvboxtheme == "light" 
        let g:gruvboxtheme="dark"
        set background=dark
    else
        let g:gruvboxtheme="light"
        set background=light
    endif
endfunction

nnoremap <silent> ,Ot :call ToggleGruvbox()<CR>

function! HasColorscheme(name) abort
    let pat = 'colors/'.a:name.'.vim'
    return !empty(globpath(&rtp, pat))
endfunction

" fix SpellBad
let g:gruvbox_guisp_fallback='bg'
au colorscheme * hi SpellBad ctermbg=NONE ctermfg=NONE cterm=underline gui=undercurl 
au colorscheme * hi SpellCap ctermbg=NONE ctermfg=NONE cterm=underline gui=undercurl 
au colorscheme * hi SpellLocal ctermbg=NONE ctermfg=NONE cterm=underline gui=undercurl 
au colorscheme * hi SpellRare ctermbg=NONE ctermfg=NONE cterm=underline gui=undercurl 

set t_Co=256
if HasColorscheme('gruvbox')
    colorscheme gruvbox
endif
set background=light


" uncomment for transparent background
let t:is_transparent = 1                                                                    
hi! Normal ctermbg=NONE guibg=NONE
hi! NonText ctermbg=NONE guibg=NONE

" prettier EasyMotion
hi EasyMotionTarget ctermbg=NONE ctermfg=red
hi EasyMotionShade  ctermbg=NONE ctermfg=gray

hi EasyMotionTarget2First ctermbg=NONE ctermfg=red
hi EasyMotionTarget2Second ctermbg=NONE ctermfg=lightred

hi EasyMotionMoveHL ctermbg=none ctermfg=black
hi EasyMotionIncSearch ctermbg=NONE ctermfg=black
