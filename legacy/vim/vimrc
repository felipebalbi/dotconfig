runtime! debian.vim

syntax on
set background=dark

" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
if has("autocmd")
  filetype plugin indent on
  augroup ruby
    au BufReadPre,FileReadPre set kp=ri sw=2 ts=2 expandtab
  augroup END
endif

map <F12> o<CR>Acked-by: Felipe Balbi <balbi@ti.com><CR><Esc>
map <F11> o<CR>Reviewed-by: Felipe Balbi <balbi@ti.com><CR><Esc>
map <F10> o<CR>Tested-by: Felipe Balbi <balbi@ti.com><CR><Esc>
map <F9> o<CR>Signed-of-by: Felipe Balbi <balbi@ti.com><CR><Esc>

set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.
"set ignorecase		" Do case insensitive matching
set smartcase		" Do smart case matching
set incsearch		" Incremental search
set hlsearch		" Highlight matches
"set autowrite		" Automatically save before commands like :next and :make
"set hidden             " Hide buffers when they are abandoned
set mouse=a		" Enable mouse usage (all modes)
"set encoding=utf-8	" UTF-8
"set fileencodings=utf-8	" UTF-8
set number

" Highlight redundant whitespaces
highlight RedundantWhitespace ctermbg=red guibg=red
match RedundantWhitespace /\s\+$\| \+\ze\t/

" Source a global configuration file if available
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif

