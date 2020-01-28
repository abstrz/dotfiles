
"=============PLUGIN STUFF=================

call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'Valloric/YouCompleteMe'
Plug 'flazz/vim-colorschemes'
Plug 'lervag/vimtex'
Plug 'mboughaba/i3config.vim'
call plug#end()

"=============SETTINGS=================

set nocompatible

"sets number of lines vim checks for variable initializations. 
set modelines=0

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent

set encoding=utf-8
set scrolloff=3
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set cursorline
set ttyfast
set ruler
set backspace=indent,eol,start
set laststatus=2

"setting number and relativenumber gives us hybrid numbering
set number
set relativenumber

set undofile	
set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch

set wrap
set textwidth=79
set formatoptions=qrn1
set modifiable 



"=============MAPPINGS================
nmap - ddp
nmap _ ddkP
"maps the leader key to space.
let mapleader = ' '
nnoremap <leader><leader> :noh<cr>
nnoremap <leader>b :buffers<cr>
nnoremap <leader>1 :b1<cr>
nnoremap <leader>2 :b2<cr>
nnoremap <leader>3 :b3<cr>
nnoremap <leader>4 :b4<cr>
nnoremap <leader>5 :b5<cr>
nnoremap <leader>6 :b6<cr>
nnoremap <leader>7 :b7<cr>
nnoremap <leader>8 :b8<cr>
nnoremap <leader>9 :b9<cr>





"nnoremap <tab> %
"vnoremap <tab> %

"make duplication of buffer, and move to it.
nnoremap <leader>w <C-w>v<C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <leader>h <C-w>H
nnoremap <leader>j <C-w>J
nnoremap <leader>k <C-w>K
nnoremap <leader>l <C-w>L
nnoremap <leader>< <C-w><
nnoremap <leader>> <C-w>>
filetype plugin on
syntax on
autocmd vimenter * NERDTree
colorscheme VIvid
autocmd InsertEnter,InsertLeave * set cul!

aug i3config_ft_detection
  au!
  au BufNewFile,BufRead ~/.config/i3/config set filetype=i3config
aug end

aug polybarconfig_ft_detection
  au!
  au BufNewFile,BufRead ~/.config/polybar/config set filetype=dosini
aug end

" Run xrdb whenever Xdefaults or Xresources are updated.

autocmd BufWritePost *Xresources,*Xdefaults !xrdb %
