
"=============PLUGIN STUFF=================

call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'Valloric/YouCompleteMe'
Plug 'flazz/vim-colorschemes'
Plug 'lervag/vimtex'
Plug 'mboughaba/i3config.vim'
call plug#end()

"=============SETTINGS=================

set nocompatible

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

"escape, delete line, go back into insert mode
inoremap <c-d> <esc>ddi

"escape, visually select word, uppercase it.
inoremap <c-u> <esc>viwU

"visually select a word, and uppercase it.
nnoremap <c-u> viwU

inoremap jk <esc>


"maps the leader key to space.
let mapleader = ' '
nnoremap <leader><leader> :noh<cr>
nnoremap <leader>b :buffers<cr>

"opens vimrc file in v split
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

"sources vimrc file
nnoremap <leader>sv :source $MYVIMRC<cr>


"Note to self: nnore stands for normal more, no recursion,
nnoremap <leader>w <C-w>v<C-w>l
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
nnoremap <leader>h <C-w>H
nnoremap <leader>j <C-w>J
nnoremap <leader>k <C-w>K
nnoremap <leader>l <C-w>L
nnoremap <leader>< <C-w><


"enable loading the plugin files for specific file types
filetype plugin on

"turn syntax highlighting on 
syntax on

"syntax of autocmd:
"autocmd <event> <pattern> <action>

"if you enter vim, with any file, you run nerd tree
autocmd vimenter * NERDTree

colorscheme afterglow

"note: au stands for auto command and aug stands for auto command group
aug i3config_ft_detection
  au!
  au BufNewFile,BufRead ~/.config/i3/config set filetype=i3config
aug end

aug polybarconfig_ft_detection
  au!
  au BufNewFile,BufRead ~/.config/polybar/config set filetype=dosini
aug end


"handling comments by filetype
autocmd FileType vim noremap <buffer> <leader>c I"<esc>
autocmd Filetype scheme noremap <buffer> <leader>c I;;<esc>
autocmd Filetype sh noremap <buffer> <leader>c I#<esc>

"auto-close parentheses
autocmd Filetype scheme inoremap ( ()<left>

"snippets
autocmd FileType scheme :inoreabbrev <buffer> condd (cond ())<left><left>
autocmd FileType scheme :inoreabbrev <buffer> iff (if ())<left><left>



" Run xrdb whenever Xdefaults or Xresources are updated.
autocmd BufWritePost *Xresources,*Xdefaults !xrdb %

