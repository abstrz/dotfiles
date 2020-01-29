"=================PLUGIN STUFF===================
call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'Valloric/YouCompleteMe'
Plug 'flazz/vim-colorschemes'
Plug 'lervag/vimtex'
Plug 'mboughaba/i3config.vim'
call plug#end()
"==================SETTINGS====================

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

"enable loading the plugin files for specific file types
filetype plugin on
"turn syntax highlighting on 
syntax on
colorscheme afterglow


"===================MAPPINGS====================

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

"Note to self: nnore stands for normal mode, no recursion,
nnoremap <leader>v <C-w>v<C-w>l
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
nnoremap <leader>h <C-w>H
nnoremap <leader>j <C-w>J
nnoremap <leader>k <C-w>K
nnoremap <leader>l <C-w>L
nnoremap <leader>w :w<cr>
nnoremap <leader>q :q<cr>

"o stands for operator pending (a motion). 
"It behaves like: 
"1. start at cursor position, 
"2. enter visual mode
"3. do command
"Thus in the following, i( behave as it would in visual mode,
"where it selects all text inside parens.
onoremap p i(
"find next parenthesis, select all text inside.
onoremap in( :<c-u>normal! f(vi(<cr>
"find last parenthesis, select all text inside.
onoremap il( :<c-u>normal! F(vi(<cr>

"Note If operator-pending mapping ends with some text visually selected, Vim
"       will operate on that text
"     Otherwise, Vim will operate on text between original cursor position and
"       new one. 

"===================Auto Commands====================
"notes: autocmd syntax: autocmd <event> <pattern> <action>
"       au stands for auto command and aug stands for auto command group
"       au! clears clears group

"if you enter vim, with any file, you run nerd tree
aug NERDTree_init
    au!
    au vimenter * NERDTree
aug end

aug i3config_ft_detection
  au!
  au BufNewFile,BufRead ~/.config/i3/config set filetype=i3config
aug end

aug polybarconfig_ft_detection
  au!
  au BufNewFile,BufRead ~/.config/polybar/config set filetype=dosini
aug end

"handling comments by filetype
aug comment_binding
    au!
    au FileType vim noremap <buffer> <leader>c I"<esc>
    au Filetype scheme noremap <buffer> <leader>c I;;<esc>
    au Filetype sh noremap <buffer> <leader>c I#<esc>
aug end

"auto-close parentheses,brackets(not yet done), and so on...
aug auto_close
    au!
    au Filetype scheme inoremap ( ()<left>
aug end

aug snippets
    au!
    au FileType scheme :inoreabbrev <buffer> condd (cond ())<left><left>
    au FileType scheme :inoreabbrev <buffer> iff (if ())<left><left>
aug end

" Run xrdb whenever Xdefaults or Xresources are updated.
aug xrdb_refresh
    au!
    au BufWritePost *Xresources,*Xdefaults !xrdb %
aug end

