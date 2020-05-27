"=================PLUGIN STUFF===================
call plug#begin('~/.vim/autoload')

Plug 'Valloric/YouCompleteMe'
Plug 'flazz/vim-colorschemes'
Plug 'lervag/vimtex'
Plug 'mboughaba/i3config.vim'
Plug 'neoclide/coc.nvim', {'branch' : 'release'}
Plug 'neovimhaskell/haskell-vim'
Plug 'yuezk/vim-js'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'maxmellon/vim-jsx-pretty'
call plug#end()
"=====================SETTINGS=======================
""Clean up clutter in home dir.
set swapfile

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

"===================MAPPINGS====================

"escape, delete line, go back into insert mode
inoremap <c-d> <esc>ddi

"escape, visually select word, uppercase it.
inoremap <c-u> <esc>viwU

"visually select a word, and uppercase it.
nnoremap <c-u> viwU

"in insert mode, press jk to exit to normal mode.
inoremap asd <esc><right>
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

"yank to system clipboard
nnoremap <c-c> "+y
vnoremap <c-c> "+y

nnoremap <c-p> "+p
vnoremap <c-c> "+y

"copy to @q by pressing Q in normal mode.
noremap Q @q

autocmd BufReadPost,FileReadPost,BufNewFile * call system("tmux rename-window " . expand("%"))

"either on creating new i3 config file or reading existing file in, set
"its filetype to i3config 
aug i3config_ft_detection
    au!
    au BufNewFile,BufRead ~/.config/i3/config set filetype=i3config
aug end

"same as i3config_ft_detection explanation, except for polybar config file.
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
    au Filetype c noremap <buffer> <leader>c I//<esc>
aug end

"auto-close parentheses,brackets(not yet done), and so on...
aug auto_close
    au!
    au Filetype scheme inoremap ( ()<left>
aug end

"inoreabbrev stands for insert mode, no recursively defined mappings,
"abbreviation. 
aug snippets
    au!
    "generate (cond ()) and move the cursor two places left.
    au FileType scheme :inoreabbrev <buffer> condd (cond ())<left><left>
    "generate (if ()) and move cursor two places left.
    au FileType scheme :inoreabbrev <buffer> iff (if ())<left><left>
aug end

" Run xrdb whenever Xdefaults or Xresources are updated.
aug xrdb_refresh
    au!
    au BufWritePost *Xresources,*Xdefaults !xrdb %
aug end


"make cursor same as in zsh shell.
let &t_SI = "\e[6 q" "steady bar
let &t_EI = "\e[2 q" "steady block


augroup handle_cursor
    au!
    autocmd VimEnter * silent !echo -ne "\e[2 q"
augroup END
