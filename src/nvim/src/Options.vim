set number relativenumber
syntax on 
filetype plugin indent on

colorscheme sonokai
"remaps default tabs to CTRL-V<Tab>, and $tabstop spaces to tabs.
set tabstop=2
set expandtab

set shiftwidth=2
set hidden
set clipboard+=unnamedplus
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set shortmess+=c
setlocal formatprg =hindent

autocmd InsertEnter,InsertLeave * set cul!

