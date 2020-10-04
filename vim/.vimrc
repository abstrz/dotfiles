call plug#begin('~/.vim/plugged')

Plug 'MaxMEllon/vim-jsx-pretty'
Plug 'jaredgorski/spacecamp'

call plug#end()

set number relativenumber
syntax on 
filetype plugin indent on
set expandtab

augroup remember_folds
        autocmd!
        autocmd BufWinLeave * mkview
        autocmd BufWinEnter * silent! loadview
augroup END


let mapleader= " "
nnoremap <leader>b :buffers<CR>:buffer<Space>
nnoremap <leader>e :e<Space><C-D>
nnoremap <leader>s :source ~/.vimrc <CR>
nnoremap <leader>w :w! <CR>
nnoremap <leader>q :q! <CR>

colorscheme spacecamp

autocmd InsertEnter,InsertLeave * set cul!
