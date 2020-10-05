function Plugins()
        call plug#begin('~/.vim/plugged')
        Plug 'MaxMEllon/vim-jsx-pretty'
        Plug 'jaredgorski/spacecamp'
        call plug#end()
endfunction
function Options()
        set number relativenumber
        syntax on 
        filetype plugin indent on
        set expandtab
        colorscheme spacecamp
endfunction
function Keys()
        let g:mapleader= " "
        nnoremap <leader>b :buffers<CR>:buffer<Space>
        nnoremap <leader>e :e<Space><C-D>
        nnoremap <leader>s :source ~/.vimrc <CR>
        nnoremap <leader>w :w! <CR>
        nnoremap <leader>q :q! <CR>
        nnoremap <leader>v :vsp <CR>
        nnoremap <leader>h :sp <CR>
        nnoremap <C-j> <C-w><DOWN>
        nnoremap <C-k> <C-w><UP>
        nnoremap <C-h> <C-w><LEFT>
        nnoremap <C-l> <C-w><RIGHT>
        nnoremap <leader>- <C-w>-
        nnoremap <leader>= <C-w>+
endfunction
function Autocmds()
        augroup remember_folds
                autocmd!
                autocmd BufWinLeave * mkview
                autocmd BufWinEnter * silent! loadview
        augroup END
        autocmd InsertEnter,InsertLeave * set cul!
endfunction

call Plugins()
call Options()
call Keys()
call Autocmds()


