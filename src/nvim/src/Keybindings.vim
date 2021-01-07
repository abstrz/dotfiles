nnoremap <leader>b :buffers<CR>:buffer<Space>
nnoremap <leader>e :NERDTreeToggle <CR>
nnoremap <leader>s :source ~/.config/nvim/init.vim <CR>
nnoremap <leader>w :w! <CR>
nnoremap <leader>q :q! <CR>
nnoremap <leader>v :vsp <CR>
nnoremap <leader>o :e ~/.config/nvim/init.vim <CR>
nnoremap <C-j> <C-w><DOWN>
nnoremap <C-k> <C-w><UP>
nnoremap <C-h> <C-w><LEFT>
nnoremap <C-l> <C-w><RIGHT>
nnoremap <leader>l :bnext<CR>
nnoremap <leader>h :bprev<CR>
nnoremap <leader>c :bd! <CR>
nnoremap <leader>1 :b 1 <CR>
nnoremap <leader>2 :b 2 <CR>
nnoremap <leader>3 :b 3 <CR>
nnoremap <leader>4 :b 4 <CR>
nnoremap <leader>5 :b 5 <CR>
nnoremap <leader>6 :b 6 <CR>
imap asd <Esc>ll
" Use tab to trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"


