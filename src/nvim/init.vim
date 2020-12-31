function Plugins()
  call plug#begin('~/.vim/plugged')
  Plug 'MaxMEllon/vim-jsx-pretty'
  Plug 'jceb/vim-orgmode'
  Plug 'tpope/vim-fugitive'
  Plug 'mg979/vim-visual-multi', {'branch': 'master'}
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  Plug 'mhinz/vim-signify'
  Plug 'neovimhaskell/haskell-vim'
  Plug 'neovimhaskell/nvim-hs'
  Plug 'neovimhaskell/nvim-hs.vim'
  Plug 'dense-analysis/ale'
  Plug 'tpope/vim-surround'
  Plug 'ap/vim-buftabline'
  Plug 'preservim/nerdtree'
  Plug 'JuliaEditorSupport/julia-vim'
  call plug#end()
endfunction
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocActionAsync('doHover')
  endif
endfunction
function Options()
  let &t_ut='' "for kitty 
  set number relativenumber
  syntax on 
  filetype plugin indent on

  colorscheme sonokai
  set tabstop=2
  set shiftwidth=2
  set expandtab
  set hidden
  set clipboard+=unnamedplus
  set nobackup
  set nowritebackup
  set cmdheight=2
  set updatetime=300
  set shortmess+=c
  " Always show the signcolumn, otherwise it would shift the text each time
  "set signcolumn=number
  set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}
  setlocal formatprg =hindent
  let g:nvimhsPluginStarter=nvimhs#stack#pluginstarter()
  let g:ale_linters = {'haskell': ['cabal_ghc', 'ghc-mod', 'hdevtools', 'hie', 'hlint', 'stack_build', 'stack_ghc']}
  let g:mapleader= " "
  let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
  let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
  let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
  let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
  let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
  let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
  let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
  let g:haskell_indent_if = 3 
  let g:haskell_indent_case = 2
  let g:haskell_indent_let = 4
  let g:haskell_indent_where = 6
  let g:haskell_indent_before_where = 2
  let g:haskell_indent_after_bare_where = 2
  let g:haskell_indent_do = 3
  let g:haskell_indent_in = 1
  let g:haskell_indent_guard = 2
  let g:cabal_indent_section = 2

endfunction
function Keys() 
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
  imap sd <Esc>ll
  " Use tab to trigger completion with characters ahead and navigate.
  " NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
  " other plugin before putting this into your config.
  inoremap <silent><expr> <TAB>
        \ pumvisible() ? "\<C-n>" :
        \ <SID>check_back_space() ? "\<TAB>" :
        \ coc#refresh()
  inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

endfunction
function Autocmds()
  autocmd InsertEnter,InsertLeave * set cul!
  " Highlight the symbol and its references when holding the cursor.
  autocmd CursorHold * silent call CocActionAsync('highlight')
  augroup mygroup
    autocmd!
    " Setup formatexpr specified filetype(s).
    autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
    " Update signature help on jump placeholder.
    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
  augroup end
endfunction
function Commands()
  " Add `:Format` command to format current buffer.
  command! -nargs=0 Format :call CocAction('format')

  " Add `:Fold` command to fold current buffer.
  command! -nargs=? Fold :call     CocAction('fold', <f-args>)

  " Add `:OR` command for organize imports of the current buffer.
  command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')
endfunction
function Calls()
  call Plugins()
  call Options()
  call Keys()
  call Autocmds()
endfunction

call Calls()
