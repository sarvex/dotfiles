set nocompatible              " be iMproved, required
filetype off                  " required

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'Blackrush/vim-gocode'
Plug 'brookhong/cscope.vim'
Plug 'chrisbra/NrrwRgn'
Plug 'christoomey/vim-tmux-navigator'
Plug 'conradirwin/vim-bracketed-paste'
Plug 'dart-lang/dart-vim-plugin'
Plug 'dense-analysis/ale'
Plug 'ecomba/vim-ruby-refactoring'
Plug 'ekalinin/Dockerfile.vim'
Plug 'elixir-lang/vim-elixir'
Plug 'elzr/vim-json'
Plug 'ervandew/supertab'
Plug 'fatih/vim-go'
Plug 'frazrepo/vim-rainbow'
Plug 'godlygeek/tabular'
Plug 'honza/vim-snippets'
Plug 'hsitz/VimOrganizer'
Plug 'itchyny/lightline.vim'
Plug 'janko-m/vim-test'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'jimenezrick/vimerl'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'josemarluedke/vim-rspec'
Plug 'junegunn/fzf'
Plug 'junegunn/gv.vim'
Plug 'kana/vim-textobj-user'
Plug 'kchmck/vim-coffee-script'
Plug 'kien/ctrlp.vim'
Plug 'leafgarland/typescript-vim'
Plug 'Lokaltog/vim-easymotion'
Plug 'majutsushi/tagbar'
Plug 'mattn/calendar-vim'
Plug 'mattn/emmet-vim'
Plug 'mattn/gist-vim'
Plug 'mattn/webapi-vim'
Plug 'mbbill/undotree'
Plug 'mhinz/vim-signify'
Plug 'michaeljsmith/vim-indent-object'
Plug 'mileszs/apidock.vim'
Plug 'mxw/vim-jsx'
Plug 'nelstrom/vim-textobj-rubyblock'
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript'
Plug 'plasticboy/vim-markdown'
Plug 'raimondi/delimitMate'
Plug 'Rigellute/shades-of-purple.vim'
Plug 'rking/ag.vim'
Plug 'roxma/nvim-yarp'
Plug 'roxma/vim-hug-neovim-rpc'
Plug 'rust-lang/rust.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/vim-space'
Plug 'sheerun/vim-polyglot'
Plug 'Shougo/defx.nvim'
Plug 'Shougo/denite.nvim'
Plug 'Shougo/deoplete.nvim'
Plug 'Shougo/vimproc'
Plug 'sjl/gundo.vim'
Plug 'sjl/splice.vim'
Plug 'slim-template/vim-slim'
Plug 'tbodt/deoplete-tabnine', { 'do': './install.sh' }
Plug 'terryma/vim-multiple-cursors'
Plug 'thosakwe/vim-flutter'
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-cucumber'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-haml'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'tsaleh/vim-align'
Plug 'tsaleh/vim-tmux'
Plug 'udalov/kotlin-vim'
Plug 'unblevable/quick-scope'
Plug 'valloric/vim-indent-guides'
Plug 'vim-ruby/vim-ruby'
Plug 'vim-scripts/a.vim'
Plug 'vim-scripts/blockle.vim'
Plug 'vim-scripts/c.vim'
Plug 'vim-scripts/FuzzyFinder'
Plug 'vim-scripts/greplace.vim'
Plug 'vim-scripts/Guardian'
Plug 'vim-scripts/jade.vim'
Plug 'vim-scripts/L9'
Plug 'vim-scripts/openssl.vim'
Plug 'vim-scripts/python.vim'
Plug 'vim-scripts/taglist.vim'
Plug 'vim-scripts/utl.vim'
Plug 'vim-scripts/YankRing.vim'
Plug 'vim-syntastic/syntastic'
Plug 'vimwiki/vimwiki'
Plug 'vitaly/vim-gitignore'
Plug 'vitaly/vim-syntastic-coffee'
Plug 'wakatime/vim-wakatime'
Plug 'wavded/vim-stylus'
Plug 'wincent/command-t'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-notes'
Plug 'xuyuanp/nerdtree-git-plugin'
Plug 'yuttie/comfortable-motion.vim'
Plug 'altercation/vim-colors-solarized'
Plug 'kristijanhusak/vim-hybrid-material'
Plug 'Lokaltog/vim-distinguished'
Plug 'nanotech/jellybeans.vim'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'tpope/vim-vividchalk'

call plug#end()

filetype plugin indent on

hi SpellErrors guibg=red guifg=black ctermbg=red ctermfg=black
set wildignore=.svn,CVS,.git,.hg,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif
set autowriteall        " Automatically save before commands like :next and :make
set hidden              " enable multiple modified buffers
set history=1000
set autoread            " automatically read file that has been changed on disk and doesn't have changes in vim
set guioptions-=m       " disable toolbar"
set guioptions-=T       " disable toolbar"
set completeopt=menuone,preview

nnoremap <C-F1> :if &go=~#'m'<Bar>set go-=m<Bar>else<Bar>set go+=m<Bar>endif<CR>
nnoremap <C-F2> :if &go=~#'T'<Bar>set go-=T<Bar>else<Bar>set go+=T<Bar>endif<CR>
nnoremap <C-F3> :if &go=~#'r'<Bar>set go-=r<Bar>else<Bar>set go+=r<Bar>endif<CR>

let g:nerdtree_tabs_open_on_gui_startup = 0
let g:nerdtree_tabs_open_on_console_startup = 0
let g:nerdtree_tabs_no_startup_for_diff = 0

set ofu=syntaxcomplete#Complete
let g:rubycomplete_buffer_loading = 0
let g:rubycomplete_classes_in_global = 1

" showmarks
let g:showmarks_enable = 0 " disabled by default by populardemand ;)
hi! link ShowMarksHLl LineNr
hi! link ShowMarksHLu LineNr
hi! link ShowMarksHLo LineNr
hi! link ShowMarksHLm LineNr

" syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 2
let g:syntastic_enable_signs = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_error_symbol = '✗'
let g:syntastic_warning_symbol = '⚠'

" delimitMate
let g:delimitMate_expand_space = 1 " Turns on/off the expansion of <Space>
let g:delimitMate_expand_cr = 1    " Turns on/off the expansion of <CR>

" nerdtree
" Ctrl-P to Display the file browser tree
nmap <C-D> :NERDTreeTabsToggle<CR>
" ,p to show current file in the tree
nmap <leader>p :NERDTreeFind<CR>

" nerdcommenter
" ,/ to invert comment on the current line/selection
nmap <leader>/ :call NERDComment(0, "invert")<cr>
vmap <leader>/ :call NERDComment(0, "invert")<cr>

" ,t to show tags window
let Tlist_Show_Menu=1
nmap <leader>t :TlistToggle<CR>

" sessionman
nmap <leader>S :SessionList<CR>
nmap <leader>SS :SessionSave<CR>
nmap <leader>SA :SessionSaveAs<CR>

let g:Conque_Read_Timeout = 50 " timeout for waiting for command output.
let g:Conque_TERM = 'xterm'
" ,sh shell window
nmap <Leader>sh :ConqueSplit bash<cr>
" ,r run command
nmap <Leader>R :ConqueSplit

" yankring
let g:yankring_replace_n_pkey = '<leader>['
let g:yankring_replace_n_nkey = '<leader>]'
" ,y to show the yankring
nmap <leader>y :YRShow<cr>

" rails
" completing Rails hangs a lot
"let g:rubycomplete_rails = 1

" command-t
"nmap <unique> <silent> <Leader>, :CommandT<CR>
"nmap <unique> <silent> <Leader>. :CommandTFlush<CR>:CommandT<CR>
"let g:CommandTMatchWindowAtTop=1

let g:ctrlp_map = '<leader>,'
let g:ctrlp_cmd = 'CtrlP'

nmap <leader>. :CtrlPClearCache<cr>:CtrlP<cr>
nmap <leader>l :CtrlPLine<cr>
nmap <leader>b :CtrlPBuff<cr>
nmap <leader>m :CtrlPBufTag<cr>
nmap <leader>M :CtrlPBufTagAll<cr>

let g:ctrlp_clear_cache_on_exit = 1
" ctrlp leaves stale caches behind if there is another vim process runnin
" which didn't use ctrlp. so we clear all caches on each new vim invocation
cal ctrlp#clra()

let g:ctrlp_max_height = 40

" show on top
"let g:ctrlp_match_window_bottom = 0
"let g:ctrlp_match_window_reversed = 0

" jump to buffer in the same tab if already open
let g:ctrlp_switch_buffer = 1

" if in git repo - use git file listing command, should be faster
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files --exclude-standard -cod']

" open multiple files with <c-z> to mark and <c-o> to open. v - opening in
" vertical splits; j - jump to first open buffer; r - open first in current buffer
let g:ctrlp_open_multiple_files = 'vjr'

let g:ctrlp_extensions = ['tag', 'buffertag', 'quickfix', 'mixed', 'line']

" Fugitive
" ,g for Ggrep
nmap <leader>g :silent Ggrep<space>

" ,f for global git serach for word under the cursor (with highlight)
nmap <leader>f :let @/="\\<<C-R><C-W>\\>"<CR>:set hls<CR>:silent Ggrep -w "<C-R><C-W>"<CR>:ccl<CR>:cw<CR><CR>

" same in visual mode
:vmap <leader>f y:let @/=escape(@", '\\[]$^*.')<CR>:set hls<CR>:silent Ggrep -F "<C-R>=escape(@", '\\"#')<CR>"<CR>:ccl<CR>:cw<CR><CR>
" Ag
" ,a for Ag
nmap <leader>k :Ag<space>

" vim-indentobject
" add Markdown to the list of indentation based languages
let g:indentobject_meaningful_indentation = ["haml", "sass", "python", "yaml", "markdown"]

" indent-guides
let g:indent_guides_start_level = 2
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_guide_size = 1
let g:indent_guides_color_change_percent = 5

" Utl.vim
if has("mac")
  let g:utl_cfg_hdl_scm_http_system = "!open '%u'"
end
nmap <leader>o :Utl

" VimOrganizer
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org call org#SetOrgFileType()

" Gundo
nmap <leader>u :GundoToggle<CR>
let g:gundo_close_on_revert = 1

" Switch
" making some of the switches defined for ruby work in HAML files
autocmd FileType haml let b:switch_definitions =
      \ [
      \   g:switch_builtins.ruby_hash_style,
      \   g:switch_builtins.ruby_string,
      \   g:switch_builtins.true_false,
      \   g:switch_builtins.true_false,
      \ ]

let g:blockle_mapping = '<Leader>B'

" vim-dispatch
autocmd FileType ruby let b:dispatch = 'rspec %'

" vim-rspec
map <Leader>r :call RunNearestSpec()<CR>
" Fast saving
nmap <leader>w :w!<cr>

augroup BWCCreateDir
  autocmd!
  autocmd BufWritePre * if expand("<afile>")!~#'^\w\+:/' && !isdirectory(expand("%:h")) | execute "silent! !mkdir -p ".shellescape(expand('%:h'), 1) | redraw! | endif
augroup END

set autoindent
set clipboard=unnamedplus
set cursorline
set encoding=utf-8
set expandtab
set fileformats=unix,dos
set foldlevelstart=10
set foldmethod=syntax
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set listchars=tab:▷⋅,trail:·
set macligatures
set magic
set nobackup
set noswapfile
set nowrap
set nowritebackup
set number relativenumber
set numberwidth=3
set ruler
set shiftwidth=2
set showcmd
set showmatch
set smartcase
set smartindent
set softtabstop=2
set splitright
set textwidth=0
set wildmenu
set wrapscan

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

if (has("termguicolors"))
  set termguicolors
endif

let g:enable_bold_font = 1
set background=dark
colorscheme shades_of_purple
let g:shades_of_purple_lightline = 1
set guifont=NotoMonoForPowerline:h13
