let data = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')

  if has('nvim')
    Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  else
    Plug 'Shougo/denite.nvim'
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
  endif
    let g:deoplete#enable_at_startup=1
    autocmd FileType denite call s:denite_my_settings()
    function! s:denite_my_settings() abort
      nnoremap <silent><buffer><expr> <CR>
      \ denite#do_map('do_action')
      nnoremap <silent><buffer><expr> d
      \ denite#do_map('do_action', 'delete')
      nnoremap <silent><buffer><expr> p
      \ denite#do_map('do_action', 'preview')
      nnoremap <silent><buffer><expr> q
      \ denite#do_map('quit')
      nnoremap <silent><buffer><expr> i
      \ denite#do_map('open_filter_buffer')
      nnoremap <silent><buffer><expr> <Space>
      \ denite#do_map('toggle_select').'j'
    endfunction

  if has('win32') || has('win64')
    Plug 'tbodt/deoplete-tabnine', { 'do': 'powershell.exe .\install.ps1' }
  else
    Plug 'tbodt/deoplete-tabnine', { 'do': './install.sh' }
  endif

  Plug 'dense-analysis/ale'
  let g:ale_sign_error = '►'
    let g:ale_sign_warning = '-'
    highlight clear ALEErrorSign
    highlight clear ALEWarningSign
    highlight ALEErrorSign guifg=red
    highlight ALEWarningSign guifg=orange
    nnoremap ge :ALENextWrap<cr>
  Plug 'junegunn/goyo.vim' " distraction free writing in vim
    let g:goyo_width=100
    " let g:goyo_height=50
    let g:goyo_linenr=0
    nnoremap <leader>g :Goyo<CR>
  Plug 'junegunn/limelight.vim' " hyperfocus writing
    nnoremap <leader>l :Limelight!!<CR>
  Plug 'machakann/vim-highlightedyank' " show yanked region
    let g:highlightedyank_highlight_duration = 500
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
    let g:coc_global_extensions = [ 'coc-emoji', 'coc-eslint', 'coc-prettier', 'coc-tsserver', 'coc-tslint', 'coc-tslint-plugin', 'coc-css', 'coc-json', 'coc-pyls', 'coc-yaml' ] 
    " Use tab for trigger completion with characters ahead and navigate.
    inoremap <silent><expr><TAB>  pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : coc#refresh()
    " Use <c-space> to trigger completion.
    inoremap <silent><expr> <c-space> coc#refresh()

    function! s:check_back_space() abort
      let col = col('.') - 1
      return !col || getline('.')[col - 1] =~# '\s'
    endfunction

    " Use `lp` and `ln` for navigate diagnostics
    nmap <silent> <leader>lp <Plug>(coc-diagnostic-prev)
    nmap <silent> <leader>ln <Plug>(coc-diagnostic-next)

    " Remap keys for gotos
    nmap <silent> <leader>ld <Plug>(coc-definition)
    nmap <silent> <leader>lt <Plug>(coc-type-definition)
    nmap <silent> <leader>li <Plug>(coc-implementation)
    nmap <silent> <leader>lf <Plug>(coc-references)

    " Remap for rename current word
    nmap <leader>lr <Plug>(coc-rename)

    " Use K for show documentation in preview window
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    function! s:show_documentation()
      if &filetype == 'vim'
        execute 'h '.expand('<cword>')
      else
        call CocAction('doHover')
      endif
    endfunction
    " Highlight symbol under cursor on CursorHold
    autocmd CursorHold * silent call CocActionAsync('highlight')
  Plug 'semanser/vim-outdated-plugins'
    let g:outdated_plugins_silent_mode = 1
  Plug 'vim-airline/vim-airline-themes'
    let g:airline_powerline_fonts=1
    let g:airline#extensions#tabline#enabled = 1

  Plug 'airblade/vim-gitgutter'
  Plug 'airblade/vim-rooter'
  Plug 'ap/vim-css-color', {'for': ['css', 'scss', 'conf', 'javascript']}
  Plug 'arithran/vim-delete-hidden-buffers'
  Plug 'Blackrush/vim-gocode'
  Plug 'bogado/file-line'
  Plug 'brookhong/cscope.vim'
  Plug 'Carpetsmoker/undofile_warn.vim'
  Plug 'chrisbra/nrrwrgn'
  Plug 'christoomey/vim-sort-motion'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'conradirwin/vim-bracketed-paste'
  Plug 'ctrlpvim/ctrlp.vim'
  Plug 'dart-lang/dart-vim-plugin'
  Plug 'davidhalter/jedi-vim'
  Plug 'dstein64/vim-startuptime'
  Plug 'easymotion/vim-easymotion'
  Plug 'ecomba/vim-ruby-refactoring'
  Plug 'editorconfig/editorconfig-vim'
  Plug 'ekalinin/Dockerfile.vim'
  Plug 'elixir-editors/vim-elixir'
  Plug 'elzr/vim-json'
  Plug 'ervandew/supertab'
  Plug 'farmergreg/vim-lastplace'
  Plug 'fatih/vim-go'
  Plug 'godlygeek/tabular'
  Plug 'gorodinskiy/vim-coloresque'
  Plug 'honza/vim-snippets'
  Plug 'hsitz/VimOrganizer'
  Plug 'janko-m/vim-test'
  Plug 'jeetsukumaran/vim-buffergator'
  Plug 'jiangmiao/auto-pairs'
  Plug 'jimenezrick/vimerl'
  Plug 'jistr/vim-nerdtree-tabs'
  Plug 'josemarluedke/vim-rspec'
  Plug 'jparise/vim-graphql', { 'for': 'graphql' }
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/goyo.vim'
  Plug 'junegunn/limelight.vim'
  Plug 'junegunn/gv.vim'
  Plug 'junegunn/vim-easy-align'
  Plug 'kana/vim-textobj-user'
  Plug 'kchmck/vim-coffee-script'
  Plug 'Konfekt/FastFold'
  Plug 'kristijanhusak/defx-icons'
  Plug 'kristijanhusak/vim-hybrid-material'
  Plug 'leafgarland/typescript-vim'
  Plug 'Lokaltog/vim-distinguished'
  Plug 'luochen1990/rainbow'
  Plug 'machakann/vim-highlightedyank'
  Plug 'machakann/vim-sandwich'
  Plug 'majutsushi/tagbar'
  Plug 'mattn/calendar-vim'
  Plug 'mattn/emmet-vim'
  Plug 'mattn/gist-vim'
  Plug 'mattn/webapi-vim'
  Plug 'MaxMEllon/vim-jsx-pretty', { 'for': 'javascript' }
  Plug 'mbbill/undotree'
  Plug 'mechatroner/rainbow_csv'
  Plug 'mhinz/vim-signify'
  Plug 'mhinz/vim-startify'
  Plug 'michaeljsmith/vim-indent-object'
  Plug 'mileszs/apidock.vim'
  Plug 'moll/vim-bbye'
  Plug 'morhetz/gruvbox'
  Plug 'mxw/vim-jsx'
  Plug 'nanotech/jellybeans.vim'
  Plug 'nelstrom/vim-textobj-rubyblock'
  Plug 'neoclide/vim-jsx-improve', { 'for': 'javascript' }
  Plug 'neomake/neomake'
  Plug 'octol/vim-cpp-enhanced-highlight'
  Plug 'octref/RootIgnore'
  Plug 'othree/html5.vim'
  Plug 'pangloss/vim-javascript'
  Plug 'plasticboy/vim-markdown'
  Plug 'radenling/vim-dispatch-neovim'
  Plug 'rafi/awesome-vim-colorschemes'
  Plug 'raimondi/delimitMate'
  Plug 'Rigellute/shades-of-purple.vim'
  Plug 'rking/ag.vim'
  Plug 'rrethy/vim-illuminate'
  Plug 'rust-lang/rust.vim'
  Plug 'ryanoasis/vim-devicons'
  Plug 'sbdchd/neoformat'
  Plug 'scrooloose/nerdcommenter'
  Plug 'scrooloose/nerdtree'
  Plug 'scrooloose/syntastic'
  Plug 'scrooloose/vim-space'
  Plug 'sgeb/vim-diff-fold'
  Plug 'sheerun/vim-polyglot'
  Plug 'Shougo/neosnippet-snippets'
  Plug 'Shougo/neosnippet.vim'
  Plug 'Shougo/vimproc'
  Plug 'simnalamburt/vim-mundo', { 'on': 'MundoToggle' }
  Plug 'SirVer/ultisnips'
  Plug 'farmergreg/vim-lastplace'
  Plug 'suan/vim-instant-markdown', { 'for': 'markdown' }
  Plug 'tbastos/vim-lua', { 'for': 'lua' }
  Plug 'terryma/vim-multiple-cursors'
  Plug 'thosakwe/vim-flutter'
  Plug 'tommcdo/vim-exchange'
  Plug 'tpope/vim-abolish'
  Plug 'tpope/vim-bundler'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-cucumber', {'for': 'cucumber'}
  Plug 'tpope/vim-dispatch'
  Plug 'tpope/vim-dotenv'
  Plug 'tpope/vim-endwise'
  Plug 'tpope/vim-eunuch'
  Plug 'tpope/vim-fireplace'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-git'
  Plug 'tpope/vim-haml'
  Plug 'tpope/vim-markdown'
  Plug 'tpope/vim-obsession'
  Plug 'tpope/vim-ragtag'
  Plug 'tpope/vim-rails', {'for': ['ruby', 'cucumber']}
  Plug 'tpope/vim-repeat'
  Plug 'tpope/vim-rhubarb'
  Plug 'tpope/vim-scriptease'
  Plug 'tpope/vim-sleuth'
  Plug 'tpope/vim-speeddating'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-unimpaired'
  Plug 'tpope/vim-vinegar'
  Plug 'tpope/vim-vividchalk'
  Plug 'tsaleh/vim-tmux'
  Plug 'udalov/kotlin-vim'
  Plug 'unblevable/quick-scope'
  Plug 'valloric/vim-indent-guides'
  Plug 'vimwiki/vimwiki'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-ruby/vim-ruby'
  Plug 'vim-scripts/a.vim'
  Plug 'vim-scripts/blockle.vim'
  Plug 'vim-scripts/c.vim'
  Plug 'vim-scripts/FuzzyFinder'
  Plug 'vim-scripts/greplace.vim'
  Plug 'vim-scripts/Guardian'
  Plug 'vim-scripts/jade.vim'
  Plug 'vim-scripts/L9'
  Plug 'vim-scripts/mru'
  Plug 'vim-scripts/openssl.vim'
  Plug 'vim-scripts/python.vim'
  Plug 'vim-scripts/searchfold.vim'
  Plug 'vim-scripts/taglist.vim'
  Plug 'vim-scripts/utl.vim'
  Plug 'vim-scripts/YankRing.vim'
  Plug 'vitaly/vim-gitignore'
  Plug 'vitaly/vim-syntastic-coffee'
  Plug 'wakatime/vim-wakatime'
  Plug 'wavded/vim-stylus'
  Plug 'wincent/command-t'
  Plug 'xuyuanp/nerdtree-git-plugin'
  Plug 'yardnsm/vim-import-cost', { 'do': 'npm install', 'for': 'javascript' }
  Plug 'Yggdroot/leaderf'
  Plug 'Yggdroot/indentline'
  Plug 'yuttie/comfortable-motion.vim'
call plug#end()

if (has("termguicolors"))
 set termguicolors
endif

filetype plugin indent on
syntax enable

set autoindent
set clipboard=unnamedplus
set cursorline
set colorcolumn=121
set encoding=utf-8
set expandtab
set fileformats=unix,dos
set foldlevelstart=10
set foldmethod=syntax
set guifont=MesloLGS_NF
set hidden
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set list
set listchars=tab:‣\ ,trail:•,precedes:«,extends:»,eol:¬
set magic
set mouse=a
set nobackup
set noswapfile
set nowrap
set nowritebackup
set number relativenumber
set numberwidth=3
set path+=**
set ruler
set shiftwidth=2
set shortmess+=c
set showcmd
set showmatch
set signcolumn=yes
set smartcase
set smartindent
set softtabstop=2
set spelllang=en
set spell
set splitright
set textwidth=0
set undofile
set undodir=~/.vim/undo
set updatetime=300
set wildmenu
set wrapscan

let &t_8f="\<Esc>[38;2;%lu;%lu;%lum" " enable italcs
let &t_8b="\<Esc>[48;2;%lu;%lu;%lum" " enable italics

augroup mygroup
  autocmd!
  autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
augroup end
