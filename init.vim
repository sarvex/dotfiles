set runtimepath+=~/.config/nvim/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.config/nvim/dein')
  call dein#begin('~/.config/nvim/dein')

  call dein#add('~/.config/nvim/dein/repos/github.com/Shougo/dein.vim')

  call dein#add('Shougo/defx.nvim')
  call dein#add('Shougo/denite.nvim')
  call dein#add('Shougo/deol.nvim')
  call dein#add('Shougo/deoplete.nvim')
  call dein#add('Shougo/deoppet.nvim')
  call dein#add('Shougo/deorise.nvim')
  call dein#add('Shougo/neosnippet-snippets')
  call dein#add('Shougo/neosnippet.vim')

  call dein#add('deoplete-plugins/deoplete-go', {'build': 'make'})
  call dein#add('deoplete-plugins/deoplete-jedi')
  call dein#add('zchee/deoplete-clang')

  call dein#add('airblade/vim-gitgutter')
  call dein#add('airblade/vim-rooter')
  call dein#add('ap/vim-css-color', {'on_ft': ['css', 'scss', 'conf', 'javascript']})
  call dein#add('arithran/vim-delete-hidden-buffers')
  call dein#add('bogado/file-line')
  call dein#add('ctrlpvim/ctrlp.vim')
  call dein#add('dart-lang/dart-vim-plugin')
  call dein#add('davidhalter/jedi-vim')
  call dein#add('dense-analysis/ale')
  call dein#add('easymotion/vim-easymotion')
  call dein#add('editorconfig/editorconfig-vim')
  call dein#add('ervandew/supertab')
  call dein#add('fatih/vim-go')
  call dein#add('godlygeek/tabular')
  call dein#add('gorodinskiy/vim-coloresque')
  call dein#add('honza/vim-snippets')
  call dein#add('jistr/vim-nerdtree-tabs')
  call dein#add('junegunn/fzf')
  call dein#add('junegunn/vim-easy-align')
  call dein#add('kchmck/vim-coffee-script')
  call dein#add('Konfekt/FastFold')
  call dein#add('luochen1990/rainbow')
  call dein#add('machakann/vim-highlightedyank')
  call dein#add('majutsushi/tagbar')
  call dein#add('mattn/emmet-vim')
  call dein#add('mattn/webapi-vim')
  call dein#add('moll/vim-bbye')
  call dein#add('morhetz/gruvbox')
  call dein#add('mxw/vim-jsx')
  call dein#add('nathanaelkane/vim-indent-guides')
  call dein#add('neomake/neomake')
  call dein#add('octol/vim-cpp-enhanced-highlight')
  call dein#add('octref/RootIgnore')
  call dein#add('pangloss/vim-javascript')
  call dein#add('radenling/vim-dispatch-neovim')
  call dein#add('raimondi/delimitmate')
  call dein#add('rust-lang/rust.vim')
  call dein#add('ryanoasis/vim-devicons')
  call dein#add('sbdchd/neoformat')
  call dein#add('scrooloose/nerdcommenter')
  call dein#add('scrooloose/nerdtree')
  call dein#add('scrooloose/syntastic')
  call dein#add('sgeb/vim-diff-fold')
  call dein#add('sheerun/vim-polyglot')
  call dein#add('terryma/vim-multiple-cursors')
  call dein#add('thosakwe/vim-flutter')
  call dein#add('tommcdo/vim-exchange')
  call dein#add('tpope/vim-abolish')
  call dein#add('tpope/vim-bundler')
  call dein#add('tpope/vim-commentary')
  call dein#add('tpope/vim-cucumber', {'on_ft': 'cucumber'})
  call dein#add('tpope/vim-dispatch')
  call dein#add('tpope/vim-dotenv')
  call dein#add('tpope/vim-endwise')
  call dein#add('tpope/vim-eunuch')
  call dein#add('tpope/vim-fireplace')
  call dein#add('tpope/vim-fugitive')
  call dein#add('tpope/vim-git')
  call dein#add('tpope/vim-haml')
  call dein#add('tpope/vim-markdown')
  call dein#add('tpope/vim-obsession')
  call dein#add('tpope/vim-ragtag')
  call dein#add('tpope/vim-rails', {'on_ft': ['ruby', 'cucumber']})
  call dein#add('tpope/vim-repeat')
  call dein#add('tpope/vim-rhubarb')
  call dein#add('tpope/vim-scriptease')
  call dein#add('tpope/vim-sleuth')
  call dein#add('tpope/vim-speeddating')
  call dein#add('tpope/vim-surround')
  call dein#add('tpope/vim-unimpaired')
  call dein#add('tpope/vim-vinegar')
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('vim-scripts/searchfold.vim')
  call dein#add('vim-scripts/taglist.vim')
  call dein#add('vimwiki/vimwiki')
  call dein#add('wakatime/vim-wakatime')
  call dein#add('wincent/command-t')
  call dein#add('xuyuanp/nerdtree-git-plugin')
  call dein#add('yggdroot/leaderf')

  call dein#end()
  call dein#save_state()
endif

filetype plugin indent on
syntax enable

if dein#check_install()
  call dein#install()
endif

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
let g:airline#extensions#tabline#enabled = 1
let g:deoplete#enable_at_startup = 1

inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

