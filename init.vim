set runtimepath+=~/.config/nvim/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.config/nvim/dein')
  call dein#begin('~/.config/nvim/dein')

  call dein#add('~/.config/nvim/dein/repos/github.com/Shougo/dein.vim')

  call dein#add('airblade/vim-gitgutter')
  call dein#add('airblade/vim-rooter')
  call dein#add('ap/vim-css-color', {'on_ft': ['css', 'scss', 'conf', 'javascript']})
  call dein#add('arithran/vim-delete-hidden-buffers')
  call dein#add('Blackrush/vim-gocode')
  call dein#add('bogado/file-line')
  call dein#add('brookhong/cscope.vim')
  call dein#add('chrisbra/nrrwrgn')
  call dein#add('christoomey/vim-tmux-navigator')
  call dein#add('conradirwin/vim-bracketed-paste')
  call dein#add('ctrlpvim/ctrlp.vim')
  call dein#add('dart-lang/dart-vim-plugin')
  call dein#add('davidhalter/jedi-vim')
  call dein#add('dense-analysis/ale')
  call dein#add('deoplete-plugins/deoplete-go', {'build': 'make'})
  call dein#add('deoplete-plugins/deoplete-jedi')
  call dein#add('easymotion/vim-easymotion')
  call dein#add('ecomba/vim-ruby-refactoring')
  call dein#add('editorconfig/editorconfig-vim')
  call dein#add('ekalinin/Dockerfile.vim')
  call dein#add('elixir-lang/vim-elixir')
  call dein#add('elzr/vim-json')
  call dein#add('ervandew/supertab')
  call dein#add('fatih/vim-go')
  call dein#add('godlygeek/tabular')
  call dein#add('gorodinskiy/vim-coloresque')
  call dein#add('honza/vim-snippets')
  call dein#add('hsitz/VimOrganizer')
  call dein#add('janko-m/vim-test')
  call dein#add('jeetsukumaran/vim-buffergator')
  call dein#add('jimenezrick/vimerl')
  call dein#add('jistr/vim-nerdtree-tabs')
  call dein#add('josemarluedke/vim-rspec')
  call dein#add('junegunn/fzf')
  call dein#add('junegunn/gv.vim')
  call dein#add('junegunn/vim-easy-align')
  call dein#add('kana/vim-textobj-user')
  call dein#add('kchmck/vim-coffee-script')
  call dein#add('Konfekt/FastFold')
  call dein#add('kristijanhusak/defx-icons')
  call dein#add('kristijanhusak/vim-hybrid-material')
  call dein#add('leafgarland/typescript-vim')
  call dein#add('Lokaltog/vim-distinguished')
  call dein#add('luochen1990/rainbow')
  call dein#add('machakann/vim-highlightedyank')
  call dein#add('majutsushi/tagbar')
  call dein#add('mattn/calendar-vim')
  call dein#add('mattn/emmet-vim')
  call dein#add('mattn/gist-vim')
  call dein#add('mattn/webapi-vim')
  call dein#add('mbbill/undotree')
  call dein#add('mechatroner/rainbow_csv')
  call dein#add('mhinz/vim-signify')
  call dein#add('michaeljsmith/vim-indent-object')
  call dein#add('mileszs/apidock.vim')
  call dein#add('moll/vim-bbye')
  call dein#add('morhetz/gruvbox')
  call dein#add('mxw/vim-jsx')
  call dein#add('nanotech/jellybeans.vim')
  call dein#add('nelstrom/vim-textobj-rubyblock')
  call dein#add('neoclide/coc.nvim', {'merged':0, 'rev': 'release'})
  call dein#add('neomake/neomake')
  call dein#add('octol/vim-cpp-enhanced-highlight')
  call dein#add('octref/RootIgnore')
  call dein#add('othree/html5.vim')
  call dein#add('pangloss/vim-javascript')
  call dein#add('plasticboy/vim-markdown')
  call dein#add('radenling/vim-dispatch-neovim')
  call dein#add('rafi/awesome-vim-colorschemes')
  call dein#add('raimondi/delimitMate')
  call dein#add('rking/ag.vim')
  call dein#add('rust-lang/rust.vim')
  call dein#add('ryanoasis/vim-devicons')
  call dein#add('sbdchd/neoformat')
  call dein#add('scrooloose/nerdcommenter')
  call dein#add('scrooloose/nerdtree')
  call dein#add('scrooloose/syntastic')
  call dein#add('scrooloose/vim-space')
  call dein#add('sgeb/vim-diff-fold')
  call dein#add('sheerun/vim-polyglot')
  call dein#add('Shougo/defx.nvim')
  call dein#add('Shougo/denite.nvim')
  call dein#add('Shougo/deol.nvim')
  call dein#add('Shougo/deoplete.nvim')
  call dein#add('Shougo/deoppet.nvim')
  call dein#add('Shougo/deorise.nvim')
  call dein#add('Shougo/neosnippet-snippets')
  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/vimproc')
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
  call dein#add('tpope/vim-vividchalk')
  call dein#add('tsaleh/vim-tmux')
  call dein#add('udalov/kotlin-vim')
  call dein#add('unblevable/quick-scope')
  call dein#add('valloric/vim-indent-guides')
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('vim-ruby/vim-ruby')
  call dein#add('vim-scripts/a.vim')
  call dein#add('vim-scripts/blockle.vim')
  call dein#add('vim-scripts/c.vim')
  call dein#add('vim-scripts/FuzzyFinder')
  call dein#add('vim-scripts/greplace.vim')
  call dein#add('vim-scripts/Guardian')
  call dein#add('vim-scripts/jade.vim')
  call dein#add('vim-scripts/L9')
  call dein#add('vim-scripts/openssl.vim')
  call dein#add('vim-scripts/python.vim')
  call dein#add('vim-scripts/searchfold.vim')
  call dein#add('vim-scripts/taglist.vim')
  call dein#add('vim-scripts/utl.vim')
  call dein#add('vim-scripts/YankRing.vim')
  call dein#add('vitaly/vim-gitignore')
  call dein#add('vitaly/vim-syntastic-coffee')
  call dein#add('wakatime/vim-wakatime')
  call dein#add('wavded/vim-stylus')
  call dein#add('wincent/command-t')
  call dein#add('xuyuanp/nerdtree-git-plugin')
  call dein#add('yggdroot/leaderf')
  call dein#add('yuttie/comfortable-motion.vim')
  call dein#add('zchee/deoplete-clang')
  
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
