if has('nvim')
  let plug_path = '~/.local/share/nvim/site/autoload/plug.vim'
  let path = '~/.config/nvim/plugged'
else 
  let plug_path = '~/.vim/autoload/plug.vim'
  let path = '~/.vim/plugged'
endif

if empty(glob(plug_path))
  silent !curl -fLo &plug_path --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin(path)
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'ap/vim-css-color', {'for': ['css', 'scss', 'conf', 'javascript']}
Plug 'arithran/vim-delete-hidden-buffers'
Plug 'Blackrush/vim-gocode'
Plug 'bogado/file-line'
Plug 'brookhong/cscope.vim'
Plug 'chrisbra/nrrwrgn'
Plug 'christoomey/vim-tmux-navigator'
Plug 'conradirwin/vim-bracketed-paste'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'dart-lang/dart-vim-plugin'
Plug 'davidhalter/jedi-vim'
Plug 'dense-analysis/ale'
Plug 'easymotion/vim-easymotion'
Plug 'ecomba/vim-ruby-refactoring'
Plug 'editorconfig/editorconfig-vim'
Plug 'ekalinin/Dockerfile.vim'
Plug 'elixir-lang/vim-elixir'
Plug 'elzr/vim-json'
Plug 'ervandew/supertab'
Plug 'fatih/vim-go'
Plug 'godlygeek/tabular'
Plug 'gorodinskiy/vim-coloresque'
Plug 'honza/vim-snippets'
Plug 'hsitz/VimOrganizer'
Plug 'janko-m/vim-test'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'jimenezrick/vimerl'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'josemarluedke/vim-rspec'
Plug 'junegunn/fzf'
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
Plug 'majutsushi/tagbar'
Plug 'mattn/calendar-vim'
Plug 'mattn/emmet-vim'
Plug 'mattn/gist-vim'
Plug 'mattn/webapi-vim'
Plug 'mbbill/undotree'
Plug 'mechatroner/rainbow_csv'
Plug 'mhinz/vim-signify'
Plug 'michaeljsmith/vim-indent-object'
Plug 'mileszs/apidock.vim'
Plug 'moll/vim-bbye'
Plug 'morhetz/gruvbox'
Plug 'mxw/vim-jsx'
Plug 'nanotech/jellybeans.vim'
Plug 'nelstrom/vim-textobj-rubyblock'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'neomake/neomake'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'octref/RootIgnore'
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript'
Plug 'plasticboy/vim-markdown'
Plug 'radenling/vim-dispatch-neovim'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'raimondi/delimitMate'
Plug 'rking/ag.vim'
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
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
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
Plug 'Yggdroot/leaderf'
Plug 'Yggdroot/indentline'
Plug 'yuttie/comfortable-motion.vim'
call plug#end()

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
set hidden
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
set shortmess+=c
set showcmd
set showmatch
set signcolumn=yes
set smartcase
set smartindent
set softtabstop=2
set splitright
set textwidth=0
set updatetime=300
set wildmenu
set wrapscan

let g:airline#extensions#tabline#enabled = 1
"let g:deoplete#enable_at_startup = 1

" Use tab for trigger completion with characters ahead and navigate.
inoremap <silent><expr><TAB>  pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : coc#refresh()
" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1] =~# '\s'
endfunction

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
augroup end

