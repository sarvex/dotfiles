set nocompatible
source $VIMRUNTIME/vimrc_example.vim
source $VIMRUNTIME/mswin.vim
behave mswin

set diffexpr=MyDiff()
function MyDiff()
  let opt = '-a --binary '
  if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
  if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
  let arg1 = v:fname_in
  if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
  let arg2 = v:fname_new
  if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
  let arg3 = v:fname_out
  if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
  let eq = ''
  if $VIMRUNTIME =~ ' '
    if &sh =~ '\<cmd'
      let cmd = '""' . $VIMRUNTIME . '\diff"'
      let eq = '"'
    else
      let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
    endif
  else
    let cmd = $VIMRUNTIME . '\diff'
  endif
  silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3 . eq
endfunction

set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

" plugin management
Plugin 'gmarik/Vundle.vim'

" file tree
Plugin 'scrooloose/nerdtree'
" file tree and tabs interaction
Plugin 'jistr/vim-nerdtree-tabs'
" commenting
Plugin 'scrooloose/nerdcommenter'
" fuzzy file open
Plugin 'kien/ctrlp.vim'
" popup completion menu
Plugin 'AutoComplPop'
" tags list navigation
Plugin 'taglist.vim'
" yank history
Plugin 'YankRing.vim'
" git integration
Plugin 'tpope/vim-fugitive'
" syntax checking on save
Plugin 'scrooloose/syntastic'
" TextMate-style snippets
Plugin 'msanders/snipmate.vim'
" manipulation of surraunding parens, quotes, etc.
Plugin 'tpope/vim-surround'
" vertical alignment tool
Plugin 'tsaleh/vim-align'
" 'ag' searching integration
Plugin 'rking/ag.vim'
" text object based on indent level (ai, ii)
Plugin 'austintaylor/vim-indentobject'
" global search & replace
Plugin 'greplace.vim'
" better looking statusline
" Plugin 'Lokaltog/powerline'

Plugin 'bling/vim-airline'
" plugin for resolving three-way merge conflicts
Plugin 'sjl/splice.vim'
" plugin for visually displaying indent levels
Plugin 'Indent-Guides'
" end certain structures automatically, e.g. begin/end etc.
Plugin 'tpope/vim-endwise'
" automatic closing of quotes, parenthesis, brackets, etc.
Plugin 'Raimondi/delimitMate'
" calendar, duh!
Plugin 'calendar.vim--Matsumoto'
" A Narrow Region Plugin (similar to Emacs)
Plugin 'chrisbra/NrrwRgn'
" url based hyperlinks for text files
Plugin 'utl.vim'
" A clone of Emacs' Org-mode for Vim
Plugin 'hsitz/VimOrganizer'
" visual undo tree
Plugin 'sjl/gundo.vim'
" switch segments of text with predefined replacements. e.g. '' -> ""
Plugin 'AndrewRadev/switch.vim'
" async external commands with output in vim
Plugin 'tpope/vim-dispatch'
" git diff in the gutter (sign column) and stages/reverts hunks
Plugin 'airblade/vim-gitgutter'

" Ruby/Rails

" rails support
Plugin 'tpope/vim-rails'
" bundler integration (e.g. :Bopen)
Plugin 'tpope/vim-bundler'
" rake integration
Plugin 'tpope/vim-rake'
" A custom text object for selecting ruby blocks (ar/ir)
Plugin 'nelstrom/vim-textobj-rubyblock'
" ruby refactoring
Plugin 'ecomba/vim-ruby-refactoring'
" apidock.com docs integration
Plugin 'apidock.vim'
" toggle ruby blocks style
Plugin 'vim-scripts/blockle.vim'
" lightweight Rspec runner for Vim
Plugin 'josemarluedke/vim-rspec'

" color themes
Plugin 'altercation/vim-colors-solarized'
Plugin 'tpope/vim-vividchalk'
Plugin 'chriskempson/tomorrow-theme', {'rtp': 'vim/'}
Plugin 'Lokaltog/vim-distinguished'
Plugin 'nanotech/jellybeans.vim'
Plugin 'candy.vim'

" syntax support
Plugin 'vim-ruby/vim-ruby'
Plugin 'tsaleh/vim-tmux'
Plugin 'Puppet-Syntax-Highlighting'
Plugin 'JSON.vim'
Plugin 'tpope/vim-cucumber'
Plugin 'tpope/vim-haml'
Plugin 'tpope/vim-markdown'
Plugin 'kchmck/vim-coffee-script'
Plugin 'vitaly/vim-syntastic-coffee'
Plugin 'vim-scripts/jade.vim'
Plugin 'wavded/vim-stylus'
Plugin 'VimClojure'
Plugin 'slim-template/vim-slim'
Plugin 'elixir-lang/vim-elixir'
Plugin 'Blackrush/vim-gocode'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'Buffergator'
Plugin 'Vim-JDE'
Plugin 'Guardian'
Plugin 'github-theme'
Plugin 'Solarized'
Plugin 'cscope.vim'
" Support and minor

" Support for user-defined text objects
Plugin 'kana/vim-textobj-user'
" replacement for the repeat mapping (.) to support plugins
Plugin 'tpope/vim-repeat'
" hide .gitignore-d files from vim
Plugin 'vitaly/vim-gitignore'
" repeat motion with <Space>
Plugin 'scrooloose/vim-space'
" Github's gist support
Plugin 'mattn/gist-vim'
" web APIs support
Plugin 'mattn/webapi-vim'

Plugin 'Shougo/vimproc'
Plugin 'Shougo/unite.vim'
Plugin 'Shougo/unite-outline'
Plugin 'ujihisa/unite-colorscheme'
Plugin 'jimenezrick/vimerl'

Plugin 'OmniCppComplete'
Plugin 'c.vim'
Plugin 'python.vim'
Plugin 'SuperTab'
Plugin 'a.vim'
Plugin 'L9'
Plugin 'FuzzyFinder'
Plugin 'pyflakes.vim'
Plugin 'airblade/vim-rooter'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'openssl.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'maksimr/vim-jsbeautify'
Plugin 'majutsushi/tagbar'
Plugin 'tpope/vim-unimpaired'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'OrangeT/vim-csharp.git'
Plugin 'mhinz/vim-signify'

call vundle#end()            " required

filetype plugin indent on    " required

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

" let g:Powerline_symbols = 'fancy'
set t_Co=256
" let g:Powerline_mode_V="V·LINE"
" let g:Powerline_mode_cv="V·BLOCK"
" let g:Powerline_mode_S="S·LINE"
" let g:Powerline_mode_cs="S·BLOCK"
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1


" showmarks
let g:showmarks_enable = 0 " disabled by default by populardemand ;)
hi! link ShowMarksHLl LineNr
hi! link ShowMarksHLu LineNr
hi! link ShowMarksHLo LineNr
hi! link ShowMarksHLm LineNr

" syntastic
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=2
let g:syntastic_check_on_wq=0
let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'

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

" VimClojure
let g:vimclojure#ParenRainbow = 1
let g:vimclojure#DynamicHighlighting = 1

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

let g:unite_source_history_yank_enable = 1
let g:unite_enable_start_insert = 1
let g:unite_source_file_mru_long_limit = 100
let g:unite_source_directory_mru_long_limit = 100
call unite#filters#matcher_default#use(['matcher_fuzzy'])

" vim-rspec
map <Leader>r :call RunNearestSpec()<CR>
" Fast saving
nmap <leader>w :w!<cr>

set hlsearch
set incsearch
set ignorecase
set smartcase
set wrapscan
set incsearch
set magic
set cursorline
set splitright
set foldmethod=syntax
set foldlevelstart=10
set guifont=Dejavu_Sans_Mono_for_Powerline:h8
set shiftwidth=4
set softtabstop=4
set autoindent
set smartindent
set expandtab
set laststatus=2
set encoding=utf-8
set number
set nowritebackup
set nobackup
set noswapfile
set numberwidth=3
set textwidth=0
set nowrap
set showcmd
set showmatch
set ruler
set wildmenu
set laststatus=2
set listchars=tab:▷⋅,trail:·
set magic
set clipboard=unnamedplus
set fileformat=unix
set fileformats=unix,dos

colors solarized
filetype on
