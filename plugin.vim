filetype off                  " required

" set the runtime path to include Vundle and initialize
set runtimepath+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'tpope/vim-fugitive'

Plugin 'godlygeek/tabular'
Plugin 'raimondi/delimitmate'
autocmd FileType cpp let b:delimitMate_expand_cr = 1


Plugin 'Chiel92/vim-autoformat'

Plugin 'plasticboy/vim-markdown'
Plugin 'rust-lang/rust.vim'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'itchyny/vim-haskell-indent'
Plugin 'dag/vim-fish'

Plugin 'vim-syntastic/syntastic'
let g:syntastic_rust_checkers = ['rustc']
let g:syntastic_cpp_compiler_options = ' -std=c++1y'
let g:syntastic_haskell_checkers = ['hdevtools', 'hlint']

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ

