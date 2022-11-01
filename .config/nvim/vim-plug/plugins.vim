call plug#begin('~/.config/nvim/autoload/plugged')
    " Better Syntax Support
    Plug 'sheerun/vim-polyglot'
    " File Explorer
    Plug 'scrooloose/NERDTree'
    " Auto pairs for '(' '[' '{'
    Plug 'jiangmiao/auto-pairs'
    " Airline status
    Plug 'vim-airline/vim-airline' 
    " Airline themes
    Plug 'vim-airline/vim-airline-themes'
    " Coc autocompletion and more
    Plug 'neoclide/coc.nvim', {'branch': 'release'}    
    " Monokai pro alternative theme
    Plug 'sainnhe/sonokai'
    Plug 'sainnhe/edge'
    " Git status line symbols
    Plug 'airblade/vim-gitgutter'
    " Git integration in vim
    Plug 'tpope/vim-fugitive'
    " Insert unicode from latex commands using <C-l>
    Plug 'joom/latex-unicoder.vim'
    " Racket plugin
    Plug 'wlangstroth/vim-racket'
    " Fuzzy file search with <C-p>
    Plug 'kien/ctrlp.vim'
    " fzf fuzzy search
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    " fzf vim integration
    Plug 'junegunn/fzf.vim'
    " Rust
    Plug 'rust-lang/rust.vim'
    " dwim commenting wth gc
    Plug 'tomtom/tcomment_vim'
    " Better highlighting for C and family
    Plug 'jackguo380/vim-lsp-cxx-highlight'
    " make s be f but two letters
    Plug 'justinmk/vim-sneak'
    " surround things with things very easily
    Plug 'tpope/vim-surround'
call plug#end()

