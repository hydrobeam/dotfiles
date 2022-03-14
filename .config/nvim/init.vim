source $HOME/.config/nvim/vim-plug/plugins.vim
source $HOME/.config/nvim/airline.vim

set showmatch               " show matching 
set ignorecase              " case insensitive 
set hlsearch                " highlight search 
set invhls                  " fuck highlighting
set tabstop=4               " number of columns occupied by a tab 
set softtabstop=4           " see multiple spaces as tabstops so <BS> does the right thing
set expandtab               " converts tabs to white space
set shiftwidth=4            " width for autoindents
set autoindent              " indent a new line the same amount as the line just typed
set number                  " add line numbers
set wildmode=longest,list   " get bash-like tab completions
syntax on                   " syntax highlighting
set mouse=a                 " enable mouse click
set clipboard=unnamedplus   " using system clipboard
set cursorline              " highlight current cursorline
set ttyfast                 " Speed up scrolling in Vim
set whichwrap+=<,h,[        " Make left + h go up a line;[ <-brackets for insert mode-> ]
set whichwrap+=>,l,]        " Make right + l go down a line
set list
set listchars+=space:.      " Add dots for whitespace like vscode

filetype plugin indent on

let mapleader=","

set termguicolors
let g:sonokai_style = 'andromeda'
let g:sonokai_enable_italic = 0
let g:sonokai_disable_italic_comment = 0
colorscheme sonokai 
" hi Normal guibg=NONE ctermbg=NONE

" let g:edge_style = 'neon'
" let g:edge_disable_italic_comment=1
" let g:edge_better_performance = 1
" colorscheme edge

set nobackup
set nowritebackup

set updatetime=300

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <leader>rn <Plug>(coc-rename)
let g:coc_disable_transparent_cursor = 1
let g:coc_global_extensions = ['coc-json', 'coc-rust-analyzer', 'coc-pyright']

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ }

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" : "<TAB>"

inoremap <C-l> <Esc>:call unicoder#start(1)<CR>

tnoremap <Esc> <C-\><C-n>

" Markdown
let g:vim_markdown_fenced_languages = ['rust=rs']
let g:vim_markdown_math = 1
let g:vim_markdown_frontmatter =1
let g:vim_markdown_toml_frontmatter =1
let g:vim_markdown_folding_disabled = 1

filetype plugin indent on
