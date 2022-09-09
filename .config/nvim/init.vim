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

nnoremap <SPACE> <Nop>
let mapleader=" "

set termguicolors
let g:sonokai_style = 'andromeda'
let g:sonokai_enable_italic = 0
let g:sonokai_disable_italic_comment = 0
colorscheme sonokai 

set nobackup
set nowritebackup

set updatetime=300

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
" doom inspired
nmap <leader> cr <Plug>(coc-rename)

let g:coc_disable_transparent_cursor = 1
let g:coc_global_extensions = ['coc-json', 'coc-rust-analyzer', 'coc-pyright']

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ }

" stolen from :h coc-completion
function! CheckBackSpace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

" Insert <tab> when previous text is space, refresh completion if not.
inoremap <silent><expr> <TAB>
  \ coc#pum#visible() ? coc#pum#next(1):
  \ CheckBackSpace() ? "\<Tab>" :
  \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
"""

" Use <CR> (enter) to confirm completion, use: >
inoremap <expr> <cr> coc#pum#visible() ? coc#_select_confirm() : "\<CR>"
"


inoremap <C-l> <Esc>:call unicoder#start(1)<CR>

tnoremap <Esc> <C-\><C-n>
" rusty
let g:rustfmt_autosave = 1

" Markdown
let g:vim_markdown_fenced_languages = ['rust=rs']
let g:vim_markdown_math = 1
let g:vim_markdown_frontmatter =1
let g:vim_markdown_toml_frontmatter =1
let g:vim_markdown_folding_disabled = 1

filetype plugin indent on
