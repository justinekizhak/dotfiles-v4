"" -----------------------------------------------------------------------------
"" Copyright (c) 2018, Justine T Kizhakkinedath
"" All rights reserved
""
"" Licensed under the terms of MIT License
"" See LICENSE file in the project root for full information.
"" -----------------------------------------------------------------------------

"Automatic reloading of .vimrc
    augroup sourcing_plugins
        au!
        autocmd! bufwritepost ~/dotfiles/vim/plugins.vim source ~/dotfiles/vim/plugins.vim
    augroup END
"""""""""""""""""""""""""""""VIM-PLUG SETTINGS"""""""""""""""""""""""""""""""""
call plug#begin()
    "1) Airline status bar
        Plug 'vim-airline/vim-airline'
        Plug 'vim-airline/vim-airline-themes'
        let g:airline_theme='solarized'
        "powerline symbols
            let g:airline_left_sep = ''
            let g:airline_right_sep = ''

    "2)NERDTree
        Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
        map <C-n> :NERDTreeToggle<CR>
        let NERDTreeQuitOnOpen=1

    "3)better vim-tmux navigation
        Plug 'christoomey/vim-tmux-navigator'

    "4)Ale code linting
        Plug 'w0rp/ale'
        "Set this in your vimrc file to disabling highlighting
            let g:ale_set_highlights = 0
        "Set this. Airline will handle the rest.
            let g:airline#extensions#ale#enabled = 1
        highlight clear ALEErrorSign
        highlight clear ALEWarningSign

    "5) vim-repeat. required for easyclip
        Plug 'tpope/vim-repeat'

    "6) Easyclip for copy pasting in vim
        Plug 'svermeulen/vim-easyclip'

    "7)AutoPairs auto inserting brackets
        Plug 'jiangmiao/auto-pairs'

    "8)Timestamp
        Plug 'vim-scripts/timestamp.vim'
        augroup temp_timestamp_disble
            au!
            autocmd BufEnter *.snippets :DisableTimestamp
            autocmd BufLeave *.snippets :EnableTimestamp
        augroup END

    "9)UltiSnips snippets
        "Track the engine.
            Plug 'SirVer/ultisnips'
        "Snippets are separated from the engine. Add this if you want them:
            Plug 'honza/vim-snippets'
        "If you want :UltiSnipsEdit to split your window.
            let g:UltiSnipsEditSplit="vertical"
            let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"
            let g:UltiSnipsSnippetDirectories=["UltiSnips"]

    "10)SuperTabs for better integration of ycm and ultisnips
        Plug 'ervandew/supertab'
        "make YCM compatible with UltiSnips (using supertab)
            let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
            let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
            let g:SuperTabDefaultCompletionType = '<C-n>'
        "better key bindings for UltiSnipsExpandTrigger
            let g:UltiSnipsExpandTrigger = "<c-space>"
            let g:UltiSnipsJumpForwardTrigger = "<tab>"
            let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

    "11)highlight text as they are typed
        Plug 'osyo-manga/vim-over'
        map <leader>r :OverCommandLine<CR>

    "12)git diff
        Plug 'airblade/vim-gitgutter'

    "13)Show vim undo tree
        Plug 'simnalamburt/vim-mundo'
        map <leader>u :MundoToggle<CR>

    "14)Show indent line
        Plug 'Yggdroot/indentLine'

    "15)Show tagbar on right side
        Plug 'majutsushi/tagbar'
        nnoremap <leader>t :TagbarToggle<CR>

    "16)Use ag
        Plug 'brookhong/ag.vim'
        "cnoreabbrev Ack Ack!
        nnoremap <Leader>s :Ag!<Space>

    "17)Buffer explorer
        Plug 'jlanzarotta/bufexplorer'

    "18)Renamer bulk rename plugin
        Plug 'qpkorr/vim-renamer'
        let g:RenamerSupportColonWToRename=1
        nmap <Leader>rf <Plug>RenamerStart

    "19)Fugitive for git integration
        Plug 'tpope/vim-fugitive'

    "20)Sourrounding with parenthesis
        Plug 'tpope/vim-surround'

    "21)For commenting lines
        Plug 'scrooloose/nerdcommenter'

    "22)Vim motion on speed
        Plug 'easymotion/vim-easymotion'

    "23)Use multiple cursors like in sublime text
        Plug 'terryma/vim-multiple-cursors'
        let g:multi_cursor_use_default_mapping=0
        " Default mapping
        let g:multi_cursor_start_word_key      = '<C-m>'
        let g:multi_cursor_select_all_word_key = '<A-n>'
        let g:multi_cursor_start_key           = 'g<C-m>'
        let g:multi_cursor_select_all_key      = 'g<A-n>'
        let g:multi_cursor_next_key            = '<C-m>'
        let g:multi_cursor_prev_key            = '<C-p>'
        let g:multi_cursor_skip_key            = '<C-x>'
        let g:multi_cursor_quit_key            = '<Esc>'

    "25)Use DevIcons
        Plug 'ryanoasis/vim-devicons'
        set encoding=UTF-8

    "26) YCM
        Plug 'Valloric/YouCompleteMe'
        "Youcompleteme fix
        let g:ycm_global_ycm_extra_conf = '~/.vim/plugged/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
        noremap <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>

    "27) CommandT better file search
        Plug 'wincent/command-t'
        nmap <silent> <Leader>T <Plug>(CommandT)
        nmap <silent> <Leader>b <Plug>(CommandTBuffer)
        nmap <silent> <Leader>j <Plug>(CommandTJump)

    "28)Clang formatting
        Plug 'rhysd/vim-clang-format'

    "29)Solarised theme
        Plug 'justinethomas009/vim-colors-solarized'

    "30)Hightlight hex values
        Plug 'vim-scripts/hexHighlight.vim'

    "31)Show number of times search pattern found
        Plug 'google/vim-searchindex'

call plug#end()
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! ClearRegisters()
    let regs='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/-="*+'
    let i=0
    while (i<strlen(regs))
        exec 'let @'.regs[i].'=""'
        let i=i+1
    endwhile
endfunction

command! ClearRegisters call ClearRegisters()
