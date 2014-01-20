set nocompatible
" source $VIMRUNTIME/vimrc_example.vim
behave xterm

set bs=1
set comments=sr:/*,mb:*,mb:**,el:*/,://,b:#,:%,:XCOMM,n:>,fb:-
set formatoptions=tcql
set nobackup
set nowrap
set ruler
set shortmess=aT
set ul=0
set wildmenu
set wildmode=list:longest
set vb
set matchpairs+=<:>
set colorcolumn=81

map  <F2> o/*<CR><ESC>79a*<ESC>o*/<CR><ESC>
map! <F2>  /*<CR><ESC>79a*<ESC>o*/<CR><ESC>

map  <F3> o/<ESC>71a/<ESC>o//<ESC>68a <ESC>a//<ESC>yypkVkyjjpddpo<ESC>
map! <F3>  /<ESC>71a/<ESC>o//<ESC>68a <ESC>a//<ESC>yypkVkyjjpddpo<ESC>

map  <F4> o/<ESC>78a/<ESC>^
map! <F4>  /<ESC>78a/<ESC>^

map  <F5> :nohlsearch<CR>

if has ("gui_running")

	" For Unix:
	set number
	set guifont=Bitstream\ Vera\ Sans\ Mono\ 12
elseif &term == "linux" || &term == "xterm"
	set t_Co=8
	set t_Sf=[3%dm
	set t_Sb=[4%dm
endif

"
" Windows can't deal with these characters.
"

if has ("gui_running") || &term != "win32"
	set listchars=tab:»·,trail:·
endif

syntax on

hi Comment    cterm=NONE ctermfg=6 gui=NONE    guifg=#828482
hi Constant   cterm=NONE ctermfg=6 gui=NONE    guifg=#828482
hi Special    cterm=NONE ctermfg=2 gui=NONE    guifg=#0000ff
hi Identifier cterm=NONE ctermfg=2 gui=NONE    guifg=#0000ff
hi Statement  cterm=NONE ctermfg=2 gui=NONE    guifg=#0000ff
hi PreProc    cterm=NONE ctermfg=2 gui=NONE    guifg=#0000ff
hi Type       cterm=NONE ctermfg=2 gui=NONE    guifg=#0000ff
hi Ignore     cterm=NONE ctermfg=1 gui=NONE    guifg=#ff0000
hi NonText    cterm=NONE ctermfg=1 gui=NONE    guifg=#6495ed
hi Visual                          gui=reverse guifg=#da70d6

" De-uglification for C and C++
hi Structure  cterm=NONE ctermfg=2 gui=NONE guifg=#0000ff

" The stupid system vimrc is overriding my formatoptions (which breaks <F2> and
" <F3>).

" Apparently also only for Unix.
" au! cprog FileType

digraph /l  8467  " ℓ
digraph [=  8849  " ⊑
digraph ]=  8850  " ⊒
digraph [[  10214 " ⟦
digraph ]]  10215 " ⟧
digraph \|- 8866  " ⊢
digraph \|= 8872  " ⊨
digraph ox  8855  " ⊗
digraph o+  8853  " ⊕
digraph _\| 8869  " ⊥
digraph \|_ 8869  " ⊥
digraph TT  8868  " ⊤
digraph \|> 8614  " ↦
digraph /=  8800  " ≠

au Syntax tex syn region texZone start="\\begin{ocaml}" end="\\end{ocaml}" 
au Syntax tex syn region texZone start="\\code{" end="}" 

