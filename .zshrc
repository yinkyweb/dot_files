# Colors
default="%{$(print '\e[m\e[1m')%}"
grey="%{$(echo -n '\e[1;30m')%}"
red="%{$(echo -n '\e[1;31m')%}"
green="%{$(echo -n '\e[1;32m')%}"
yellow="%{$(echo -n '\e[1;33m')%}"
blue="%{$(echo -n '\e[1;34m')%}"
magenta="%{$(echo -n '\e[1;35m')%}"
cyan="%{$(echo -n '\e[1;36m')%}"
white="%{$(echo -n '\e[1;37m')%}"
lodefault="%{$(echo -n '\e[m')%}"
lored="%{$(echo -n '\e[0;31m')%}"
logreen="%{$(echo -n '\e[0;32m')%}"
loyellow="%{$(echo -n '\e[0;33m')%}"
loblue="%{$(echo -n '\e[0;34m')%}"
lomagenta="%{$(echo -n '\e[0;35m')%}"
locyan="%{$(echo -n '\e[0;36m')%}"
lowhite="%{$(echo -n '\e[0;37m')%}"

# Prompts
export LS_COLORS="no=00:fi=00:di=33;4:ln=35;4:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=31:*.cmd=01;32:*.exe=01;32:*.com=01;32:*.btm=01;32:*.bat=01;32:*.sh=01;32:*.csh=01;32:*.tar=32:*.tgz=32:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=32;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=32:*.cpio=01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:"
PROMPTTTY=`tty | awk 'BEGIN{FS="/"}{print $NF}'`
PROMPT="%B%U%n[%m]<${PROMPTTTY}>(%!) :${cyan} %~ %u${default} %E%#%#%b "
SPROMPT="${red}Correct ${default}> "\''%r'\'' [%BY%bes %BN%bo %BA%bbort %BE%bdit] ? '
unset	PROMPTTTY

# Setopt
setopt auto_cd
setopt auto_pushd
setopt correct
setopt list_packed
setopt noautoremoveslash
setopt nolistbeep
setopt complete_aliases

# Aliases
alias	l='ls -CFGa'
alias	pp='popd'
alias	x='exit'
alias	ls='ls -CFGa'
alias	la='ls -A'
alias	ll='ls -Alh'
alias	lt='ll -t'
alias	lsd='ll -d'
alias -g L="| less"
alias -g G="| grep"

# Lang
export LANG="ja_JP.utf8"

# History
HISTFILE=$HOME/.zsh-history
HISTSIZE=5000000
HISTFILESIZE=5000000
SAVEHIST=5000000
setopt share_history

# Path
PATH=/usr/bin:$PATH
PATH=/usr/sbin:$PATH
PATH=/usr/local/bin:$PATH
PATH=/usr/local/kahua/bin:$PATH
PATH=/usr/local/teTeX/bin:$PATH
PATH=$HOME/sh:$PATH
PATH=.:$PATH

# For fast cpan
export FTP_PASSIVE=1

# For rubygems
export RUBYLIB=/usr/lib/ruby/site_ruby/1.8:/usr/lib/ruby/gems/1.8
export GEM_HOME=/usr/lib/ruby/gems/1.8

# Added from http://0xcc.net/unimag/3/
zstyle ':completion:*:default' menu select=1
autoload -U compinit
compinit

export PYTHONPATH=/usr/local/lib/python2.6/site-packages/

keychain id_rsa
. $HOME/.keychain/$HOST-sh

# Google Go
export GOROOT=$HOME/go
export GOARCH=386
export GOOS=linux
export GOBIN=$HOME/go/bin
export PATH=$GOBIN:$PATH
