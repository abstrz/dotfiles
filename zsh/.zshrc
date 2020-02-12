source ~/.profile


export EDITOR=vim
export TERMINAL="st"
export BROWSER=firefox
export READER="zathura"

# Path to your oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh/

ZSH_THEME="kolo"


ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

source $ZSH/oh-my-zsh.sh

bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete mode
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

#Change cursor shape for different vi modes
zle-keymap-select(){
  if [[ ${KEYMAP} == vicmd ]] || 
     [[ $1 = 'block' ]]; then
      echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
        [[ ${KEYMAP} == viins ]] ||
        [[ ${KEYMAP} == '' ]] ||
        [[ $1 = 'beam' ]]; then
      echo -ne '\e[5 q'
  fi
}

zle -N zle-keymap-select

zle-line-init(){
zle -K viins  #initiative `vi insert` as keymap (can be removed if 'bindkey -V' has been set elsewhere)
  echo -ne "\e[5 q"
}

zle -N zle-line-init
echo -ne '\e[5 q' #Use beam shape cursor on startup
preexec() { echo -ne '\e[5 q' ;} #Use beam shape cursor for each new prompt



#Edit line in vim with ctrl-e

autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line



PATH="/home/david/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/david/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/david/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/david/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/david/perl5"; export PERL_MM_OPT;

#scripts
source ~/scripts/defs.sh

export beats_id=DC:D3:A2:C4:41:1F
export _JAVA_AWT_WM_NONREPARENTING=1
export NNN_USE_EDITOR=1

#plugins
source ~/.oh-my-zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
plugins=(git)
