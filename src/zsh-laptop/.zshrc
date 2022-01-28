alias ls='ls -l --color=auto'
export EDITOR=nvim
export TERMINAL="st"
export BROWSER=firefox
export READER="zathura"
bindkey -e
export KEYTIMEOUT=1

# zle-keymap-select(){
#   if [[ ${KEYMAP} == vicmd ]] || 
#      [[ $1 = 'block' ]]; then
#       echo -ne '\e[1 q'
#   elif [[ ${KEYMAP} == main ]] ||
#         [[ ${KEYMAP} == viins ]] ||
#         [[ ${KEYMAP} == '' ]] ||
#         [[ $1 = 'beam' ]]; then
#       echo -ne '\e[5 q'
#   fi
# }

# zle -N zle-keymap-select

# zle-line-init(){
# zle -K viins  #initiative `vi insert` as keymap (can be removed if 'bindkey -V' has been set elsewhere)
#   echo -ne "\e[5 q"
# }

# zle -N zle-line-init
# echo -ne '\e[5 q' #Use beam shape cursor on startup
# preexec() { echo -ne '\e[5 q' ;} #Use beam shape cursor for each new prompt

#Edit line in vim with ctrl-e
autoload edit-command-line; zle -N edit-command-line
#bindkey '^e' edit-command-line

source ~/dotfiles/main.sh
source ~/shell-functions/main.sh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

export PATH="${PATH}:$HOME/.local/bin/"
export beats_id=88:E9:FE:93:7F:BF
export _JAVA_AWT_WM_NONREPARENTING=1

[ -f "/home/david/.ghcup/env" ] && source "/home/david/.ghcup/env" # ghcup-env


eval "$(starship init zsh)"
