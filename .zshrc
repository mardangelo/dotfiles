# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

#function tldr() {
#  command tldr "$@" | less -R
#}

# Configure previews in .zshrc
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls $realpath'
zstyle ':fzf-tab:complete:z:*' fzf-preview 'ls $realpath'

#zstyle ':completion:*:*:cp:*' file-sort size

zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' menu select

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/.zcompcache"

#zstyle ':completion:*' verbose yes
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'

# Command descriptions for executables
#zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
#  fzf-preview 'cmd=${word}; whatis $cmd 2>/dev/null || echo "No description available"'

# File information
#zstyle ':fzf-tab:complete:*:*' fzf-preview 'ls -l --color=always ${(Q)realpath}'

# Directory contents
#zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls -l --color=always $realpath'

# Process information
zstyle ':fzf-tab:complete:kill:*' fzf-preview 'ps --pid=$word -o cmd,pcpu,pmem'

zstyle ':completion:*' group-name ''
zstyle ':completion:*:*:-command-:*:*' group-order alias builtins functions commands

zstyle ':completion:*' file-list all

zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zle -C alias-expension complete-word _generic
bindkey '^a' alias-expension
zstyle ':completion:alias-expension:*' completer _expand_alias

#zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
#  fzf-preview '
#    cmd=${word}
#    if [[ -n "$cmd" ]]; then
#      if command -v tldr &>/dev/null; then
#        tldr "$cmd" 2>/dev/null || whatis "$cmd" 2>/dev/null || echo "No description available"
#      else
#        whatis "$cmd" 2>/dev/null || echo "No description available"
#      fi
#    else
#      echo "No command name available"
#    fi
#  '
#
## Keep your existing style for file/path completion
#zstyle ':fzf-tab:complete:*:*' fzf-preview '
#  if [[ -d ${realpath} ]]; then
#    ls --color=always ${realpath}  # Simple directory listing, no -l flag
#  elif [[ -f ${realpath} ]]; then
#    # Just show filename and preview of content
#    { echo -e "\033[1;36m${realpath:t}\033[0m"; echo; head -n20 ${realpath} } 2>/dev/null
#  else
#    echo ${(Q)realpath}  # Just show the name without ls -l
#  fi
#'
# Add this at the very end of your .zshrc
zstyle ':fzf-tab:complete:*:*' fzf-preview '
  # Get the completion item
  local item=${(Q)realpath:-$word}
  
  # If realpath exists, it is likely a file/directory completion
  if [[ -n "$realpath" ]]; then
    # Extract just the filename from the path
    local filename=${item##*/}
    
    if [[ -d "$item" ]]; then
      echo -e "\033[1;34m📁 $filename/\033[0m"
      ls --color=always "$item" | head -10
    elif [[ -f "$item" ]]; then
      echo -e "\033[1;32m📄 $filename\033[0m"
      head -5 "$item" 2>/dev/null || echo "[Binary file]"
    else
      echo -e "\033[1;33m$filename\033[0m"
    fi
  
  # Otherwise it might be a command
  elif [[ -n "$word" ]]; then
    # Check if it looks like a command
    if command -v "$word" &>/dev/null; then
      # Try tldr first, then fall back to whatis
      if command -v tldr &>/dev/null; then
        tldr "$word" 2>/dev/null || whatis "$word" 2>/dev/null || echo "No description available"
      else
        whatis "$word" 2>/dev/null || echo "No description available"
      fi
    else
      # Just show the word itself
      echo "$word"
    fi
  else
    echo "No preview available"
  fi
'

export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye >/dev/null
gpg-connect-agent /bye > /dev/null || gpg-agent --daemon --options ~/.gnupg/gpg-agent.conf

# Customize to your needs...

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

PATH="$HOME/.config/emacs/bin:$PATH"
export PATH

export FZF_DEFAULT_COMMAND="rg --files --hidden"

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export TERM=xterm-24bit

test -r ~/.dir_colors && eval "$(dircolors -b ~/.dir_colors)" || eval "$(dircolors -b)"
LS_COLORS="$LS_COLORS:*Dockerfile=38;2;122;162;247:*Makefile=38;2;86;95;137:*README.md=1;38;2;224;175;104"

