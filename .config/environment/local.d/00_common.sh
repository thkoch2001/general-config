# source this file from .bashrc or .zshrc

export PATH=$HOME/bin:$HOME/go/bin:$PATH
export DEBEMAIL="thomas@koch.ro"
export DEBFULLNAME="Thomas Koch"
export QUILT_PATCHES=debian/patches
export SHELL=/bin/zsh
export MAILER=kmailservice

# fix java apps in awesome
# http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=508650
export _JAVA_AWT_WM_NONREPARENTING=1

# fix eclipse
# http://glandium.org/blog/?p=416
# via http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=538871
export MOZ_GRE_CONF=/etc/gre.d/1.9.system.conf
export BC_ENV_ARGS=$HOME/.config/bc

# emacs rules
export ALTERNATE_EDITOR=""
export EDITOR=emacsclient
alias e=emacsclient\ -t

# saveguard against vcsh

if [ x${VCSH_REPO_NAME} != "x" ]
then
  for C in sudo su vcsh
  do
    alias $C="echo \"You don't want to run $C during a vcsh session!\" && false "
  done
fi

get_first_xpra_display()
{
  xpra list | sed -n 's/^.* session at \(.*\)/\1/p' | head -n 1
}

#if [ -z "$DISPLAY" ] && [ -n "$SSH_CONNECTION" ]; then
#  XPRADISPLAY=$(get_first_xpra_display)
#  export DISPLAY=${XPRADISPLAY}
#fi

# 256 colors in terminal
[ -z "$TMUX" ] && export TERM=xterm-256color

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi
