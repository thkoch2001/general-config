# see http://zsh.sourceforge.net/Contrib/startup/std/zshenv

if [[ $SHLVL == 1 && ! -o LOGIN ]]; then
  source ~/.config/environment/source_local_d.sh
fi
