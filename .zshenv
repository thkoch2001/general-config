# see http://zsh.sourceforge.net/Contrib/startup/std/zshenv

if [[ $SHLVL == 1 && ! -o LOGIN ]]; then
  source ~/.myenvironment
fi
