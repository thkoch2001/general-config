for LOCAL_ENVFILE in ${HOME}/.config/environment/local.d/*.sh
do
  . ${LOCAL_ENVFILE}
done
