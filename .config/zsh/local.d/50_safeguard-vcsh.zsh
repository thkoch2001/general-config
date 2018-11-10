# saveguard against vcsh
if [ x${VCSH_REPO_NAME} != "x" ]
then
  for C in sudo su vcsh
  do
    alias $C="echo \"You don't want to run $C during a vcsh session!\" && false "
  done
fi
