#!/bin/sh

CFGDIR=~/.config/equivs

mkdir -p /tmp/equivs
cd /tmp/equivs

for F in $(find $CFGDIR -name "*.equivs" -type f)
do
  echo "found .equivs file $F"
  PACKAGE=$(cat $F|grep "^Package: " | cut -d " " -f 2)
  PACKAGEPATTERN="${PACKAGE}_*_*.deb"
  if [ ! -e ${CFGDIR}/${PACKAGEPATTERN} ]
  then
    echo "not yet built package: $PACKAGE"
    equivs-build $F
    mv $PACKAGEPATTERN $CFGDIR
  fi
done

rmdir /tmp/equivs || true