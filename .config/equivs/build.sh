#!/bin/sh

CFGDIR=~/.config/equivs
REPODIR=$CFGDIR/repo
WORKDIR=/tmp/equivs

mkdir -p $WORKDIR
cd $WORKDIR

for F in $(find $CFGDIR -name "*.equivs" -type f)
do
  echo "found .equivs file $F"
  PACKAGE=$(cat $F|grep "^Package: " | cut -d " " -f 2)
  PACKAGEPATTERN="${PACKAGE}_*_*.deb"
  if [ ! -e ${REPODIR}/${PACKAGEPATTERN} ]
  then
    echo "not yet built package: $PACKAGE"
    equivs-build $F
    mv $PACKAGEPATTERN $REPODIR
  fi
done

cd $REPODIR
apt-ftparchive packages . >Packages
apt-ftparchive release . >Release
if [ -e Release.gpg ]
then
  rm Release.gpg
fi
gpg --output Release.gpg -ba Release

rmdir $WORKDIR || true