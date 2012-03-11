#!/bin/sh

CFGDIR=~/.config/equivs
REPODIR=$CFGDIR/repo

mkdir -p /tmp/equivs
cd /tmp/equivs

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

dpkg-scanpackages $REPODIR >$REPODIR/Packages
apt-ftparchive release $REPODIR >$REPODIR/Release
gpg --output $REPODIR/Release.gpg -ba $REPODIR/Release

rmdir /tmp/equivs || true