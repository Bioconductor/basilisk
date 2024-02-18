#!/bin/bash
set -x
PKG="$1"
BIOC_VERSION="$2"
LOGS_DIR="$3"

mkdir -p "$LOGS_DIR"
echo "$PKG starting" >> $LOGS_DIR/$PKG-deb.log
dh-make-R --repo bioc &>> $LOGS_DIR/$PKG-deb.log
dh-update-R &>> $LOGS_DIR/$PKG-deb.log
install_cmd="apt-get -o Debug::pkgProblemResolver=yes --no-install-recommends -y"
mk-build-deps --install --tool "${install_cmd}" debian/control
[[ "$PKG" == "BiocVersion" ]] && sed -i "/Suggests:/ a\Provides: r-api-bioc-$BIOC_VERSION" debian/control || sed -i "s/Depends:/Depends: r-bioc-$BIOC_VERSION,/" debian/control
sed -i 's/Maintainer: Debian R Packages Maintainers <r-pkg-team@alioth-lists.debian.net>/Maintainer: Bioc GHA Build <maintainer@bioconductor.org>/' debian/control
apt-get -y build-dep . &>> $LOGS_DIR/$PKG-deb.log
dpkg-buildpackage -us -uc -d -b &>> $LOGS_DIR/$PKG-deb.log
cat $LOGS_DIR/$PKG-deb.log
ls ../r-*-$(echo $PKG | tr '[:upper:]' '[:lower:]')_*.deb | awk -F'/' '{print $NF}' > $LOGS_DIR/$PKG-debname.log
