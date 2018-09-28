# Process this file with autoconf to produce a configure script.
# author: Aaron Lun (alun@wehi.edu.au)

AC_PREREQ([2.65])
AC_INIT([Rattlesnake], [0.99.0], [alun@wehi.edu.au])
AC_CONFIG_SRCDIR([src])

# Check the compiler configured with R
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
MAKE=`"${R_HOME}/bin/R" CMD config MAKE`

# Checks for programs.
AC_PROG_CC

# Download source files here.
# This should be the only location where versions need modification.
BASEPYNAME="Python-3.7.0"
cd src
if test -d ./${BASEPYNAME}; then 
	echo 'found' $BASEPYNAME 'sources.'
else
	echo "downloading Python 3.7.0..."
	wget https://www.python.org/ftp/python/3.7.0/Python-3.7.0.tgz
	tar -xf ${BASEPYNAME}.tgz
fi

# Building Python.
echo "building Python..."
cd ${BASEPYNAME}
./configure --with-pic --enable-shared=no CC="${CC}" CFLAGS="${CFLAGS} -w"
$MAKE
