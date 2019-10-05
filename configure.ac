# Process this file with autoconf to produce a configure script.
# author: Aaron Lun (infinite.monkeys.wit.keyboards@gmail.com)

AC_PREREQ([2.65])
AC_INIT([basilisk], [0.99.0], [infinite.monkeys.with.keyboards@gmail.com])
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
USER_INST_DIR="${R_PACKAGE_DIR}/inst/python"
./configure --enable-shared=yes CC="${CC}" CFLAGS="${CFLAGS}" \
    LDFLAGS="-Wl,--rpath=${USER_INST_DIR}/lib" \
    --prefix="${USER_INST_DIR}"
$MAKE
cd ../../

# Update Makevars variables.
PYTHON_DIR="${BASEPYNAME}"
AC_SUBST(PYTHON_DIR)
AC_SUBST(USER_INST_DIR)
AC_MSG_NOTICE([   PYTHON_DIR=${PYTHON_DIR}])
AC_MSG_NOTICE([   USER_INST_DIR=${USER_INST_DIR}])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
