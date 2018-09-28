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
USER_INST_DIR="${R_PACKAGE_DIR}/inst/python"
./configure --enable-shared=no CC="${CC}" CFLAGS="${CFLAGS} -w" --prefix="${USER_INST_DIR}"
$MAKE
cd ../../

# Update Makevars variables.
PYTHON_DIR="${BASEPYNAME}"
PYTHON_INCLUDE="${BASEPYNAME}/Include"
PYTHON_LIB="${BASEPYNAME}/libpython3.7m.a"
AC_SUBST(PYTHON_DIR)
AC_SUBST(PYTHON_INCLUDE)
AC_SUBST(PYTHON_LIB)
AC_SUBST(USER_INST_DIR)
AC_MSG_NOTICE([   PYTHON_DIR=${PYTHON_DIR}])
AC_MSG_NOTICE([   PYTHON_INCLUDE=${PYTHON_INCLUDE}])
AC_MSG_NOTICE([   PYTHON_LIB=${PYTHON_LIB}])
AC_MSG_NOTICE([   USER_INST_DIR=${USER_INST_DIR}])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
