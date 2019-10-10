# Process this file with autoconf to produce a configure script.
# author: Aaron Lun (infinite.monkeys.wit.keyboards@gmail.com)

AC_PREREQ([2.65])
AC_INIT([basilisk], [0.99.0], [infinite.monkeys.with.keyboards@gmail.com])
AC_CONFIG_SRCDIR([src])

# Check the compiler configured with R
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
	AC_MSG_ERROR([  could not determine R_HOME], [1])
fi

# Download source files here.
# This should be the only location where versions need modification.
BASEPYNAME="Python-3.7.0"
cd src
if test -d ./${BASEPYNAME}; then 
	AC_MSG_NOTICE([  found $BASEPYNAME sources])
else
	AC_MSG_NOTICE([  downloading $BASEPYNAME sources])
	curl https://www.python.org/ftp/python/3.7.0/Python-3.7.0.tgz > Python-3.7.0.tgz
	tar -xf ${BASEPYNAME}.tgz
fi

USER_INST_DIR="${R_PACKAGE_DIR}/inst/python"

# Switching hard-coded paths between clang and gcc.
CC=`"${R_HOME}/bin/R" CMD config CC`
AC_PROG_CC
TESTFLAGS="-Wl,-rpath,${USER_INST_DIR}/lib"
AX_CHECK_COMPILE_FLAG([$TESTFLAGS], [EXTRAFLAGS="$TESTFLAGS"], [EXTRAFLAGS="-Wl,-rpath ${USER_INST_DIR}/lib"])

# Update Makevars variables.
PYTHON_DIR="${BASEPYNAME}"
AC_SUBST(PYTHON_DIR)
AC_SUBST(USER_INST_DIR)
AC_SUBST(EXTRAFLAGS)
AC_MSG_NOTICE([   PYTHON_DIR=${PYTHON_DIR}])
AC_MSG_NOTICE([   USER_INST_DIR=${USER_INST_DIR}])
AC_MSG_NOTICE([   EXTRAFLAGS=${EXTRAFLAGS}])
AC_CONFIG_FILES([Makevars])
AC_OUTPUT
