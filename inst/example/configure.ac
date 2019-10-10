# Process this file with autoconf to produce a configure script.
# author: Aaron Lun (infinite.monkeys.wit.keyboards@gmail.com)

AC_PREREQ([2.65])
AC_INIT([son.of.basilisk], [0.99.0], [infinite.monkeys.with.keyboards@gmail.com])

# Check the compiler configured with R
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
	echo "could not determine R_HOME"
    exit 1
fi

# Installing Python modules that I might need.
mkdir -p "${R_HOME}/library/son.of.basilisk/inst"
${R_HOME}/bin/R --slave --quiet -e 'basilisk::setupVirtualEnv("env1", "pandas==0.25.1", pkgname="son.of.basilisk")'
${R_HOME}/bin/R --slave --quiet -e 'basilisk::setupVirtualEnv("env2", "scikit-learn==0.21.0", pkgname="son.of.basilisk")'

AC_CONFIG_FILES
AC_OUTPUT
