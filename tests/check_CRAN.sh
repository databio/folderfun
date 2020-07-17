#!/bin/bash
pkgroot=$(dirname $PWD)
R CMD build $pkgroot
buildfile=$(basename $pkgroot)_$1.tar.gz
R CMD check --as-cran $buildfile
rm $buildfile

