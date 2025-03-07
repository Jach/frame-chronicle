#!/bin/sh
# Meant for CI/CD systems to get the latest fresh SBCL, load the system, and run its tests.
if [ -z $WORKSPACE ]; then
  WORKSPACE=/tmp/
fi
cd $WORKSPACE
wget https://sourceforge.net/projects/sbcl/files/latest/download -O source-latest.tar.bz2
tar axf source-latest.tar.bz2
v=$(ls -d sbcl-*)
vbin="$v-x86-64-linux"
wget "https://sourceforge.net/projects/sbcl/files/${v/-//}/$vbin-binary.tar.bz2/download" -O $vbin.tar.bz2
tar axf $vbin.tar.bz2
wget https://beta.quicklisp.org/quicklisp.lisp
$vbin/run-sbcl.sh --load quicklisp.lisp --eval '(quicklisp-quickstart:install :path "./")' --quit
TEST_SYSTEM='com.thejach.frame-chronicle/test'
echo '(load "setup.lisp")
(ql:quickload "'$TEST_SYSTEM'")
(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))
(let ((result (5am:run-all-tests :summary :suite)))
  (sb-cover:report "coverage/")
  (if result (uiop:quit 0) (uiop:quit 1)))' > cover.lisp
$vbin/run-sbcl.sh --script cover.lisp

