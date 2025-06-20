#|
Run tests + generate coverage report, use sbcl --script coverage.lisp
|#
(in-package #:cl-user)
(defvar *system* "com.thejach.frame-chronicle")
(defvar *test-system* (uiop:strcat *system* "/test"))

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload *system* :silent t)
(ql:quickload *test-system* :silent t)

#+sbcl
(require :sb-cover)
#+sbcl
(declaim (optimize sb-cover:store-coverage-data))

(let ((*compile-verbose* nil)
      (*load-verbose* nil))
  (asdf:load-system *system* :force t)

  (asdf:test-system *system*))

#+sbcl
(handler-bind ((warning #'muffle-warning))
  (sb-cover:report "coverage/"))
