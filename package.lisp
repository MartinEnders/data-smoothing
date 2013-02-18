;;;; package.lisp

(defpackage #:data-smoothing
  (:use #:cl)
  (:export #:centered-moving-average
	   #:simple-moving-average))

