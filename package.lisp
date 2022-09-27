;;;; package.lisp

(defpackage #:eye-math
  (:nicknames #:em)
  (:use #:cl)
  (:export #:first-blur
           #:acuity->diopters
           #:get-astig-tot-sph
	   #:proper-distance
	   #:proper-lens
	   #:correction-delta
	   #:convert-units
	   #:eye-diagnostics))
