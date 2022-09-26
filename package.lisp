;;;; package.lisp

(defpackage #:eye-math
  (:nicknames #:em)
  (:use #:cl)
  (:export #:first-blur
           #:acuity->diopters
           #:get-astigmatism
	   #:view-distance
	   #:proper-distance
	   #:proper-lens
	   #:correction-delta
	   #:convert-units))
