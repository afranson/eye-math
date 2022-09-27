;;;; eye-math.lisp

(in-package #:eye-math)

(defparameter *acuity->diopter-list* '((0 1) (10 0.75) (13 0.5) (15 0.25) (20 0)
				       (25 -0.25) (30 -0.5) (40 -0.75) (50 -1)
				       (70 -1.25) (100 -1.50) (150 -2.0)
				       (200 -2.5) (250 -3) (300 -3.5) (400 -4)
				       "Points that define the approximate conversion between the best 20/x acuity to diopter correction"))

(defparameter conversions (make-hash-table) "Hash map to convert the input units into meters (or from meters to the unit if divided by)")
(setf (gethash :m conversions) 1)
(setf (gethash :cm conversions) 0.01)
(setf (gethash :in conversions) 0.0254)
(setf (gethash :ft conversions) 0.3048)

(defun interpolate (x1 y1 x2 y2 xm)
  "Linearly interpolates between point 1 (x1, y1) and point 2 (xy, y2) for some x value xm"
  (let ((slope (/ (- y2 y1) (- x2 x1))))
    (+ y1 (* slope (- xm x1)))))

(defun interpolate-from-list (list xm)
  "Linearly inpolates for the x-value xm by finding the two points in the sorted list that are closest to xm"
  (reduce
   #'(lambda (a b)
       (if (not (consp a))
	   a
	   (if (<= (elt a 0) xm (elt b 0))
	       (interpolate (elt a 0) (elt a 1) (elt b 0) (elt b 1) xm)
	       b)))
   list))


(defun first-blur (distance &optional (unit :in) &rest trash)
  "Return the diopter value if distance is the first time blur is observed"
  (declare (ignore trash))
  (unless (gethash unit conversions)
    (error "Unit not found, try ':~s'?" unit))
  (/ 1 (gethash unit conversions 1) distance))


(defun acuity->diopters (read-distance &optional (read-size-20/x 20) (chart-distance 14) (lenses 0) (unit :in) &rest trash)
  "Return the diopter value if read-distance is the first point that the text at read-size-20/x (20/20 or 20/30 for example) is readable"
  (declare (ignore trash))
  (let ((effective-acuity (* read-size-20/x chart-distance (/ read-distance))))
    (when (not (<= 15 effective-acuity 50))
      (format t "Warning: Unreliable acuity extrapolation, 20/~D~%" (floor effective-acuity)))
    (+ (abs lenses)
       (- (interpolate-from-list *acuity->diopter-list* effective-acuity))
       (first-blur read-distance unit))))


(defun proper-distance (full-prescription &optional (lenses 0) (unit :m) &rest trash)
  "Returns the max viewing distance for clear vision given a full-prescription and current worn lenses"
  (declare (ignore trash))
  (/ 1 (- full-prescription lenses) (gethash unit conversions)))


(defun proper-lens (full-prescription &optional (distance 1) (unit :m) &rest trash)
  "Returns the proper diopter value lens that should be worn to clearly see something distance away"
  (declare (ignore trash))
  (- full-prescription (/ 1 (gethash unit conversions) distance)))


(defun correction-delta (full-prescription &optional (lenses 0) (distance 1) (unit :m) &rest trash)
  "Give the diopter gap between the appropriate perscription to view something distance away with full-prescription vs the lenses currently worn"
  (declare (ignore trash))
  (- (proper-lens full-prescription distance unit) lenses))


(defun get-astig-tot-sph (total-first-to-blur-distance &optional (20/20-acuity-distance 9.75))
  "Returns the astigmatism, total correction (astig + sph), and spherical correction in diopters given a total-first-to-blur-distance along the hard axis (the astigmatism direction, and a 20/20-acuity-distance (the furthest distance 20/20 is readable from on a 14 in Snellen chart with no correction"
  (let ((sph (acuity->diopters 20/20-acuity-distance))
	(tot (first-blur total-first-to-blur-distance)))
    (list (- tot sph) tot sph)))


(defun convert-units (distance &optional (unit-from :m) (unit-to :in) &rest trash)
  "Converts between units using the conversions hash table. Does meters, inches, feet, and centimeters."
  (declare (ignore trash))
  (/ (* distance (gethash unit-from conversions)) (gethash unit-to conversions)))



;;; CLI Strings, Utilities, and Main function
(defparameter function-info (make-hash-table) "Hash table relating function names to their information")
(setf (gethash 1 function-info) '( "acuity->diopters" "read-dist" "read-size-20/x" "chart-dist" "lenses" "unit"))


(defparameter methods
  '(( "acuity->diopters" "read-dist" "read-size-20/x" "chart-dist" "lenses" "unit" )
    ( "first-blur" "distance" "unit")
    ( "astigmatism" "first-blur-ast" "acuity-read-dist")
    ( "proper-distance" "full-prescr" "lenses" "unit" )
    ( "proper-lens" "full-prescr" "dist" "unit" )
    ( "correction-delta" "full-prescr" "lenses" "dist" "unit" )
    ( "convert-units" "distance" "unit-from" "unit-to")))
(defparameter method-strings (mapcar #'(lambda (x) (format nil "~{~16a~^ ~}" x)) methods))
(defparameter methods-str (format nil "~{~a~^~%~}~%" (loop for l in method-strings
                                                           for y from 1
                                                           collect (format nil "~a: ~a" y l))))

(defun read-from-string-iff-string (input)
  (if (stringp input)
      (read-from-string input)
      input))

(defun eye-diagnostics (&rest argv)
  "CLI for determining eye parameters. Always prints help information."
  (princ methods-str)
  (if (not (nthcdr 1 argv))
      (format t "~%^ Use [more] args ^~%")
      (let* ((inputs (mapcar #'read-from-string-iff-string argv))
             (chosen-option (elt method-strings (- (elt inputs 0) 1)))
             (input-string (format nil "~a~13,,,'_:@<~a~>*~{~16,,,'_:@<~s~>*~}~%" "Inputs:" (elt inputs 0) (subseq inputs 1))))
        (case (elt inputs 0)
          (0 nil)
          ((1 2 3 4 5 6 7) (format t "~%~a: ~a~%~a~%" (elt inputs 0) chosen-option input-string))
          (t nil))
        (case (elt inputs 0)
          (0 nil)
          (1 (format t "Full Prescription: ~a Diopters~%" (apply #'acuity->diopters (subseq inputs 1))))
          (2 (format t "Full Prescription: ~a Diopters~%" (apply #'first-blur (subseq inputs 1))))
          (3 (apply (lambda (x y z) (format t "Astigmatism: ~4,2f  Total: ~4,2f  Spherical: ~4,2f Diopters~%" x y z)) (apply #'get-astig-tot-sph (subseq inputs 1))))
          (4 (format t "Proper Viewing Distance: ~a m~%" (apply #'proper-distance (subseq inputs 1))))
          (5 (format t "Proper Lenses: ~a Diopters~%" (apply #'proper-lens (subseq inputs 1))))
          (6 (format t "Correction Delta: ~a Diopters~%" (apply #'correction-delta (subseq inputs 1))))
	  (7 (format t "Converted Distance: ~a ~a~%" (apply #'convert-units (subseq inputs 1)) (elt inputs 3)))))))
