;;;; eye-math.lisp

(in-package #:eye-math)

;;; Eye math foundational objects/functions
(defparameter *acuity->diopter-list* '((0 1) (10 0.75) (13 0.5) (15 0.25) (20 0)
				       (25 -0.25) (30 -0.5) (40 -0.75) (50 -1)
				       (70 -1.25) (100 -1.50) (150 -2.0)
				       (200 -2.5) (250 -3) (300 -3.5) (400 -4))
  "Points that define the approximate conversion between the best 20/x acuity to diopter correction")

(defparameter conversions (make-hash-table) "Hash map to convert the input units into meters (or from meters to the unit if divided by)")
(setf (gethash :m conversions) 1)
(setf (gethash :cm conversions) 0.01)
(setf (gethash :in conversions) 0.0254)
(setf (gethash :ft conversions) 0.3048)

(defun range (n)
  (assert (> n 0) (n))
  (do ((m 0 (1+ m))
       (ret-list nil (cons m ret-list)))
      ((= n m) (reverse ret-list))))

(defun check-for-num (arg)
  "Checks if arg is a number, otherwise print helpful message and gives safe default"
  (if (numberp arg)
      arg
      (progn (format t "Invalid entry, '~a' should be a number. Defaulting to 1.~%" arg)
	     1)))

(defun get-conversion (unit)
  "Checks if unit is in the conversions hash table and prints helpful message if it is not with safe default returned"
  (let ((conv (gethash unit conversions)))
    (if conv
	conv
	(progn (format t "Bad unit was entered -> '~s'. Use one of :m :cm :in :ft. Defaulting to ':m', meters.~%" unit)
	       1))))

(defun interpolate-points (p1 p2 xm)
  "Linearly interpolates between point 1 (x1, y1) and point 2 (xy, y2) to extract ym for some x value xm"
  (let* ((x1 (car p1))
	 (y1 (cadr p1))
	 (x2 (car p2))
	 (y2 (cadr p2))
	 (slope (/ (- y2 y1) (- x2 x1))))
    (+ y1 (* slope (- xm x1)))))

(defun between-points (p1 x p2 &optional (xkey #'first))
  (let ((x1 (funcall xkey p1))
	(x2 (funcall xkey p2)))
    (or (<= x1 x x2) (<= x2 x x1))))

(defun interpolate-from-list (list xm &key (pos-keys (list #'first #'second)))
  "Linearly inpolates for the x value xm by finding the two points in the list that are closest to xm. Use sort-key to determine how the list is sorted (or not).
xkey function extract the x value from the structure, ykey the y value."
  (let* ((list (sort (copy-list list) #'< :key (elt pos-keys 0)))
	 (list (mapcar #'(lambda (x) (list (funcall (elt pos-keys 0) x) (funcall (elt pos-keys 1) x))) list)))
    ;; list is now forced a be an sorted list of points with x first and y second
    (if (< xm (caar list))
	(interpolate-points (car list) (cadr list) xm)
	(if (> xm (caar (last list)))
	    (interpolate-points (car (reverse list)) (cadr (reverse list)) xm)
	    (reduce
	     #'(lambda (a b)
		 (if (not (consp a))
		     a
		     (if (between-points a xm b)
			 (interpolate-points a b xm)
			 b)))
	     list)))))



;;; The mathematical relations that do the work as well as the table to store for easy reference
(defparameter *full-methods* nil "Lookup table for function, parameters, and formatting output")

(defmacro defeyefun (name lambda-list must-be-nums format-style &body body)
  "Generates the function described in 'body' and uses 'check-for-num' function to verify the status of the 'must-be-num' var names. Then adds the (function args format-style) list to the *full-methods* global var for later use."
  (labels ((check-for-doc-and-declare (body)
	     (cond ((and (stringp (car body)) (eq (caadr body) 'declare)) 2)
		   ((and (stringp (car body)) (not (eq (caadr body) 'declare))) 1)
		   ((eq (caar body) 'declare) 1)
		   (t 0)))
	   (lambda-to-arg-names (lambda-item)
	     (if (member lambda-item '(&optional &key &rest trash))
		 nil
		 (list (sb-unicode:lowercase (if (consp lambda-item)
						 (symbol-name (car lambda-item))
						 (symbol-name lambda-item)))))))
    `(progn
       (defun ,name ,lambda-list
	 ,@(append (subseq body 0 (check-for-doc-and-declare body))
		   (list `(let ,(mapcar #'(lambda (y) `(,y (check-for-num ,y))) must-be-nums)
			    ,@(subseq body (check-for-doc-and-declare body))))))
       (push (list #',name (list ,(sb-unicode:lowercase (symbol-name name)) ,@(mapcan #'lambda-to-arg-names lambda-list)) ,format-style) *full-methods*))))


(defeyefun first-blur (distance &optional (unit :in) &rest trash)
    (distance)
    (lambda (x) (format t "Full Prescription: ~a Diopters~%" x))
  "Return the diopter value if distance is the first time blur is observed"
  (declare (ignore trash))
  (/ 1 (get-conversion unit) distance))

(defeyefun acuity->diopters (read-distance &optional (read-size-20/x 20) (chart-distance 14) (lenses 0) (unit :in) &rest trash)
    (read-distance read-size-20/x chart-distance lenses)
    (lambda (x) (format t "Full Prescription: ~a Diopters~%" x))
  "Return the diopter value if read-distance is the first point that the text at read-size-20/x (20/20 or 20/30 for example) is readable"
  (declare (ignore trash))
  (let ((effective-acuity (* read-size-20/x chart-distance (/ read-distance))))
    (when (not (<= 15 effective-acuity 50))
      (format t "Warning: Unreliable acuity extrapolation, 20/~D~%" (floor effective-acuity)))
    (+ (abs lenses)
       (- (interpolate-from-list *acuity->diopter-list* effective-acuity))
       (first-blur read-distance unit))))

(defeyefun proper-distance (full-prescription &optional (lenses 0) (unit :m) &rest trash)
    (full-prescription lenses)
    (lambda (x y) (format t "Proper Viewing Distance: ~a ~a~%" x y))
  "Returns the max viewing distance for clear vision given a full-prescription and current worn lenses"
  (declare (ignore trash))
  (list (/ 1 (- full-prescription lenses) (get-conversion unit)) unit))

(defeyefun proper-lens (full-prescription &optional (distance 1) (unit :m) &rest trash)
    (full-prescription distance)
    (lambda (x) (format t "Proper Lenses: ~a Diopters~%" x))
  "Returns the proper diopter value lens that should be worn to clearly see something distance away"
  (declare (ignore trash))
  (- full-prescription (/ 1 (get-conversion unit) distance)))

(defeyefun correction-delta (full-prescription &optional (lenses 0) (distance 1) (unit :m) &rest trash)
    (full-prescription lenses distance)
    (lambda (x) (format t "Correction Delta: ~a Diopters~%" x))
  "Give the diopter gap between the appropriate perscription to view something distance away with full-prescription vs the lenses currently worn"
  (declare (ignore trash))
  (- (proper-lens full-prescription distance unit) lenses))


(defeyefun get-all (total-first-to-blur-distance &optional (20/20-acuity-distance 9.75))
    (total-first-to-blur-distance 20/20-acuity-distance)
    (lambda (x y z a) (format t "Total: ~4,2f  Center: ~4,2f  Spherical: ~4,2f  Astigmatism: ~4,2f Diopters~%" x y z a))
  "Returns the astigmatism, total correction (astig + sph), and spherical correction in diopters given a total-first-to-blur-distance along the hard axis (the astigmatism direction, and a 20/20-acuity-distance (the furthest distance 20/20 is readable from on a 14 in Snellen chart with no correction"
  (let ((sph (acuity->diopters 20/20-acuity-distance))
	(tot (first-blur total-first-to-blur-distance)))
    (list tot (/ (+ tot sph) 2) sph (- tot sph)))) ;; total, center, spherical, astig

(defeyefun convert-units (distance &optional (unit-from :m) (unit-to :in) &rest trash)
    (distance)
    (lambda (x y) (format t "Converted Distance: ~a ~a~%" x y))
  "Converts between units using the conversions hash table. Does meters, inches, feet, and centimeters."
  (declare (ignore trash))
  (list (/ (* distance (get-conversion unit-from)) (get-conversion unit-to)) unit-to))

(defeyefun diopters->acuity (full-prescription &optional (distance 14) (unit :in) (lenses 0))
    (full-prescription distance lenses)
    (lambda (x) (format t "Reading Acuity: 20/~a~%" (round x)))
  "Converts a reading environment into an expected acuity value"
  (interpolate-from-list *acuity->diopter-list* (- (correction-delta full-prescription lenses distance unit)) :pos-keys (list #'second #'first)))

(defeyefun nearest-safe-dist (lenses &optional (full-prescription 4.75) (unit :cm) (optical-range 6))
    (lenses full-prescription optical-range)
    (list (lambda (x y) (format t "Minimum usable distance: ~a ~a~%" x y)))
  "Returns the minimum viable viewing distance given a prescription and lens strength. Anything closer than this will guarantee a myopic stimulus."
  (list (/ 1 (+ (- lenses) full-prescription optical-range) (get-conversion unit))
	unit))

;;; CLI Strings, Utilities, and Main function
(defun make-methods (funcs)
  "Specify the functions you want in methods and in what order you want them as (list #'func1 #'func2 ...)" 
  (mapcar #'(lambda (x)
	      (find x *full-methods* :test #'(lambda (y z) (eq y (car z)))))
	  funcs))

(defun get-method-strings (methods)
  "Get and format all the arg strings from the methods as made with 'make-methods'"
  (mapcar #'(lambda (x)
	      (format nil "~{~17a~^ ~}" (elt (elt methods x) 1)))
	  (range (length methods))))

(defun get-methods-str (methods)
  "Create a single string of the args for functions in methods as made by 'make-methods'"
  (format nil "~{~a~^~%~}~%" (loop for l in (get-method-strings methods)
				   for y from 1
				   collect (format nil "~a: ~a" y l))))

(defun read-from-string-iff-string (input)
  "Read value in string with parser if and only if it is a string"
  (if (stringp input)
      (read-from-string input)
      input))

(defun force-list (input)
  "Returns a list, no exceptions"
  (if (consp input)
      input
      (list input)))

(defun get-func-from-methods (methods inputs)
  "Extract the function from the function-info hash table (specified by 0th element of inputs)"
  (elt (elt methods (1- (elt inputs 0))) 0))

(defun get-format-from-methods (methods inputs)
  "Extract the format for printing the result of the function in the function-info hash table (specified by 0th element of inputs)"
  (elt (elt methods (1- (elt inputs 0))) 2))

(defun apply-func-to-inputs (methods inputs)
  "Get result of the function in hash table applied to the inputs (1st to last)"
  (apply (get-func-from-methods methods inputs) (subseq inputs 1)))

(defun eye-diagnostics (&rest argv)
  "CLI for determining eye parameters. Always prints help information."
  (let ((methods (make-methods (list #'acuity->diopters
				     #'diopters->acuity
				     #'first-blur
				     #'get-all
				     #'proper-distance
				     #'nearest-safe-dist
				     #'proper-lens
				     #'correction-delta
				     #'convert-units))))
    (princ (get-methods-str methods)))
  (if (not argv) ;; no arguments
      (format t "~%^ Use [more] args ^~%")
    (let* ((inputs (mapcar #'read-from-string-iff-string argv))
	   (chosen-option (elt (get-method-strings methods) (- (elt inputs 0) 1)))
	   (input-string (format nil "~a~14,,,'_:@<~a~>*~{~17,,,'_:@<~s~>*~}~%" "Inputs:"
				 (elt inputs 0)
				 (subseq inputs 1))))
      (format t "~%~a: ~a~%~a~%"
	      (elt inputs 0)
	      chosen-option
	      input-string)
      (when (nthcdr 1 argv) ;; only perform function if args are given
	(format t "~60,,,'-a~%" "")
	(apply (get-format-from-methods methods inputs)
	       (force-list (apply-func-to-inputs methods inputs)))
	(format t "~60,,,'-a~%" "")))))


;; TODO Create abort handler to prevent errors dumping users into lisp debugger
(defun eye-diagnostics-exec ()
  "CLI for determining eye parameters. Always prints help information. Serves as entry point for binary creation."
  (let ((argv (uiop:command-line-arguments)))
    (apply #'eye-diagnostics argv)))
