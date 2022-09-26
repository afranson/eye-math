;;;; eye-math.lisp

(in-package #:eye-math)

(defparameter *acuity->diopter-list* '((0 1) (10 0.75) (13 0.5) (15 0.25) (20 0)
				       (25 -0.25) (30 -0.5) (40 -0.75) (50 -1)
				       (70 -1.25) (100 -1.50) (150 -2.0)
				       (200 -2.5) (250 -3) (300 -3.5) (400 -4)))

(defparameter conversions (make-hash-table))
(setf (gethash :m conversions) 1)
(setf (gethash :cm conversions) 0.01)
(setf (gethash :in conversions) 0.0254)

(defun interpolate (x1 y1 x2 y2 xm)
  (let ((slope (/ (- y2 y1) (- x2 x1))))
    (+ y1 (* slope (- xm x1)))))

(defun interpolate-from-list (list xm)
  "Must be a sorted list."
  (reduce
   #'(lambda (a b)
       (if (not (consp a))
	   a
	   (if (<= (elt a 0) xm (elt b 0))
	       (interpolate (elt a 0) (elt a 1) (elt b 0) (elt b 1) xm)
	       b)))
   list))


(defun first-blur (distance &optional (unit :in) &rest trash)
  (declare (ignore trash))
  (unless (gethash unit conversions)
    (error "Unit not found, try ':~s'?" unit))
  (/ 1 (gethash unit conversions 1) distance))


(defun acuity->diopters (read-distance &optional (read-size-x/20 20) (chart-distance 14) (lenses 0) (unit :in) &rest trash)
  (declare (ignore trash))
  (let ((effective-acuity (* read-size-x/20 chart-distance (/ read-distance))))
    (when (not (<= 15 effective-acuity 50))
      (format t "Warning: Unreliable acuity extrapolation, 20/~D~%" (floor effective-acuity)))
    (+ (abs lenses)
       (- (interpolate-from-list *acuity->diopter-list* effective-acuity))
       (first-blur read-distance unit))))


(defun view-distance (full-prescription lenses &key (unit :m))
  (/ 1 (- full-prescription lenses) (gethash unit conversions)))


(defun proper-distance (full-prescription &optional (lenses 0) (unit :m) &rest trash)
  (declare (ignore trash))
  (view-distance full-prescription lenses :unit unit))


(defun proper-lens (full-prescription &optional (distance 1) (unit :m) &rest trash)
  (declare (ignore trash))
  (- full-prescription (/ 1 (gethash unit conversions) distance)))


(defun correction-delta (full-prescription &optional (lenses 0) (distance 1) (unit :m) &rest trash)
  (declare (ignore trash))
  (- (proper-lens full-prescription distance unit) lenses))


(defun get-astigmatism (total-first-to-blur-distance x/20-acuity-distance)
  (let ((sph (acuity->diopters x/20-acuity-distance))
	(tot (first-blur total-first-to-blur-distance)))
    (list (- tot sph) tot sph)))


(defun convert-units (distance &optional (unit-from :m) (unit-to :in) &rest trash)
  (declare (ignore trash))
  (/ (* distance (gethash unit-from conversions)) (gethash unit-to conversions)))
