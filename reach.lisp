;;;; reach.lisp

(in-package #:reach)

;;(defparameter *net* (make-petri-net))

;; the transition matrix, rows correspond to transitions, coluns correspond to positions
;;                                   p1 p2
(defparameter *transition-matrix* '((1  2)  ;t1
				    (3  4)) ;t2
  )

(defun transition-vector (i)
  (elt *transition-matrix* i))

(defun allowable-p (marking index)
  (when (every #'identity
	       (mapcar #'(lambda (m ti)
			   (>= m ti))
		       marking
		       (transition-vector index)))))

(defun fire-transition (marking index)
  (when (allowable-p marking index)
    (mapcar #'(lambda (m ti)
		 (+ m ti))
	     marking
	     (transition-vector index))))
