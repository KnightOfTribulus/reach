;;;; reach.lisp

(in-package #:reach)

;;(defparameter *net* (make-petri-net))

;; the transition matrix, rows correspond to transitions, coluns correspond to positions
;;                                        p1 p2
(defparameter *transition-matrix-test* '((1  -1)  ;t1
					 (-1  1)) ;t2
  )

(defun transition-vector (*transition-matrix* i)
  (elt *transition-matrix* i))

(defun permitted-p (marking transition)
  (every #'identity
	 (mapcar #'(lambda (m ti)
		     (>= m (- ti)))
		 marking
		 transition)))

(defun fire-transition (marking transition)
  (when (permitted-p marking transition)
    (mapcar #'(lambda (m ti)
		 (+ m ti))
	     marking
	     transition)))

(defun rec-helper (matrix depth marking &key (max-depth 5)) ;;ok
  (if (<= depth max-depth)
      (cons marking
	    ;; list
	    (mapcar #'(lambda (transition) (rec-helper matrix 
						  (+ 1 depth)
						  (fire-transition marking transition)))
		    (remove-if-not #'(lambda (transition) (permitted-p marking transition))
				   matrix)))
      ;;nil
      nil))

(defun !rec-helper (matrix depth marking &key (derived-by 'initial) (max-depth 5)) ;;very bad
  (if (<= depth max-depth)
      (cons (list marking derived-by) 
	    ;; list
	    (remove-if-not #'identity (mapi #'(lambda (transition indx)
			 (when (permitted-p marking transition)
			   (!rec-helper matrix 
					(+ 1 depth)
					(fire-transition marking transition)
					:derived-by (intern (format nil "T~A" indx)))))
		     matrix)))
      ;;nil
      nil))

(defun print-reachability-tree (matrix marking &key (max-depth 5) (format :plot))
  (match format
    (:plot
     (format-tree t (!rec-helper matrix 0 marking :max-depth max-depth)
		  :layout :down))
    (:bare-verbose
     (format t "~&~A" (!rec-helper matrix 0 marking :max-depth max-depth)))
    (:bare-brief
     (format t "~&~A"(rec-helper matrix 0 marking)))))

;; (cond format
;; 	((eql format :plot)
;; 	 (!rec-helper matrix 0 :max-depth max-depth)))

;;                   p1 p2 p3 p4
(defparameter *D* '((-1 1  -1  1)  ;; t1
		    ( 0 0   1 -1)) ;; t2
  )

(defparameter *mu-0-short* '(1 0 1 0))

(defparameter *mu-0* '(1 1 1 1))

;;(print-reachability-tree *D* *mu-0*)
