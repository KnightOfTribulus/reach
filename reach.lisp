;;;; reach.lisp

(in-package #:reach)

;;(defparameter *net* (make-petri-net))
(defclass location ()
  ((%name :initarg :name
	  :accessor name)
   (%token-count :initarg :token-count
		 :initform 0
		 :accessor token-count)))

(defun make-location (name count)
  (make-instance 'location
		 :token-count count
		 :name name))

(defmacro deflocation (name &optional count) ;; deprecated!!!
  `(defparameter ,name (make-instance 'location
				      ,@(if count
					    `(:token-count ,count)
					    '(:token-count 0)))))

(defclass in-bit ()
  ((%from :initarg :from ;; location
	  :accessor from)
   (%takes :initarg :takes ;; number
	   :initform 1
	   :accessor takes)))

(defun make-in-bit (from takes)
  (make-instance 'in-bit
		 :from from
		 :takes takes))

(defun make-in-bits (pairs)
  (mapcar
   #'(lambda (pair)
       (make-in-bit (car pair) (cadr pair)))
   pairs))

(defclass out-bit ()
  ((%to :initarg :to ;; location
	:accessor to)
  (%gives :initarg :gives ;; number
	  :initform 1
	  :accessor gives)))

(defun make-out-bit (to gives)
  (make-instance 'out-bit
		 :to to
		 :gives gives))

(defun make-out-bits (pairs)
  (mapcar
   #'(lambda (pair)
       (make-out-bit (car pair) (cadr pair)))
   pairs))

(defclass transition ()
  ((%name :initarg :name
	  :accessor name)
   (%in-bits :initarg :in-bits
	     :accessor in-bits)
   (%out-bits :initarg :out-bits
	      :accessor out-bits)))

(defun make-transition (name in-pairs out-pairs)
  (make-instance 'transition
		 :name name
		 :in-bits (make-in-bits in-pairs) 
		 :out-bits (make-out-bits out-pairs)))

(defmethod allowed-p ((this transition))
  (when (every #'(lambda (bit)
		   (>= (token-count (from bit))
		       (takes this)))
	       (in-bits this))
    t))

(defmethod fire ((this transition))
  (when (allowed-p this)
    (with-accessors ((in-bits in-bits)
		     (out-bits out-bits))
	this
      (mapcar
       #'(lambda (inbit)
	   (decf (token-count (from inbit)) (takes inbit)))
       in-bits)
      (mapcar
       #'(lambda (outbit)
	   (incf (token-count (to outbit)) (gives outbit)))
       out-bits))))

(defclass net ()
  ((locations :initarg :locations
	      :accessor locations)
   (transitions :initarg :transitions
		:accessor transitions)))

(defun make-net (locations transitions)
  ())
