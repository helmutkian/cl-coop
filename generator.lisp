
(in-package #:com.helmutkian.cl-coop.generator)

;;; ***************************************************************
;;; ***************************************************************


(defclass generator-mixin () ()
  (:documentation
   "Abstract base class for GENERATORs."))

(defclass exhausted-generator (generator-mixin) ()
  (:documentation 
   "A 'null' GENERATOR that can no longer yield values."))

(defclass generator (generator-mixin)
  ((continuation
    :reader continuation
    :initarg :continuation
    :documentation 
    "CL-CONT::FUNCALLABLE/CC object used to implement generator"))
  (:documentation 
   "Wrapper for CL-CONT::FUNCALLABLE/CC in order to provide an
    external GENERATOR protocol for FUNCALLABLE objects")) 

;;; ***************************************************************
;;; ***************************************************************                    
(defparameter *generator-exhausted* (gensym)
  "Symbol which represents when GENERATOR which has reached the
   end of its execution.")

(defun/cc make-generator (function)
  "Constructor for GENERATOR.
   Takes a closure formed within a dynamic CL-CONT:WITH-CALL/CC 
   environment whose only argument represents the calling 
   continuation to be yielded to and returns a GENERATOR object."
  (let (state)
    (setf state
	  (lambda (k)
	    (funcall function
		     (lambda (result)
		       (setf k
			     (let/cc resume
			       (setf state resume)
			       (funcall k result)))))
	    (funcall k nil)
	    *generator-exhausted*
	    ))
    (make-instance 'generator
		   :continuation (lambda () 
				   (call/cc state)))))

;;; ***************************************************************
;;; ***************************************************************

(defgeneric next (generator)
  (:documentation 
   "Handles advancing the execution of 
    the generator to the  next value to be yielded."))

(defmethod next ((generator generator))
  (let ((yield-value (funcall (continuation generator))))
    (if (eql yield-value *generator-exhausted*)
	(values nil (change-class generator 'exhausted-generator))
	(values yield-value generator))))

(defmethod next ((generator exhausted-generator))
  (values nil generator))

;;; ***************************************************************
;;; ***************************************************************

(defgeneric exhaustedp (generator)
  (:documentation 
   "Determines whether a GENERATOR has exhausted its execution and
    can no longer yield values."))
  
(defmethod exhaustedp ((generator generator))
  (declare (ignore generator))
  nil)

(defmethod exhaustedp ((generator exhausted-generator))
  (declare (ignore generator))
  t)
 
;;; ***************************************************************
;;; ***************************************************************

(defmacro with-generator (&body body)
  "Macro which wraps it's body within a dynamic 
   CL-CONT:WITH-CALL/CC environment closure and constructs a
   GENERATOR thereof. The macro provides a local macro YIELD which
   yields a given value to the calling environment."
  (let ((yield-sym (gensym)))
    `(make-generator
       (lambda/cc (,yield-sym)
	 (macrolet 
	     ((yield (&rest args) `(funcall ,',yield-sym ,@args)))
	   ,@body))))))



