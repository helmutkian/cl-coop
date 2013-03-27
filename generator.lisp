
(in-package #:com.helmutkian.cl-coop.generator)

;;; ***************************************************************
;;; ***************************************************************



(defclass generator ()
  ((%continuation
    :reader continuation
    :initarg :continuation
    :documentation 
    "CL-CONT::FUNCALLABLE/CC object used to implement generator")
   (%status
    :accessor status-of
    :reader deadp
    :initform nil
    :documentation 
    "Sentinel value for whether or not generator has been exhausted."))
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
	    *generator-exhausted*))
    (make-instance 'generator
		   :continuation (lambda ()
				   (call/cc state)))))

;;; ***************************************************************
;;; ***************************************************************


(defun next (the-generator)
  "Handles advancing the execution of the generator to the 
   next value to be yielded."
  (when (not (deadp the-generator))
    (let ((yield-value (funcall (continuation the-generator))))
      (when (eql yield-value *generator-exhausted*)
	(setf (status-of the-generator) t
	      yield-value nil))
      yield-value)))
  

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



