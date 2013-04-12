
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
  "***** SYNTAX *****

   MAKE-GENERATOR (function) => generator

   ***** ARGUMENTS & VALUES *****

   function -- closure over a CL-CONT:WITH-CALL/CC environment 
               that takes at least one argument
   generator -- GENERATOR object  


   ***** DESCRIPTION *****

   Constructor for GENERATOR.

   Takes a closure over a CL-CONT:WITH-CALL/CC 
   environment with a mandatory first argument represents the
   continuation yielded to when the GENERATOR's execution is 
   suspended. The closure may optionally take a second argument 
   representing the value passed to the GENERATOR upon the
   resumption of its execution.

   ***** EXAMPLE *****

   ;;; WITHOUT additional argument

   (defvar *g0*
     (make-generator 
       (lambda/cc (yield-cont)
         (funcall yield-cont 1)
         (funcall yield-cont 2)
         (funcall yield-cont 3))))

   (next *g0*)
   => 1
   (next *g0*)
   => 2
   (next *g0*)
   => 3
   (next *g0*)
   => NIL

   ;;; WITH additional argument

   (defvar *g1*
     (make-generator 
       (lambda/cc (yield-cont arg)
         (let (resume-value)
           (setf resume-value (funcall yield-cont 
                                       (1+ arg))
                 resume-value (funcall yield-cont 
                                       (1+ resume-value)))))))

   (next *g1* 1)
   => 2
   (next *g1* 5)
   => 6
   (next *g1* 12)
   NIL"
  (let (local-state return/resume)
    (setf local-state
	  (lambda (&rest args)
	    (apply function return/resume args)
	    *generator-exhausted*))
    (setf return/resume
	  (lambda (&rest args)
	    (let/cc cc
	      (let ((old-state local-state))
		(setf local-state cc)
		(apply old-state args)))))
    (make-instance 'generator
		   :continuation return/resume)))
;;; ***************************************************************
;;; ***************************************************************


(defun next (the-generator &rest args)
  "Handles advancing the execution of the generator to the 
   next value to be yielded."
  (when (not (deadp the-generator))
    (let ((yield-value (apply (continuation the-generator) args)))
      (when (eql yield-value *generator-exhausted*)
	(setf (status-of the-generator) t
	      yield-value nil))
      yield-value)))
  

 ;;; ***************************************************************
;;; ***************************************************************

(defmacro with-generator (lambda-list &body body)
  "Macro which wraps it's body within a dynamic 
   CL-CONT:WITH-CALL/CC environment closure and constructs a
   GENERATOR thereof. The macro provides a local macro YIELD which
   yields a given value to the calling environment."
  (let ((yield-sym (gensym)))
    `(make-generator
       (lambda/cc (,yield-sym ,@lambda-list)
	 (macrolet 
	     ((yield (&rest args) `(funcall ,',yield-sym ,@args)))
	   ,@body)))))



