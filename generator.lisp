
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

   MAKE-GENERATOR closure/cc => generator

   ***** ARGUMENTS & VALUES *****

   closure/cc -- closure over a CL-CONT:WITH-CALL/CC environment 
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

   Returns a GENERATOR object which can be suspended and resumed
   with respect to the closure's calls to its yielded-to 
   continuation.

   ***** EXAMPLE *****

   ;;; WITHOUT additional argument

   (defvar *g0*
     (make-generator 
       (lambda/cc (yield-cont)
         (funcall yield-cont 1)
         (funcall yield-cont 2)
         (funcall yield-cont 3))))
   => <GENERATOR>

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
    => <GENERATOR>

   (next *g1* 1)
   => 2
   (next *g1* 5)
   => 6
   (next *g1* 12)
   NIL"
  (let (local-state return/resume)
    ;;; LET (var ...) (SETF var ...) idiom used because mutual
    ;;; referencing in LABELS using CL-CONT:WITH-CALL/CC is 
    ;;; problematic
    (setf local-state
	  (lambda (&rest arg)
	    (apply function return/resume arg)
	    *generator-exhausted*)
          return/resume
	  (lambda (&rest arg)
	    (let/cc cc
	      (let ((old-state local-state))
		(setf local-state cc)
		(apply old-state arg)))))
    (make-instance 'generator
		   :continuation return/resume)))
;;; ***************************************************************
;;; ***************************************************************


(defun next (the-generator &rest arg)
  "***** SYNTAX *****

   NEXT the-generator [arg] => value

   ***** ARGUMENTS & VALUES *****

   the-generator -- generator whose execution is to advanced
                    to the next yield
   arg           -- optional argument to be passed to the generator
                    upon its resumption
   value         -- value yielded by generator

   ***** DESCRIPTION *****

   Handles advancing the execution of the given generator to the 
   next value to be yielded. 

   Passes an additional optional argument to the generator and
   returns the yielded value of the generator.

   Returns NIL if the generator has reached the end of its
   execution.

   ***** EXAMPLE *****

   See EXAMPLE section for MAKE-GENERATOR"
  (when (not (deadp the-generator))
    (let ((yield-value (apply (continuation the-generator) arg)))
      (when (eql yield-value *generator-exhausted*)
	(setf (status-of the-generator) t
	      yield-value nil))
      yield-value)))
  

 ;;; ***************************************************************
;;; ***************************************************************

(defmacro with-generator (arg &body body)
  "***** SYNTAX *****

   WITH-GENERATOR (arg) body* => generator

   ***** ARGUMENT & VALUES *****

   arg -- A list of up-to one element naming the optional
          initial argument of the generator
   body -- Implicit PROGN within a closure over a
           CL-CONT:WITH-CALL/CC environment
   generator -- GENERATOR object

   ***** DESCRIPTION *****

   Macro which wraps it's body within a
   CL-CONT:WITH-CALL/CC environment closure and constructs a
   GENERATOR thereof. The macro provides a local macro YIELD which
   yields a given value to the calling environment.

   ***** EXAMPLE *****
   
   ;;; WITHOUT additional argument

   (defvar *g0*
     (with-generator ()
         (yield 1)
         (yield 2)
         (yield 3))))
   => <GENERATOR>

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
     (with-generator (arg)
         (let (resume-value)
           (setf resume-value (yield (1+ arg))
                 resume-value (yield (1+ resume-value))))))
    => <GENERATOR>

   (next *g1* 1)
   => 2
   (next *g1* 5)
   => 6
   (next *g1* 12)
   NIL"
  (let ((yield-sym (gensym)))
    `(make-generator
       (lambda/cc (,yield-sym ,@arg)
	 (macrolet 
	     ((yield (&optional value) `(funcall ,',yield-sym value)))
	   ,@body)))))



