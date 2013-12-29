
(in-package #:com.helmutkian.cl-coop)

;;; ***************************************************************
;;; ***************************************************************


(defclass coroutine ()
  ((%continuation
    :reader continuation
    :initarg :continuation
    :documentation 
    "CL-CONT::FUNCALLABLE/CC object used to implement coroutine")
   (%status
    :accessor status-of
    :initform nil
    :documentation 
    "Sentinel value for whether or not coroutine has been exhausted."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation 
   "Funcallable wrapper for CL-CONT::FUNCALLABLE/CC in order to provide an
   external COROUTINE protocol for FUNCALLABLE objects"))

(defmethod initialize-instance :after ((coro coroutine) &key)
  "Calling FUNCALL on a COOP:COROUTINE is identical to calling COOP:NEXT."
  (closer-mop:set-funcallable-instance-function
   coro
   (lambda (&rest args)
     (apply #'next coro args))))

;;; ***************************************************************
;;; ***************************************************************                    

(defparameter *coroutine-exhausted* (gensym)
  "Symbol which represents when COROUTINE which has reached the
   end of its execution.")

(defun/cc make-coroutine (function)
  "***** SYNTAX *****

   MAKE-COROUTINE closure/cc => coroutine

   ***** ARGUMENTS & VALUES *****

   closure/cc -- closure over a CL-CONT:WITH-CALL/CC environment 
                 that takes at least one argument
   coroutine -- COROUTINE object  


   ***** DESCRIPTION *****

   Constructor for COROUTINE.

   Takes a closure over a CL-CONT:WITH-CALL/CC 
   environment with a mandatory first argument represents the
   continuation yielded to when the COROUTINE's execution is 
   suspended. The closure may optionally take a second argument 
   representing the value passed to the COROUTINE upon the
   resumption of its execution.

   Returns a COROUTINE object which can be suspended and resumed
   with respect to the closure's calls to its yielded-to 
   continuation.

   ***** EXAMPLE *****

   ;;; WITHOUT additional argument

   (defvar *g0*
     (make-coroutine 
       (lambda/cc (yield-cont)
         (funcall yield-cont 1)
         (funcall yield-cont 2)
         (funcall yield-cont 3))))
   => <COROUTINE>

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
     (make-coroutine 
       (lambda/cc (yield-cont arg)
         (let (resume-value)
           (setf resume-value (funcall yield-cont 
                                       (1+ arg))
                 resume-value (funcall yield-cont 
                                       (1+ resume-value)))))))
    => <COROUTINE>

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
	    *coroutine-exhausted*)
          return/resume
	  (lambda (&rest arg)
	    (let/cc cc
	      (let ((old-state local-state))
		(setf local-state cc)
		(apply old-state arg)))))
    (make-instance 'coroutine
		   :continuation return/resume)))

;;; ***************************************************************
;;; ***************************************************************

(defgeneric deadp (routine))

(defmethod deadp ((routine function))
  (declare (ignore routine))
  t)

(defmethod deadp ((routine coroutine))
  (status-of routine))

;;; ***************************************************************
;;; ***************************************************************

(defgeneric next (routine &rest arg))

(defmethod next ((routine function) &rest arg)
  (apply routine arg))

(defmethod next ((routine coroutine) &rest arg)
  "***** SYNTAX *****

   NEXT routine [arg] => value

   ***** ARGUMENTS & VALUES *****

   routine -- coroutine whose execution is to advanced
                    to the next yield
   arg           -- optional argument to be passed to the coroutine
                    upon its resumption
   value         -- value yielded by coroutine

   ***** DESCRIPTION *****

   Handles advancing the execution of the given coroutine to the 
   next value to be yielded. 

   Passes an additional optional argument to the coroutine and
   returns the yielded value of the coroutine.

   Returns NIL if the coroutine has reached the end of its
   execution.

   ***** EXAMPLE *****

   See EXAMPLE section for MAKE-COROUTINE"
  (when (not (deadp routine))
    (let ((yield-value (apply (continuation routine) arg)))
      (when (eql yield-value *coroutine-exhausted*)
	(setf (status-of routine) t
	      yield-value nil))
      yield-value)))
  

;;; ***************************************************************
;;; ***************************************************************

(defmacro with-coroutine (arg &body body)
  "***** SYNTAX *****

   WITH-COROUTINE (arg) body* => coroutine

   ***** ARGUMENT & VALUES *****

   arg -- A list of up-to one element naming the optional
          initial argument of the coroutine
   body -- Implicit PROGN within a closure over a
           CL-CONT:WITH-CALL/CC environment
   coroutine -- COROUTINE object

   ***** DESCRIPTION *****

   Macro which wraps it's body within a
   CL-CONT:WITH-CALL/CC environment closure and constructs a
   COROUTINE thereof. The macro provides a local macro YIELD which
   yields a given value to the calling environment.

   ***** EXAMPLE *****
   
   ;;; WITHOUT additional argument

   (defvar *g0*
     (with-coroutine ()
         (yield 1)
         (yield 2)
         (yield 3))))
   => <COROUTINE>

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
     (with-coroutine (arg)
         (let (resume-value)
           (setf resume-value (yield (1+ arg))
                 resume-value (yield (1+ resume-value))))))
    => <COROUTINE>

   (next *g1* 1)
   => 2
   (next *g1* 5)
   => 6
   (next *g1* 12)
   NIL"
  (let ((yield-sym (gensym))
	(value (gensym)))
    `(make-coroutine
       (lambda/cc (,yield-sym ,@arg)
	 (macrolet 
	     ((yield (&optional ,value) 
		`(funcall ,',yield-sym ,,value)))
	   ,@body)))))



