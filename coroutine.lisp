
(in-package #:com.helmutkian.cl-coop.coroutine)

;;; ***************************************************************
;;; ***************************************************************

(defclass coroutine-queue (%queue:queue) ())

(defun make-coroutine-queue ()
  (make-instance 'coroutine-queue))

;;; ***************************************************************
;;; Intended only for package-interal use.
;;; ***************************************************************

(deftacit push-coroutine
    "Pushes a coroutine onto the tail of a COROUTINE-QUEUE"
  #'%queue:push)

(deftacit pop-coroutine
    "Pops a coroutine off the head of a COROUTINE-QUEUE"
  #'%queue:pop)

(deftacit all-coroutines-exhausted-p
    "Determines whether all the coroutines within a COROUTINE-QUEUE
     have reached the end of their execution."
  #'%queue:emptyp)

;;; ***************************************************************
;;; ***************************************************************

(defvar *coroutine-queue* (make-coroutine-queue)
  "Dynamically accessible COROUTINE-QUEUE")

(defun coroutines-run (&optional (queue *coroutine-queue*))
  "Begins the execution of the coroutines within the given
   COROUTINE-QUEUE, which is by default the dynamically set
   *COROUTINE-QUEUE*."
  (do ()
      ((all-coroutines-exhausted-p queue))
    (let ((coroutine (next (pop-coroutine queue))))
      (unless (exhaustedp coroutine)
	(push-coroutine coroutine queue)))))

(defun coroutines-halt (&optional (queue *coroutine-queue*))
  "Calls HALT-COROUTINE to cease the execution of the coroutines
   within the given COROUTINE-QUEUE, which is by default the
   dynamically set *COROUTINE-QUEUE*"
  (do ()
      ((all-coroutines-exhausted-p queue))
    (halt-coroutine (pop-coroutine queue))))

;;; ***************************************************************
;;; ***************************************************************

(defclass coroutine-mixin () ()
  (:documentation
   "MIXIN that provides access to external COROUTINE protocol"))

(defclass coroutine (coroutine-mixin generator) 
  ()
  (:documentation
   "Coroutine object implmeneted using a continuation-based
    generator."))

(defclass exhausted-coroutine (coroutine-mixin exhausted-generator) 
  ()
  (:documentation
   "A coroutine that has reached the end of its execution"))

;;; ***************************************************************
;;; ***************************************************************

(defgeneric make-coroutine
    (object &optional queue &rest initargs)
  (:documentation 
   "Generic function for the construction of coroutines."))

(defmethod make-coroutine 
    "Creates coroutines from CL-CONT continuation objects"
    ((thunk cl-cont::funcallable/cc)
      &optional (queue *coroutine-queue*) 
      &rest initargs)
  (declare (ignore initargs))
  (push-coroutine (make-instance 'coroutine
				 :continuation thunk)
		  queue))

;;; ***************************************************************
;;; ***************************************************************

(defmethod next :around ((coroutine coroutine-mixin))
  ":AROUND method on CL-COOP.GENERATOR:NEXT which ensures that
   an exhausted coroutine changes its class to EXHAUSTED-COROUTINE."
  (let ((coroutine (call-next-method)))
    (when (eql (find-class 'exhausted-generator)
	       (class-of coroutine))
      (change-class coroutine 'exhausted-coroutine))))

;;; ***************************************************************
;;; ***************************************************************

(defgeneric halt-coroutine (coroutine)
  (:documentation 
   "Ceases the execution of a coroutine. Methods are intended to be
    added :BEFORE, :AFTER, or :AROUND in order to control resource
    management in the manner of UNWIND-PROTECT since UNWIND-PROTECT
    is unavailable when using CL-CONT library"))

(defmethod halt-coroutine ((coroutine coroutine-mixin))
  "Ensures an exhausted coroutine changes its class to
   EXHAUSTED-COROUTINE."
  (unless (exhaustedp coroutine)
    (change-class coroutine 'exhausted-coroutine)))

;;; ***************************************************************
;;; ***************************************************************


(defmacro with-coroutine (&body body)
  (let ((yield-sym (gensym)))
    `(make-coroutine
      (lambda/cc (,yield-sym)
	(macrolet ((yield () (funcall ,yield-sym nil)))
	  ,@body)))))
