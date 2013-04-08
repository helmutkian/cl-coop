(defpackage #:com.helmutkian.cl-coop.thread.queue
  (:use #:common-lisp
        #:com.helmutkian.cl-self)
  (:export #:queue
           #:make-queue
           #:queue-push
           #:queue-pop
           #:queue-empty-p))

(in-package #:com.helmutkian.cl-coop.thread.queue)

(defclass queue ()
    ((%head :accessor head :initform nil)
     (%tail :accessor tail :initform nil)))

(defun make-queue ()
  	(make-instance 'queue))

(defself queue-push (obj self)
  (let ((new-tail (list obj)))
  	(setf (cdr @tail) new-tail
          @tail new-tail)
	(when (null @head)
      (setf @head @tail))))

(defself queue-pop (self)
  (pop @head))

(defself queue-empty-p (self)
  (null @head))
