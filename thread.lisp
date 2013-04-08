(defpackage #:com.helmutkian.cl-coop.thread
    (:use #:common-lisp
          #:com.helmutkian.cl-coop.self
          #:com.helmutkian.cl-coop.generator
          #:com.helmutkian.cl-coop.thread.queue)
  	(:export #:thread
             #:thread-queue
             #:make-thread
             #:with-thread
             #:run-thread))

(defclass thread (generator) ())

(defclass thread-queue 
  ((%queue :accessor queue :initform (make-queue))
   (%status :accessor runningp :initfor nil)))

(defun make-thread-queue ()
  (make-instance 'thread-queue))

(defself run-thread-queue (self)
  ;; Cannot run an already running queue
  (assert (not @runningp)))
  (setf @runningp t)
  (do ()
      ((queue-empty-p @queue) (setf @runningp nil))
    (let ((the-thread (queue-pop @queue)))
      (next the-thead)
      (unless (deadp the-thread)
        (queue-push the-thread @queue)))))
  

(defvar *thread-queue* (make-thread-queue))

(defun make-thread (thunk)
  (make-instance 'thread thunk))

(defmacro with-thread (&body body)
  `(change-class (with-generator ,@body)
                 'thread))

(defun run-thread (the-thread)
  (queue-push the-thread (queue *thread-queue*))
  (unless (runningp *thread-queue)
    (run-thread-queue *thread-queue*)))
