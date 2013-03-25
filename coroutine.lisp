
(in-package #:com.helmutkian.cl-coop.coroutine)


(defclass coroutine ()
  ((generator 
    :reader generator
    :initarg generator)))

(defun make-coroutine (thunk)
  (make-instance 'coroutine
		 :generator thunk))

(defmacro with-coroutine (&body body)
  (let ((resume-sym (gensym)))
    `(make-coroutine
      (lambda/cc (,resume-sym)
	(macrolet ((resume (coroutine)
		     `(funcall ,resume-sym ,coroutine)))
	  ,@body)))))

(defun start (coroutine &optional initval)
  (do ((process coroutine (next process)))
      ((deadp process))))
      

