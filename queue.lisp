(in-package #:com.helmutkian.cl-coop.%queue)

(defclass queue ()
  ((head
    :accessor head
    :initform nil
    :documentation
    "Pointer to the first CONS cell of the queue")
   (tail
    :accessor tail
    :initform nil
    :documentation 
    "Pointer to the last CONS cell of the queue."))
  (:documentation "FiFo data structure implemented with a LIST"))

(defun queue-push (new-elem queue)
  "Pushes a new element onto the tail of the QUEUE."
  (let ((boxed-elem (list new-elem)))
    (if (null (head queue))
	(setf (head queue) boxed-elem
	      (tail queue) boxed-elem)
	(setf (cdr (tail queue)) boxed-elem
	      (tail queue) boxed-elem))
    queue))

(defun queue-pop (queue)
  "Pops an element off of the head of the QUEUE."
  (let ((popped-value (pop (head queue))))
    (values popped-value queue)))

(defun queue-empty-p (queue)
  "Determines whether a QUEUE is empty or not"
  (null (head queue)))
