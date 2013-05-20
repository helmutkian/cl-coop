
(in-package #:com.helmutkian.cl-coop.test.coroutine)

(test test-next-without-arg
  (let* ((coro 
	  (make-coroutine 
	   (lambda/cc (yield-cont)
	     (funcall yield-cont 1)
	     (funcall yield-cont 2)
	     (funcall yield-cont 3))))
	 (results 
	  (loop for result = (next coro) 
	        until (null result)
	        collect result)))
    (is (equalp results '(1 2 3)))))

(test test-next-with-arg
  (let* ((coro
	   (make-coroutine 
	    (lambda/cc (yield-cont arg)
	      (let (resume-value)
		(setf resume-value (funcall yield-cont 
					    (1+ arg))
		      resume-value (funcall yield-cont 
					    (1+ resume-value)))))))
	 (results 
	  (loop for i from 1
	        for result = (next coro i)
	        until (null result)
                collect result)))
    (is (equalp results '(2 3)))))

(test test-with-coroutine-without-arg
  (let* ((coro
	  (with-coroutine ()
	    (yield 1)
	    (yield 2)
	    (yield 3)))
	 (results
	  (loop for result = (next coro)
	        until (null result)
                collect result)))
    (is (equalp results '(1 2 3)))))

(test test-with-coroutine-with-arg
  (let* ((coro
	  (with-coroutine (arg)
	    (let (resume-value)
	      (setf resume-value (yield (1+ arg))
		    resume-value (yield (1+ resume-value))))))
	 (results 
	  (loop for i from 1
	        for result = (next coro i)
	        until (null result)
	        collect result)))
    (is (equalp results '(2 3)))))
