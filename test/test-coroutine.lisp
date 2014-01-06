
(in-package #:com.helmutkian.cl-coop.test)

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

(defcoro counter (n) ()
  (dotimes (i n)
    (yield i)))

(defcoro guessing-game (target &key (max-guesses 3)) (guess)
  (loop repeat max-guesses
        for result = guess then (yield 'try-again)
        when (= result target)
          do (yield 'win) and do (return)
        end
        finally (yield 'loss)))

(test test-defcoro-without-arg 
  (let ((results
	 (loop with count = (counter 3)
	       for c = (funcall count)
               until (null c)
	       collect c)))
    (is (equalp '(0 1 2) results))))

(test test-defcoro-with-arg
  (let ((result 
	 (loop with game = (guessing-game 100 :max-guesses 2)
               for i from 0
	       for g = (funcall game i)
	       until (null g)
	       collect g)))
    (is (equalp '(try-again loss) result))))
