
(defun eql-to-and-not-exhausted-p (eql-to gen)
  (and (eql (next gen) eql-to)
       (not (exhaustedp gen))))

(test test-generator
  (let ((gen (make-generator
	      (lambda/cc (yield)
		(funcall yield 'a)
		(funcall yield 'b)
		(funcall yield 'c)))))
    (is (eql-to-and-not-exhausted-p 'a gen))
    (is (eql-to-and-not-exhausted-p 'b gen))
    (is (eql-to-and-not-exhausted-p 'c gen))
    (multiple-value-bind (val gen) (next gen)
      (is (null val))
      (is (eql (find-class 'exhausted-generator)
	       (class-of gen)))
      (is (exhaustedp gen)))))

(test test-with-generator
  (let ((gen
