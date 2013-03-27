
(in-package #:com.helmutkian.cl-coop.generator.test)

(defun eql-to-and-not-dead-p (eql-to gen)
  (and (eql (next gen) eql-to)
       (not (deadp gen))))

(test test-make-generator
  (let ((gen (make-generator 
	      (lambda/cc (k) 
		(funcall k 1) 
		(funcall k 2) 
		(funcall k 3)))))
    (is (eql-to-and-not-dead-p 1 gen))
    (is (eql-to-and-not-dead-p 2 gen))
    (is (eql-to-and-not-dead-p 3 gen))))

(test test-with-generator
  (let ((gen (with-generator
		(yield 1) 
		(yield 2) 
		(yield 3))))
    (is (eql-to-and-not-dead-p 1 gen))
    (is (eql-to-and-not-dead-p 2 gen))
    (is (eql-to-and-not-dead-p 3 gen))))
