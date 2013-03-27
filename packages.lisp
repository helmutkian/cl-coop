(defpackage #:com.helmutkian.cl-coop.generator
  (:nicknames #:cl-coop.generator))
  (:use #:common-lisp
	#:cl-cont)
  (:export #:generator
	   #:make-generator
	   #:next
	   #:deadp
	   #:with-generator))

(defpackage #:com.helmutkian.cl-coop.coroutine
  (:nicknames #:cl-coop.coroutine)
  (:use #:common-lisp
	#:com.helmutkian.cl-tacit
	#:com.helmutkian.cl-coop.generator)
  (:export #:coroutine
	   #:make-coroutine
           #:with-coroutine
	   #:start))
	   
 
(defpackage #:com.helmutkian.cl-coop.generator.test
  (:nicknames #:cl-coop.generator.test)
  (:use #:common-lisp
	#:com.helmutkian.cl-coop.generator
	#:cl-cont
	#:5am)
  (:export #:test-make-generator
	   #:test-with-generator)
  (:documentation "Package of unit tests for CL-COOP.GENERATOR package."))
