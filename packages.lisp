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
	   
 
