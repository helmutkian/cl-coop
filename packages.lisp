(defpackage #:com.helmutkian.cl-coop.generator
  (:nicknames #:cl-coop.generator))
  (:use #:common-lisp
	#:cl-cont)
  (:export #:generator-mixin
	   #:generator
	   #:exhausted-generator
	   #:make-generator
	   #:next
	   #:exhaustedp
	   #:with-generator))

(defpackage #:com.helmutkian.cl-coop.%queue
  (:nicknames #:%queue)
  (:use #:common-lisp)
  (:shadow #:push
	   #:pop)
  (:export #:queue
	   #:make-queue
	   #:push
	   #:pop
	   #:emptyp)
  (:documentation
   "Utility package used internally to implement 
    :COM.HELMUTKIAN.CL-COOP.COROUTINE package."))

(defpackage #:com.helmutkian.cl-coop.coroutine
  (:nicknames #:cl-coop.coroutine)
  (:use #:common-lisp
	#:com.helmutkian.cl-tacit
	#:com.helmutkian.cl-coop.generator)
  (:export #:coroutine-queue
	   #:make-coroutine-queue
	   #:*coroutine-queue*
	   #:run-coroutines
	   #:halt-coroutines
	   #:coroutine
	   #:exhausted-coroutine
	   #:make-coroutine
	   #:with-coroutine))
	   
 
