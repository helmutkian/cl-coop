
(asdf:defsystem #:com.helmutkian.cl-coop.test
  :components
  ((:system "cl-cont")
   (:system "fiveam")
   (:system "cl-coop")
   (:file "packages"
	  :depends-on ("cl-cont"
		       "fiveam"
		       "cl-coop"))
   (:file "test-coroutine"
	  :depends-on ("packages"))))
