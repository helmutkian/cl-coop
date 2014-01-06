
(asdf:defsystem #:cl-coop-test
  :depends-on ("cl-cont" "fiveam" "cl-coop")
  :components
  ((:file "packages")
   (:file "test-coroutine"
	  :depends-on ("packages"))))
