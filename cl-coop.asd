
(asdf:defsystem #:cl-coop
  :components
  ((:system "cl-cont")
   (:file "packages"
	  :depends-on ("cl-cont"))
   (:file "coroutine"
	  :depends-on ("packages"))))
