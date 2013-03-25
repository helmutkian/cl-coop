
(asdf:defsystem #:cl-coop
  :components
  ((:system "cl-cont")
   (:file "packages"
	  :depends-on ("cl-cont"))
   (:file "generator"
	  :depends-on ("packages"))
   (:file "coroutines"
	  :depends-on ("packages" "generator"))))
