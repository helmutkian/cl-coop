
(asdf:defsystem #:cl-coop
  :components
  ((:system "cl-cont")
   (:system "cl-tacit")
   (:file "packages"
	  :depends-on ("cl-cont"))
   (:file "generator"
	  :depends-on ("packages"))
   (:file "queue"
	  :depends-on ("packages"))
   (:file "coroutines"
	  :depends-on ("cl-tacit" "queue"))))
