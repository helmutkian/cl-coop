
(asdf:defsystem #:cl-coop
  :depends-on ("cl-cont" "closer-mop")
  :components
  ((:file "packages"
	  :depends-on ("cl-cont"))
   (:file "coroutine"
	  :depends-on ("packages" "closer-mop"))))
