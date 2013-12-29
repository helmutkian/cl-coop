
(asdf:defsystem #:cl-coop
  :depends-on ("cl-cont" "closer-mop")
  :components
  ((:file "packages")
   (:file "coroutine"
	  :depends-on ("packages"))))
