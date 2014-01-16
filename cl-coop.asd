
(asdf:defsystem #:cl-coop
  :depends-on ("cl-cont" "closer-mop" "parse-declarations-1.0")
  :components
  ((:file "packages")
   (:file "coroutine"
	  :depends-on ("packages"))))
