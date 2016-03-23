;;;; fccgparse.asd

(asdf:defsystem #:fccgparse
  :serial t
  :description "Describe fccgparse here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:pdfparse
               #:alexandria
	       #:split-sequence
	       #:parse-number
	       #:cl-interpol
	       #:esrap)
  :components ((:file "package")
               (:file "fccgparse")))

(asdf:defsystem #:fccgparse/gui
  :serial t
  :description "Describe fccgparse here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:jtk #:fccgparse)
  :components ((:file "gui")))
