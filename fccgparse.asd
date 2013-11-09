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
	       #:jtk)
  :components ((:file "package")
               (:file "fccgparse")))

