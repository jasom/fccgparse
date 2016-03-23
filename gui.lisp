(in-package #:fccgparse)

(defun file-selector ()
  (let ((retval))
    (ltk:with-ltk ()
      (ltk:withdraw ltk::*tk*)
      (setf retval
	    (ltk:get-open-file :filetypes '(("Fantasy Craft Second Printing" "Fantasy_Craft_Second_Printing.pdf") ("PDF Files" "*.pdf") ("All Files" "*"))
			       :title "Please Select Fantasy Craft PDF File"))
      (ltk::exit-wish))
    retval))

(defun main ()
  (write-data-file
   "data.lisp"
   (file-selector))
  (uiop:quit))
