;;;; fccgparse.lisp

(in-package #:fccgparse)

;;; "fccgparse" goes here. Hacks and glory await!



(defclass simple-pdf-device ()
  ((output-stream :initarg :output-stream)
   (last-y :initform -1d20)))

(defmethod DEVICE-BEGIN-TAG ((device simple-pdf-device) tag &optional props)
  (declare (ignore device tag props)))
(defmethod DEVICE-END-TAG ((device simple-pdf-device)))
(defmethod DEVICE-DO-TAG ((device simple-pdf-device) tag &optional props)
  (declare (ignore device tag props)))
(defmethod DEVICE-BEGIN-PAGE ((device simple-pdf-device) page ctm))
(defmethod DEVICE-END-PAGE ((device simple-pdf-device) page))
(defmethod DEVICE-BEGIN-FIGURE ((device simple-pdf-device) bbox matrix))
(defmethod DEVICE-END-FIGURE ((device simple-pdf-device)))
(defmethod DEVICE-PAINT-PATH ((device simple-pdf-device) graphicstate stroke fill evenodd path))
(defmethod DEVICE-RENDER-IMAGE ((device simple-pdf-device) stream))
(defmethod DEVICE-RENDER-STRING ((device simple-pdf-device) textstate seq)
  (destructuring-bind (x y) (nthcdr 4 (slot-value textstate 'pdfparse::matrix))
    ;(when (or (< x 0) (< y 0)) (return-from device-render-string))
    (when (> (abs (- (slot-value device 'last-y) y)) 3)
      (write-char #\Newline (slot-value device 'output-stream)))
    (setf (slot-value device 'last-y) y))
  (loop with font = (slot-value textstate 'pdfparse::font)
       with outs = (slot-value device 'output-stream)
     for (a b . r) = seq then r
       when font
     do (loop for cid in (pdfparse::font-decode font a)
	     for ch = (handler-case (pdfparse::to-unichr font cid)
			  (pdfparse::key-error () (format nil "<CID ~X>" cid)))
	     when ch
	     do (format outs "~a" 
			ch))
       ;do (write-sequence
	   ;(babel:octets-to-string 
	    ;(map-into (make-array (length a) :element-type '(unsigned-byte 8)) #'char-code a)
	    ;:errorp nil) (slot-value device 'output-stream))
       unless r return nil))
  ;(write-char #\Newline (slot-value device 'output-stream)))
  ;(format t "RENDER-STRING: ~S~%" seq))

(defclass table-pdf-device (simple-pdf-device) ())
(defmethod DEVICE-RENDER-STRING ((device table-pdf-device) textstate seq)
  (let* ((fontsize (slot-value textstate 'pdfparse::fontsize))
	 (scaling (* .01d0 (slot-value textstate 'pdfparse::scaling)))
	 (charspace (* scaling (slot-value textstate 'pdfparse::charspace)))
	 (wordspace (if (or (not (slot-exists-p textstate 'pdfparse::font))
			 (pdfparse::is-multibyte (slot-value textstate 'pdfparse::font)))
			0
			(* scaling (slot-value textstate 'pdfparse::wordspace))))
	 (dxscale (* .001d0 fontsize scaling)))
  ;(destructuring-bind (x y) (nthcdr 4 (slot-value textstate 'pdfparse::matrix))
    ;(print (slot-value textstate 'pdfparse::matrix))
					;(when (or (< x 0) (< y 0)) (return-from device-render-string))
    (destructuring-bind (dx dy) (slot-value textstate 'pdfparse::linematrix)
      (let ((y (car (last (pdfparse::translate-matrix (slot-value textstate 'pdfparse::matrix) (list dx dy))))))
	(when (> (abs (- (slot-value device 'last-y) y)) 3)
	  (push :newline (slot-value device 'output-stream)))
	(setf (slot-value device 'last-y) y))
    (with-slots (output-stream) device
    (loop with font = (slot-value textstate 'pdfparse::font)
       ;with x = x
       with needcharspace = nil
       for (string adj . r) = seq then r
       when font
       do
	 (push
	  (append
	   (nthcdr 4 (pdfparse::translate-matrix
		       (slot-value textstate 'pdfparse::matrix)
		       (list dx dy)))
	   (list 
	    (with-output-to-string (outs)
	      (loop for cid in (pdfparse::font-decode font string)
		 for ch = (handler-case (pdfparse::to-unichr font cid)
			    (pdfparse::key-error () (format nil "<CID ~X>" cid)))
		 when ch
		 do (format outs "~a" 
			    ch)
		   (incf dx (* (pdfparse::char-width font cid) fontsize scaling))
		   (when (= cid 32) (incf dx wordspace))
		   (when needcharspace (incf dx charspace))
		   (setf needcharspace t))))) output-stream)
	 (when adj
	 (decf dx (* dxscale adj)))
					;do (write-sequence
					;(babel:octets-to-string 
					;(map-into (make-array (length a) :element-type '(unsigned-byte 8)) #'char-code a)
					;:errorp nil) (slot-value device 'output-stream))
       unless r return nil)
    (setf (slot-value textstate 'pdfparse::linematrix) (list dx dy))
    ))))

(defmethod (setf device-ctm) (ctm (device simple-pdf-device)) ctm)

(defun get-page-text-positions (pdffilename pages)
  (let ((device(make-instance 'table-pdf-device :output-stream nil)))
  (pdfparse::process-pdf
   (pdfparse::make-pdf-resource-manager)
   device
   (pdfparse::make-pdf-input-stream pdffilename)
   :pagenos pages)
  (nreverse (slot-value device 'output-stream))))

(defun get-page-text (pdffilename pages)
  (with-output-to-string (f)
    (pdfparse::process-pdf
     (pdfparse::make-pdf-resource-manager)
     (make-instance 'simple-pdf-device :output-stream f)
     (pdfparse::make-pdf-input-stream pdffilename)
     :pagenos pages)))


(defun get-feat-pages (pdffile)
  (split-sequence #\Newline
  (fccgparse::get-page-text pdffile (loop for i from 87 to 111 collect i))))

(defun preprocess-a-feat-line (line)
  (let*
      ((line
	(with-output-to-string (str)
	  (loop for (word . rest)  =
	       (split-sequence #\Space
			       line
			       :remove-empty-subseqs t)
	     then rest
	     do (write-sequence word str)
	     while rest
	     do (write-char #\Space str))))
       (match (cl-ppcre:scan "[A-Z ][A-Z -]+$" line)))
    (if (and match (> match 0))
	(list (subseq line 0 match)
	      (subseq line (if (char= (char line match) #\Space)
			       (1+ match) match)))
	(list line))))
	
(defun preprocess-feat-lines (lines)
  (loop with current-line = (make-string-output-stream)
     for line in lines
     when (or (cl-ppcre:scan "^\\d+$" line)
	      (string= line "LORE")
	      (string= line "CHAPTER 2"))
     do (setf line "")
     when (cl-ppcre:scan "^(Benefit: )|(Prerequisites: )" line)
     nconc (preprocess-a-feat-line (get-output-stream-string current-line))
     do (write-sequence line current-line)
     unless (or (string= line "") (char= (char line (1- (length line))) #\Space))
     nconc (preprocess-a-feat-line (get-output-stream-string current-line))))


(defun extract-feat-info (preprocessed-lines)
  (loop
     with state = :begin
     with name
     with short
     with prereq
     with benefit
     with mycat
     with result = nil
     with newfeat = (lambda ()
		      (push (list (string-capitalize name) short prereq benefit mycat) result)
		      (setf prereq "")
		      (setf benefit "")
		      (terpri))
     for line in preprocessed-lines
     do
       (case state
	   (:begin
	    (when (string= line "BASIC COMBAT FEATS")
		(setf mycat "BASIC COMBAT FEATS"
		      state :name)))
	   ((:name :benefit)
	    (cond
	      ((cl-ppcre:scan "FEATS" line)
	       (when name (funcall newfeat))
	       (setf mycat line
		     name nil))
	      ((cl-ppcre:scan "[A-Z ][A-Z -]+" line)
	       (when name
		 (funcall newfeat))
	       (setf name line
		     state :short))
	      ((eql state :benefit)
	       (setf benefit (concatenate 'string benefit (string #\Newline) line)))))
	   (:short
	    (setf short line
		  state :details))
	   (:details
	      (cond
		((starts-with-subseq "Benefit: " line)
		 (setf benefit (subseq line 9)
		       state :benefit))
		((starts-with-subseq "Prerequisites: " line)
		 (setf prereq (subseq line 15)))
		(t nil))))
       finally (progn
		 (funcall newfeat)
		 (return (nreverse result)))))

(defun get-class-pages (pdfpath)
  (split-sequence #\Newline
  (fccgparse::get-page-text pdfpath (loop for i from 32 to 54 collect i))))

(defun preprocess-a-class-line (line)
  (remove-if (lambda (x) (cl-ppcre:scan "^[IV]*$" x))
	     (preprocess-a-feat-line line)))

(defun preprocess-class-lines (lines)
  (loop with current-line = (make-string-output-stream)
     for line in lines
     when (or (cl-ppcre:scan "^\\d+$" line)
	      (string= line "HERO")
	      (string= line "CHAPTER 1"))
     do (setf line "")
     when (cl-ppcre:scan "^((Class Skills: )|(Skill Points: )|(Vitality: ))" line)
     nconc (preprocess-a-class-line (get-output-stream-string current-line))
     do (write-sequence line current-line)
     unless (or (string= line "") (char= (char line (1- (length line))) #\Space))
     nconc (preprocess-a-class-line (get-output-stream-string current-line))))

(defun extract-class-info (lines)
  (loop
					;with state = :begin
     with name
     with skills
     with points
     with vitality
     with proficiencies
     for line  in lines
     when (and name skills points vitality proficiencies)
     collect `(,(string-capitalize name) (:skills . ,(mapcar
				  (lambda (x) (subseq x 1))
				  (split-sequence #\, skills)))
		      (:skill-points . ,points)
		      (:vitality . ,vitality)
		      (:proficiencies . ,proficiencies))
       and do
       (setf name nil
	     skills nil
	     points nil
	     vitality nil
	     proficiencies nil)
     do
       (cond
					;(format t "STATE=~A ; ~A~%" state line)
	 ((and
	   (cl-ppcre:scan "^[A-Z ]+$" line)
	   (not (cl-ppcre:scan "(ABILIT)|(FEAT)" line)))
	  (setf name line))
	 ((cl-ppcre:scan "^Class Skills: " line)
	  (setf skills (subseq line 13)))
	 ((cl-ppcre:scan "^Skill Points: " line)
	  (setf points (parse-integer (subseq line 14) :junk-allowed t)))
	 ((cl-ppcre:scan "^Vitality: " line)
	  (setf vitality (parse-integer (subseq line 10) :junk-allowed t)))
	 ((cl-ppcre:scan "^Starting Proficiencies: " line)
	  (setf proficiencies (parse-integer (subseq line 24) :junk-allowed t))))))

(defun get-species-pages (pdfpath)
  (split-sequence #\Newline
		  (fccgparse::get-page-text pdfpath (loop for i from 11 to 20 collect i))))

(defun preprocess-species-lines (lines)
  (loop with current-line = (make-string-output-stream)
       with running = nil
     for line in lines
     when (string= line "DRAKE") do (setf running t)
     when (string= line "HUMAN TALENTS") do (setf running nil)
     when (or
	   (not running)
	   (cl-ppcre:scan "^\\d+$" line)
	   (string= line "HERO")
	   (string= line "CHAPTER 1"))
     do (setf line "")
     when (cl-ppcre:scan "^•" line)
     nconc (preprocess-a-class-line (get-output-stream-string current-line))
     do (write-sequence line current-line)
     unless (or (string= line "") (char= (char line (1- (length line))) #\Space))
     nconc (preprocess-a-class-line (get-output-stream-string current-line))))

(defparameter +attr-convert+
  '(:|Strength| :str
    :|Intelligence| :int
    :|Dexterity| :dex
    :|Constitution| :con
    :|Wisdom| :wis
    :|Charisma| :cha
    :|any| :any))

(defun parse-attributes (attr)
  (loop
       with modifier
       with attribute = nil
     for word in (split-sequence #\Space attr)
     ;if (string= word "to") do nil
     ;else if (starts-with-subseq "attribute" word) do nil
     if (char= (char word (1- (length word))) #\,)
       do (setf word (subseq word 0 (1- (length word))))
     if (char= (char word 0) #\–)
     when attribute collect (cons (if (cdr attribute)
				      attribute
				      (car attribute)) modifier) into result
     end
     and do (setf modifier (- (parse-integer (subseq word 1)))
		  attribute nil)
     if (char= (char word 0) #\+)
     when attribute collect (cons (if (cdr attribute)
				      attribute
				      (car attribute)) modifier) into result
     end
     and do (setf modifier (parse-integer (subseq word 1))
		  attribute nil)
     when (member (make-keyword word) +attr-convert+ :test #'equal)
     do (push (getf +attr-convert+ (make-keyword word)) attribute)
     finally (return
	       (if attribute
		   (cons (cons (if (cdr attribute)
				   attribute
				   (car attribute)) modifier) result)
		   result))))
     
(defun parse-type (typestring)
  (let ((words (split-sequence #\Space typestring))
	(result nil))
    (push (cons :size (make-keyword (string-upcase (pop words)))) result)
    (push (cons :footprint
		(if (ppcre:scan "^\\(" (car words))
		    (pop words)
		    "(1x1)")) result)
    (push (cons :legs (if (ppcre:scan "biped" (pop words)) 2 4)) result)
    (push (cons :type (make-keyword (string-upcase (pop words)))) result)
    (pop words) ;with
    (pop words) ;a
    (pop words) ;reach
    (pop words) ;of
    (push (cons :reach (parse-integer (pop words) :junk-allowed t)) result)
    (loop while (and words (not (ppcre:scan "score" (pop words)))))
    (and words (pop words)) ;x
    (cons :wound-multiplier
	  (if words
	      (parse-number (pop words))
	      1))
    result))
    

(defun extract-species-info (lines)
  (loop
					;with state = :begin
     with name
     with type
     with attributes
     with speed
     with things
     for line  in lines
     when (and name (cl-ppcre:scan "^[A-Z][A-Z ]+$" line))
     collect `(,(string-capitalize name) ,@type
		     (:attr . 
		     ,attributes)
		     (:base-speed . ,speed)
		     (:qualities . ,things)) into result
       and do
       (setf things nil
	     attributes nil)
     do
       (cond
					;(format t "STATE=~A ; ~A~%" state line)
	 ((cl-ppcre:scan "^[A-Z ]+$" line)
	  (setf name line))
	 ((cl-ppcre:scan "^• Attributes: " line)
	  (setf attributes (parse-attributes (subseq line 14))))
	 ((cl-ppcre:scan "^• Base Speed: " line)
	  (setf speed (parse-integer (subseq line 14) :junk-allowed t)))
	 ((cl-ppcre:scan "^Type: " line)
	  (setf type (parse-type (subseq line 6))))
	 ((cl-ppcre:scan "^• [^:]*:" line)
	  (let ((me (nth-value 1 (cl-ppcre:scan "^• [^:]*:" line))))
	    (push (make-keyword
		   (nsubstitute #\- #\Space
		   (string-upcase (subseq line 2 (1- me)))))
		  things))))
       finally
	(progn (setf (cdr (last result))
		     (list 
		      `(,(string-capitalize name) ,@type
			(:attr . 
			       ,attributes)
			(:base-speed . ,speed)
			(:qualities . ,things))))
	       (return result))))


(defun get-specialty-pages (pdfpath)
  (split-sequence #\Newline
		  (fccgparse::get-page-text pdfpath (loop for i from 23 to 29 collect i))))

(defun preprocess-specialty-lines (lines)
  (loop with current-line = (make-string-output-stream)
       with running = nil
     for line in lines
     when (string= line "ACROBAT") do (setf running t)
     when (string= line "STEP 3: ") do (setf running nil) and nconc (list "END")
     when (or
	   (not running)
	   (cl-ppcre:scan "^\\d+$" line)
	   (string= line "HERO")
	   (string= line "CHAPTER 1"))
     do (setf line "")
     when (cl-ppcre:scan "^•" line)
     nconc (preprocess-a-class-line (get-output-stream-string current-line))
     do (write-sequence line current-line)
     unless (or (string= line "") (char= (char line (1- (length line))) #\Space))
     nconc (preprocess-a-class-line (get-output-stream-string current-line))))

(defun extract-specialty-info (lines)
  (loop
					;with state = :begin
     with name
     with training
     with things
     with feat
     for line  in lines
     when (and name (cl-ppcre:scan "^[A-Z][A-Z ]+$" line))
     collect `(,(string-capitalize name) 
		(:attr-train . ,training)
		(:qualities . ,things)
		(:feat . ,feat))
       and do
       (setf things nil)
     do
       (cond
					;(format t "STATE=~A ; ~A~%" state line)
	 ((cl-ppcre:scan "^[A-Z ]+$" line)
	  (setf name line))
	 ((cl-ppcre:scan "^• Bonus Feat: " line)
	  (setf feat (make-keyword
		      (string-upcase (nsubstitute #\- #\Space (subseq line 14))))))
	 ((cl-ppcre:scan "^• Attribute Training: " line)
	  (setf training
		(loop for word in (split-sequence #\Space (subseq line 21))
		     when (and
			   (not (string= word "any"))
			   (getf +attr-convert+(make-keyword word)))
		     collect it)))
	 ((cl-ppcre:scan "^• [^:]*:" line)
	  (let ((me (nth-value 1 (cl-ppcre:scan "^• [^:]*:" line))))
	    (push (make-keyword
		   (nsubstitute #\- #\Space
		   (string-upcase (subseq line 2 (1- me)))))
		  things))))))

(defun preprocess-talent-lines (lines)
  (loop with current-line = (make-string-output-stream)
       with running = nil
     for line in lines
     when (string= line "ADAPTABLE") do (setf running t)
     when (string= line "SPECIALTY") do (setf running nil) and nconc (list "END")
     when (or
	   (not running)
	   (cl-ppcre:scan "^\\d+$" line)
	   (string= line "HERO")
	   (string= line "CHAPTER 1"))
     do (setf line "")
     when (cl-ppcre:scan "^•" line)
     nconc (preprocess-a-class-line (get-output-stream-string current-line))
     do (write-sequence line current-line)
     unless (or (string= line "") (char= (char line (1- (length line))) #\Space))
     nconc (preprocess-a-class-line (get-output-stream-string current-line))))

(defun extract-talent-info (lines)
  (loop
					;with state = :begin
     with name
     with speed
     with attributes
     with things
     for line  in lines
     when (and name (cl-ppcre:scan "^[A-Z][A-Z ]+$" line))
     collect `(,(string-capitalize name) 
		(:attr . ,attributes)
		(:base-speed . ,speed)
		(:qualities . ,things))
       and do
       (setf things nil)
     do
       (cond
					;(format t "STATE=~A ; ~A~%" state line)
	 ((cl-ppcre:scan "^[A-Z ]+$" line)
	  (setf name line))
	 ((cl-ppcre:scan "^• Attributes: " line)
	  (setf attributes (parse-attributes (subseq line 14))))
	 ((cl-ppcre:scan "^• Base Speed: " line)
	  (setf speed (parse-integer (subseq line 14) :junk-allowed t)))
	 ((cl-ppcre:scan "^• [^:]*:" line)
	  (let ((me (nth-value 1 (cl-ppcre:scan "^• [^:]*:" line))))
	    (push (make-keyword
		   (nsubstitute #\- #\Space
		   (string-upcase (subseq line 2 (1- me)))))
		  things))))))

(defun get-talent-pages (pdfpath)
  (split-sequence #\Newline
		  (fccgparse::get-page-text pdfpath (loop for i from 20 to 23 collect i))))


(defun table-line-text (line)
  (apply #'concatenate 'string (mapcar #'third line)))

(defun get-table-lines (pdfname page)
  (split-sequence:split-sequence :newline
				 (fccgparse::get-page-text-positions pdfname page)
				 :remove-empty-subseqs t))

(defun get-table-lines-after (table-lines matcher n)
  (loop
       with sofar = nil
     for line in table-lines
       for rest on (cdr table-lines)
     if (and sofar (< sofar n))
       collect line into result and do (incf sofar)
     else if (and sofar (>= sofar n)) return (list result rest)
     else if (funcall matcher line)
       do (setf sofar 0)
       finally (return (list result nil))))

(defparameter +class-headings+
  '(#\L
    #\B
    #\F
    #\R
    #\W
    #\D
    #\I
    #\L
    #\L
    #\S))

(defun extract-columns (header-line headings)
  (loop with header = headings
       for (x y string) in header-line
       when (char= (car header) (char string 0))
       collect x
       and do (setf header (cdr header))
       while header))

(defun split-columns (columns table-line &optional (fudge 0))
  (loop with current = (make-string-output-stream)
       with column = (cdr columns)
       for (x y string) in table-line
       when (and column (> (+ x fudge) (car column)))
       collect (get-output-stream-string current) into result
       and do (pop column)
       do (write-string string current)
       finally (return (append result (list (get-output-stream-string current))))))
       
(defun split-class-table (table-lines)
  (loop
     with columns = (extract-columns (car table-lines) +class-headings+)
     for line in (cdr table-lines)
       collect (split-columns columns line 1)))
       
(defun extract-class-tables (pdfname)
  (loop with lines = (fccgparse::get-table-lines pdfname (loop for i from  32 to 54 collect i))
       for (table-lines rest) = (get-table-lines-after lines #'match-class-table 23) then
       (get-table-lines-after rest #'match-class-table 23)
       do (print (split-class-table table-lines))
       while rest))

(defun match-class-table (x)
  (ppcre:scan "Table.*The.*" (table-line-text x)))

(defun write-data-file (fname pdffname)
  (with-open-file (outf fname :direction :output :if-exists :supersede)
    (prin1 '(cl:in-package :fccg) outf)
    (prin1 `(cl:defparameter fccg::+feats+
	      ',(extract-feat-info (preprocess-feat-lines (get-feat-pages pdffname))))
	   outf)
    (prin1 `(cl:defparameter fccg::+class+
	      ',(extract-class-info (preprocess-class-lines (get-class-pages pdffname))))
	   outf)
    (prin1 `(cl:defparameter fccg::+species+
	      ',(extract-species-info (preprocess-species-lines (get-species-pages pdffname))))
	   outf)
    (prin1 `(cl:defparameter fccg::+specialty+
	      ',(extract-specialty-info (preprocess-specialty-lines (get-specialty-pages pdffname))))
	   outf)
    (prin1 `(cl:defparameter fccg::+talent+
	      ',(extract-talent-info (preprocess-talent-lines (get-talent-pages pdffname))))
	   outf)
  (values)))

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


