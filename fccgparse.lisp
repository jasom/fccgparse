;;;; fccgparse.lisp

(in-package #:fccgparse)
;(cl-interpol:enable-interpol-syntax)

;;; "fccgparse" goes here. Hacks and glory await!
;;(declaim (optimize (debug 3) (speed 0)))
;(declaim (optimize (speed 3) (debug 0)))

(defun my-capitalize (string)
  (format nil "~{~A~^ ~}" (mapcar (lambda (x) (string-upcase x :end 1)) (split-sequence #\Space string))))

(defun strip (string)
  (with-output-to-string (s)
    (loop with started = nil
       with maybe = nil
       for char across string
       when started
       do (if (char= char #\Space)
	      (push char maybe)
	      (progn
		(when maybe (write-sequence maybe s))
		(write-char char s)
		(setf maybe nil)))
       else when (char/= char #\Space)
       do (setf started t)
	 (write-char char s))))

(defclass simple-pdf-device ()
  ((output-stream :initarg :output-stream)
   (last-y :initform -1d20)))

(defclass bold-pdf-device (simple-pdf-device)
  ((boldp :initform nil)
   (lines :initform (list :normal))))

(defun pdf-help-render-string (textstate seq outs)
  (loop with font = (slot-value textstate 'pdfparse::font)
     for (a b . r) = seq then r
     when font
     do (loop for cid in (pdfparse::font-decode font a)
	   for ch = (handler-case (pdfparse::to-unichr font cid)
		      (pdfparse::key-error () (format nil "<CID ~X>" cid)))
	   when ch
	   do (format outs "~a" 
		      ch))
     unless r return nil))

  
(defclass column-pdf-device ()
  ((page-odd-p :initform nil :initarg :even-start)
   (column-split-even :initarg :even-split)
   (column-split-odd :initarg :odd-split)
   (header :initarg :header)
   (footer :initarg :footer)
   (current-line :initform (make-string-output-stream))
   (columns :initform (make-array 2 :initial-element nil))
   (last-y :initform -1d20)
   (current-line-column :initform 0)
   (output-lines :initform nil)
   (y-separation :initform 3 :initarg :y-separation)))

(defmethod (setf device-ctm) (ctm (device column-pdf-device)) ctm)
(defmethod DEVICE-BEGIN-TAG ((device column-pdf-device) tag &optional props)
  (declare (ignore device tag props)))
(defmethod DEVICE-END-TAG ((device column-pdf-device)))
(defmethod DEVICE-DO-TAG ((device column-pdf-device) tag &optional props)
  (declare (ignore device tag props)))

(defmethod DEVICE-BEGIN-PAGE ((device column-pdf-device) page ctm)
  (declare (ignorable page ctm))
  (with-slots (page-odd-p) device
    (setf page-odd-p (not page-odd-p))))

(defmethod DEVICE-END-PAGE ((device column-pdf-device) page)
  (declare (ignore page))
  (with-slots (columns output-lines current-line current-line-column) device
    (setf (aref columns current-line-column)
	  (nconc (aref columns current-line-column)
		 (list (#+(or)strip #-(or)identity (get-output-stream-string current-line)))))
    (setf output-lines
	  
	  (nconc output-lines (aref columns 0) (aref columns 1))
	  (aref columns 0) nil
	  (aref columns 1) nil)))

(defmethod DEVICE-BEGIN-TAG ((device simple-pdf-device) tag &optional props)
  (declare (ignore device tag props)))
(defmethod DEVICE-BEGIN-FIGURE ((device column-pdf-device) bbox matrix))
(defmethod DEVICE-END-FIGURE ((device column-pdf-device)))
(defmethod DEVICE-PAINT-PATH ((device column-pdf-device) graphicstate stroke fill evenodd path))
(defmethod DEVICE-RENDER-IMAGE ((device column-pdf-device) stream))

(defmethod DEVICE-RENDER-STRING ((device column-pdf-device) textstate seq)
  (destructuring-bind (x y) (nthcdr 4 (slot-value textstate 'pdfparse::matrix))
    (with-slots  (page-odd-p column-split-even column-split-odd header footer wasbold
			     current-line columns current-line-column last-y)
	device
      #+#:debug(format t "~&~A ~S  ~S" boldp
		       (list x y page-odd-p column-split-even column-split-odd)
		       (with-output-to-string (outs)
			 (pdf-help-render-string textstate seq outs)))
      (unless (or (> y header)
		  (< y footer))
	(let ((column
	       (if (or (and page-odd-p (< x column-split-odd))
		       (and (not page-odd-p) (< x column-split-even)))
		   0
		   1)))
	  (when (or (/= column current-line-column)
		    (and (> (abs (- last-y y)) 3)))
	    #+#:debug(format t "~&SPLIT: ~D"
			     (cond
			       ((/= column current-line-column) 1)
			       ((not (eql boldp wasbold)) (list boldp wasbold))
			       ((and (not boldp)
				     (> (abs (- last-y y)) 3)) 3)))
	    (setf (aref columns current-line-column)
		  (nconc (aref columns current-line-column)
			 (list (#+(or)strip #-(or)identity (get-output-stream-string current-line))))))
	  (setf current-line-column column
		last-y y)
	  (pdf-help-render-string textstate seq current-line))))))

#+(or)(defmethod DEVICE-RENDER-STRING ((device column-pdf-device) textstate seq)
  (destructuring-bind (x y) (nthcdr 4 (slot-value textstate 'pdfparse::matrix))
    (with-slots  (page-odd-p column-split-even column-split-odd header footer
			     current-line columns current-line-column last-y
			     y-separation)
	device
      #+(or)(format t "~&~D ~D ~A~%" x y
	      (with-output-to-string (s)
	      (pdf-help-render-string textstate seq s)))
      (unless (or (> y header)
		  (< y footer))
	(let ((column
	       (if (or (and page-odd-p (< x column-split-odd))
		       (and (not page-odd-p) (< x column-split-even)))
		   0
		   1)))
	  (when (or (/= column current-line-column)
		    (> (abs (- last-y y)) y-separation))
	    (setf (aref columns current-line-column)
		  (nconc (aref columns current-line-column)
			 (list (strip (get-output-stream-string current-line))))))
	  (setf current-line-column column)
	  (pdf-help-render-string textstate seq current-line))))))

(defclass bold-column-pdf-device (column-pdf-device)
  ;; Like column-pdf-device but doesn't break-up contiguous bold
  ;; text that spans lines, and breaks-up text at bold boundaries"
  ((wasbold :initform nil)))


(defmethod DEVICE-RENDER-STRING ((device bold-column-pdf-device) textstate seq)
  (let ((boldp (search
		"bold"
		(string (slot-value (slot-value textstate 'pdfparse::font)
				    'pdfparse::basefont))
		:test #'equalp)))
    
    (destructuring-bind (x y) (nthcdr 4 (slot-value textstate 'pdfparse::matrix))
      (with-slots  (page-odd-p column-split-even column-split-odd header footer wasbold
			       current-line columns current-line-column last-y)
	  device
	#+#:debug(format t "~&~A ~S  ~S" boldp
		(list x y page-odd-p column-split-even column-split-odd)
		(with-output-to-string (outs)
		  (pdf-help-render-string textstate seq outs)))
	(unless (or (> y header)
		    (< y footer))
	  (let ((column
		 (if (or (and page-odd-p (< x column-split-odd))
			 (and (not page-odd-p) (< x column-split-even)))
		     0
		     1)))
	    (when (or (/= column current-line-column)
		      (not (eql boldp wasbold))
		      (and (not boldp)
			   (> (abs (- last-y y)) 3)))
	      #+#:debug(format t "~&SPLIT: ~D"
		      (cond
			((/= column current-line-column) 1)
			((not (eql boldp wasbold)) (list boldp wasbold))
			((and (not boldp)
			      (> (abs (- last-y y)) 3)) 3)))
	      (setf (aref columns current-line-column)
		    (nconc (aref columns current-line-column)
			   (list (#-(or)strip #+(or)identity (get-output-stream-string current-line))))))
	    (setf current-line-column column
		  wasbold boldp
		  last-y y)
	    (if boldp
		(let ((text (with-output-to-string (outs)
			      (pdf-help-render-string textstate seq outs))))
		  (write-string (remove #\Newline text) current-line))
		(pdf-help-render-string textstate seq current-line))))))))

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
    (declare (ignorable x))
    ;;(when (or (< x 0) (< y 0)) (return-from device-render-string))
    (when (> (abs (- (slot-value device 'last-y) y)) 3)
      (write-char #\Newline (slot-value device 'output-stream)))
    (setf (slot-value device 'last-y) y))
  (pdf-help-render-string textstate seq (slot-value device 'output-stream)))

(defmethod device-render-string :around ((device bold-pdf-device) textstate seq)
  (with-slots (output-stream lines) device
    #|
    (terpri)
    (print (string (slot-value (slot-value textstate 'pdfparse::font)
				      'pdfparse::basefont)))
    (terpri)
    |#
    (let ((boldp (search
		  "bold"
		  (string (slot-value (slot-value textstate 'pdfparse::font)
				      'pdfparse::basefont))
		  :test #'equalp)))
      (when (not (eql boldp (slot-value device 'boldp)))
	(setf lines (nconc lines (split-sequence #\Newline (get-output-stream-string output-stream))))
	(nconc lines (list (if boldp :bold :normal)))
	(setf (slot-value device 'boldp) boldp))
      (call-next-method))))

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
		   (setf needcharspace t))))
	   (nthcdr 4 (pdfparse::translate-matrix
		       (slot-value textstate 'pdfparse::matrix)
		       (list dx dy)))) output-stream)
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

(defun get-bold-page-text (pdffilename pages)
  (let* ((f (make-string-output-stream))
	 (device (make-instance 'bold-pdf-device :output-stream f)))
    (pdfparse::process-pdf
     (pdfparse::make-pdf-resource-manager)
     device
     (pdfparse::make-pdf-input-stream pdffilename)
     :pagenos pages)
    (slot-value device 'lines)))

(defun get-column-page-text (pdffilename pages &key (device 'column-pdf-device)
						 (header 750)
						 (footer 60)
						 (odd-split 305)
						 (even-split 328)
						 (even-start nil)
						 (y-separation 3))
  (let* ((device (make-instance device
				:header header :footer footer
				:odd-split odd-split :even-split even-split
				:even-start even-start
				:y-separation y-separation)))
    (pdfparse::process-pdf
     (pdfparse::make-pdf-resource-manager)
     device
     (pdfparse::make-pdf-input-stream pdffilename)
     :pagenos pages)
    (slot-value device 'output-lines)))

(defun get-page-text (pdffilename pages &key (device-class 'simple-pdf-device))
  (with-output-to-string (f)
    (pdfparse::process-pdf
     (pdfparse::make-pdf-resource-manager)
     (make-instance device-class :output-stream f)
     (pdfparse::make-pdf-input-stream pdffilename)
     :pagenos pages)))


(defun get-feat-pages (pdffile)
  (split-sequence #\Newline
  (fccgparse::get-page-text pdffile (loop for i from 87 to 111 collect i))))

(defun get-feat-pages2 (pdffile)
  (get-column-page-text pdffile (loop for i from 87 to 111 collect i) :even-start t))

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
       (match (cl-ppcre:scan "[A-Z ][’A-Z -]+$" line)))
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
     when (cl-ppcre:scan "^(Benefit: )|(Prerequisites: )|( ?•)" line)
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
		      (push (list (my-capitalize name) short prereq benefit mycat) result)
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
	      ((cl-ppcre:scan "^[A-Z ][’A-Z -]+$" line)
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
     when (or
	   (cl-ppcre:scan "^•" line)
	   (cl-ppcre:scan "^Type:" line))
     nconc (preprocess-a-class-line (get-output-stream-string current-line))
     do (write-sequence line current-line)
       ;(format t "~a" line)
     unless (or (string= line "") (string= line "•")(char= (char line (1- (length line))) #\Space))
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
     if (char= (char word 0) #\en_dash)
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
    (push (cons :wound-multiplier
		(if words
		    (parse-number (pop words))
		    1))
	  result)
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
	 ((cl-ppcre:scan "^[A-Z][A-Z ]+$" line)
	  (setf name line))
	 ((cl-ppcre:scan "^• Attributes: " line)
	  (setf attributes (parse-attributes (subseq line 14))))
	 ((cl-ppcre:scan "^• Base Speed:" line)
	  (setf speed (parse-integer (subseq line 14) :junk-allowed t))
	  ;(format *error-output* "Speed found for ~a~%" name)
	  (unless speed (format *error-output* "SPEED BAD: ~s~%" (subseq line 14))))
	 ((cl-ppcre:scan "^Type: " line)
	  (setf type (parse-type (subseq line 6))))
	 ((cl-ppcre:scan "^• [^:]*:" line)
	  (let ((me (nth-value 1 (cl-ppcre:scan "^• [^:]*:" line))))
	    (push (make-keyword
		   (nsubstitute #\- #\Space
		   (string-upcase (subseq line 2 (1- me)))))
		  things)))
	 (t #+(or)(when name (format *error-output* "LINE: ~s~%" line))))
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

(defun extract-columns (header-line headings &optional keep-headings)
  (let ((headings (coerce headings 'list)))
    (values
     (loop with header = headings
	for (x y string) in header-line
	when (char= (car header) (char string 0))
	collect x
	and do (setf header (cdr header))
	while header)
     (when keep-headings
       (loop
	    with output = (make-string-output-stream)
	    with any = nil
	  for (x y string) in header-line
	  with header = headings
	  when (and header (char= (car header) (char string 0)))
	  if any
	  collect (get-output-stream-string output) into result
	  else do (setf any t) end
	  and do (setf header (cdr header))
	  do (write-sequence string output)
	  finally (return (append result (list (get-output-stream-string output)))))))))

(defun split-columns (columns table-line &optional (fudge 0))
    (loop with current = (make-string-output-stream)
       with column = (cdr columns)
       for (x1 y1 string x y) in table-line
       when (and column (> (+ x fudge) (car column)))
       collect (get-output-stream-string current) into result
       and do (pop column)
       do (write-string string current)
       finally (return (append result (list (get-output-stream-string current))))))
       


(defun extract-class-tables (pdfname)
  `((:assassin  ,@(extract-assassin-table pdfname))
    (:burglar ,@(extract-burglar-table pdfname))
    (:captain ,@(extract-captain-table pdfname))
    (:courtier ,@(extract-courtier-table pdfname))
    (:explorer ,@(extract-explorer-table pdfname))
    (:keeper ,@(extract-keeper-table pdfname))
    (:lancer ,@(extract-lancer-table pdfname))
    (:mage ,@(extract-mage-table pdfname))
    (:priest ,@(extract-priest-table pdfname))
    (:sage ,@(extract-sage-table pdfname))
    (:scout ,@(extract-scout-table pdfname))
    (:soldier ,@(extract-soldier-table pdfname))))

(defun get-career-level-table (pdfname)
  (let ((tbl (extract-career-level-table pdfname)))
    (push (mapcar (alexandria:curry #'format nil "~A ~A")
		  (pop tbl)
		  (pop tbl))
	  tbl)
    (mapc (lambda (x) (setf (second x)
			    (handler-case
				(parse-integer (remove #\, (second x)))
			      (t () (second x))))) tbl)))

(defun get-skills-table (pdfname)
  (let* ((base (butlast (extract-base-skills-table pdfname) 22))
	 (expert (butlast (extract-expert-skills-table pdfname)))
	 (ht (make-hash-table :test #'equal)))
    (format t "~&~s~%" base)
    (loop for item in (cdr base)
	 do
	 (multiple-value-bind (items count)
	     (split-sequence #\Space (car item) :from-end t :count 3)
	 (destructuring-bind (name attr feat)
	     items
	   (when (> count 0)
	     (setf name (concatenate 'string (subseq (car item) 0 (1+ count)) name)))
	      (format t "~&~S ~S ~S~%" name attr feat)
	   (setf (gethash name ht) (list attr feat))
	   (loop for class in (cdar base)
		 for is-skill in (cdr item)
		when (string= is-skill "O")
		  do (setf (cdr (last (gethash name ht))) (list class))))))
    (sort (alexandria:hash-table-alist ht) #'string< :key #'car)
    ))

(defun split-table (table-lines heading &optional include-headers)
  (loop
     with (columns headers) =
       (multiple-value-list (extract-columns (car table-lines) heading include-headers))
     for line in (cdr table-lines)
     collect (split-columns columns line 1) into columnout
     finally (return (if include-headers (cons headers columnout) columnout))))

(defun ensure-length (list length &key (default ""))
  (if (> length (length list))
      (loop for i from 1 to length
	 for x = list then (cdr x)
	 when x collect (car x)
	 else collect default)
      list))
	 
	 

(defun postprocess-gear-table (table ncols skiptest)
  (let ((table
	 (remove-if-not (lambda (x) (some (lambda (x) (position #\Tab x)) x)) table)))
    (loop
       with sofar = nil
       for lne in table
       for line = (ensure-length (mapcar
				  (lambda (x) (strip (substitute #\Space #\Tab x))) lne)
				 ncols)

       ;do (format t "LINE: ~A~%" line)
       unless (funcall skiptest line)
       when (string= (car line) "")
	 do (setf sofar (mapcar (lambda (x y) (if (string= y "") x
						  (concatenate 'string x " " y))) sofar line))
       else if sofar collect sofar into result end and
	 do (setf sofar line)
	 end
	 finally (return (nconc result (list sofar))))))
	 
(defun goods-skiptest (line)
  (or
   ;(string= (car line) "Good Upgrades")
   (starts-with #\* (car line))))
    

(defun get-goods-table (pdfname)
  (let* ((lines1 (fccgparse::get-table-lines pdfname (list 160)))
	 (lines2 (fccgparse::get-table-lines pdfname (list 161)))
	 (table-lines1 (car (get-table-lines-after lines1 (lambda (x) (ppcre:scan "Table 4.6" (table-line-text x))) 100)))
	 (table-lines2 (car (get-table-lines-after lines2 (lambda (x) (ppcre:scan "159" (table-line-text x))) 100))))
    (nconc
     (split-table table-lines1 "NESCCWEC" t)
     (split-table table-lines2 "NESCCWEC"))))

(defparameter *table-funcs* nil)

(defun extract-table (pdfname page startfn columns skiptest)
  (let* ((lines1 (fccgparse::get-table-lines pdfname (list page)))
	 (table-lines1 (car (get-table-lines-after
			     lines1
			     (lambda (x) (funcall startfn (table-line-text x)))
			     100)))
	 (split-table (split-table table-lines1 columns t)))
    (postprocess-gear-table split-table (length columns) skiptest)))


(defmacro define-table-extractor (table-name page-number scanner columns skiptest &optional push)
  (let ((fname (intern (string-upcase (format nil "EXTRACT-~a-TABLE" table-name)))))
    `(progn
       (defun ,fname (pdfname)
	   (extract-table pdfname ,page-number ,scanner ,columns ,skiptest))
       ,(when push
	 `(push (cons ,table-name ',fname) *table-funcs*)))))

(defun make-skiptest (matches)
  (lambda (line)
  (or (some 'identity
       (mapcar (lambda (x) (cl-ppcre:scan
			    x
			    (apply #'concatenate 'string line))) matches))
      (starts-with #\* (car line)))))

(defun extract-goods-table (pdfname)
  (postprocess-gear-table (get-goods-table pdfname) 8 #'goods-skiptest))

(push (cons :goods 'extract-goods-table) *table-funcs*)

(define-table-extractor :kits 162 (curry #'ppcre:scan "Table 4.7") "NSSCCWEC"
			(make-skiptest '(
					 ;"Kit Upgrades"
					 )) t)

(define-table-extractor :locks 163 (curry #'ppcre:scan "Table 4.8") "NETCSCCWEC"
		 (make-skiptest '(
				  ;"Locks"
				  ;"Traps"
				  ;"Lock & Trap Upgrades"
				  ))t)

(define-table-extractor :consumables 164 (curry #'ppcre:scan "Table 4.9") "NEUSCCWEC"
		 (make-skiptest '(
				  ;"General Consumables"
				  ;"Medical Supplies"
				  ))t)

(define-table-extractor :elixirs 165 (curry #'ppcre:scan "Table 4.10") "NESCCWEC"
		 (make-skiptest '(
				  ;"Elixir Upgrades"
				  ;"Potions"
				  ;"Oils"
				  ))t)

(define-table-extractor :food 167 (curry #'ppcre:scan "Table 4.11: Food & Drink$") "NEUSCCWEC"
		 (make-skiptest '("or discarded."))t)
	 
(define-table-extractor :poisons 168 (curry #'ppcre:scan "Table 4.12") "NEIUCWEC"
			(make-skiptest '("Poison Upgrades"))t)
	 
(define-table-extractor :scrolls 169 (curry #'ppcre:scan "Table 4.13") "SSCCWEC"
		 (make-skiptest '(
				  ;"Scroll Upgrades"
				  ))t)

(define-table-extractor :services 170 (curry #'ppcre:scan "Table 4.14") "SEAEC"
		 (make-skiptest '(
				  ;"Community Services"
				;"Hired Passage"
				;"Lodging (per person or animal)"
				  ))t)

(define-table-extractor :mounts 172 (curry #'ppcre:scan "Table 4.15") "MBTWAC"
		 (make-skiptest '(
				  ;"Draft Animals"
				  ;"Flying Mounts"
				  ;"Military Mounts"
				  ;"Riding Mounts"
				  ;"Swimming Mounts"
				  ;"Mount Upgrades"
				))t)

(define-table-extractor :vehicles 173 (curry #'ppcre:scan "Table 4.16") "NQSTSOCCEC"
		 (make-skiptest '(
				  ;"Air Vehicle"
				  ;"Land Vehicles"
				  ;"Water Vehicles"
				  ;"Vehicle Upgrades"
				))t)

(define-table-extractor :armor 176 (curry #'ppcre:scan "Table 4.17") "TDRDASDCCWEC"
		 (make-skiptest '(
				  ;"^Partial"
				  ;"^Moderate"
				  ;"^Fittings"
				  "^[a-z]"
				  "^Barding:"
				  "^Unborn:"
				))t)


(define-table-extractor :armor-upgrades 177 (curry #'ppcre:scan "Table 4.18") "NDEDASDCCWEC"
		 (make-skiptest '(
				  ;"^Craftsmanship"
				  ;"^Materials"
				  ;"^Customization"
				))t)

(define-table-extractor :blunt-weapons 179 (curry #'ppcre:scan "Table 4.19") "NDTQSCCWEC"
		 (make-skiptest '(
				  ;"^Clubs"
				  ;"^Flails"
				  ;"^Hammers"
				  ;"^Shields"
				  ;"^Staves"
				  ;"^Whips"
				))t)

(define-table-extractor :edged-weapons 181 (curry #'ppcre:scan "Table 4.20") "NDTQSCCWEC"
		 (make-skiptest '(
				  ;"^Axes"
				  ;"^Fencing Blades"
				  ;"^Knives"
				  ;"^Swords"
				  ;"^Greatswords"
				  ;"^Polearms"
				  ;"^Spears"
				))t)

(define-table-extractor :hurled-weapons 183 (curry #'ppcre:scan "Table 4.21") "NDTRQSCCWEC"
		 (make-skiptest '(
				  ;"^Thrown"
				  ;"^Grenades"
				))t)

(define-table-extractor :bows 184 (curry #'ppcre:scan "Table 4.22") "NDTRQSCCWEC"
		 (make-skiptest '(
				  ;"^Arrows"
				  ;"^Bows"
				))t)

(define-table-extractor :black-powder-weapons 185 (curry #'ppcre:scan "Table 4.23") "NDTRQSCCWEC"
		 (make-skiptest '(
				  ;"^Powder"
				  ;"^Sidearms"
				  ;"^Longarms"
				)))

(define-table-extractor :siege-weapons 186 (curry #'ppcre:scan "Table 4.24") "NDTRQSCCWEC"
		 (make-skiptest '(
				))t)

(define-table-extractor :weapon-upgrades 187 (curry #'ppcre:scan "Table 4.25") "NECCWEC"
		 (make-skiptest '(
				  ;"^Craftsman"
				  ;"^Material"
				  ;"^Customiz"
				  "^Unborn:"
				  "^Improve check"
				))t)

(define-table-extractor :assassin 33 (curry #'ppcre:scan "Table 1.5") "LBFRWDILLS"
			(make-skiptest '(
					 )))

(define-table-extractor :burglar 35 (curry #'ppcre:scan "Table 1.6") "LBFRWDILLS"
			(make-skiptest '(
					 )))

(define-table-extractor :captain 37 (curry #'ppcre:scan "Table 1.7") "LBFRWDILLS"
			(make-skiptest '(
					 )))

(define-table-extractor :courtier 39 (curry #'ppcre:scan "Table 1.8") "LBFRWDILLS"
			(make-skiptest '(
					 )))

(define-table-extractor :explorer 41 (curry #'ppcre:scan "Table 1.9") "LBFRWDILLS"
			(make-skiptest '(
					 )))

(define-table-extractor :keeper 43 (curry #'ppcre:scan "Table 1.10") "LBFRWDILLS"
			(make-skiptest '(
					 )))

(define-table-extractor :lancer 45 (curry #'ppcre:scan "Table 1.11") "LBFRWDILLS"
			(make-skiptest '(
					 )))

(define-table-extractor :mage 47 (curry #'ppcre:scan "Table 1.12") "LBFRWDILLSA"
			(make-skiptest '(
					 )))

(define-table-extractor :priest 48 (curry #'ppcre:scan "Table 1.13") "LBFRWDILLS"
			(make-skiptest '(
					 )))

(define-table-extractor :sage 50 (curry #'ppcre:scan "Table 1.14") "LBFRWDILLS"
			(make-skiptest '(
					 )))

(define-table-extractor :scout 52 (curry #'ppcre:scan "Table 1.15") "LBFRWDILLS"
			(make-skiptest '(
					 )))

(define-table-extractor :soldier 54 (curry #'ppcre:scan "Table 1.16") "LBFRWDILLS"
			(make-skiptest '(
					 )))

(define-table-extractor :career-level 29 (curry #'ppcre:scan "Table 1.4: Career Level$")
			"CTSMAEEE"
			(make-skiptest '()))

(define-table-extractor :base-skills 66 (curry #'ppcre:scan "Class	Skills")
			"BABCCEKLMPSSS"
			(make-skiptest '()))

(define-table-extractor :expert-skills 66 (curry #'ppcre:scan "Tactics")
			"EABEPRS"
			(make-skiptest '()))
			

(defun get-rogue-pages (pdffile)
  (fccgparse::get-column-page-text pdffile (loop for i from 246 to 250 collect i)
				   :device 'bold-column-pdf-device))


(esrap:defrule word
    (and
     (+ (not
	 (or
	  #\Space
	  #\(
	  #\))))
     (* #\Space))
  (:destructure (letters spaces)
    (declare (ignore spaces))
    (esrap:text letters)))

(esrap:defrule description
    (and
     (+ word)
     "("
     (+ word)
     "):")
  (:destructure (name _ (&rest desc) &rest r)
      (declare (ignore _ r))
      (let ((size (first desc))
	    (xp (parse-integer (car (last desc 2))))
	    (types (butlast (rest desc) 4))
	    (move (car (last desc 4))))
	      
	(list (format nil "~{~A~^ ~}" name) size types move xp))))

(defun extract-rogue-info (lines)
  (loop
     with state = :start
     with rogue = nil
     for line in lines
     when (eq state :rogues)
       when (or (alexandria:ends-with-subseq "):" line)
		(string= line "ROGUE TEMPLATES"))
         when rogue collect rogue end and
         do (when (string/= line "ROGUE TEMPLATES")
		(setf rogue (cons (esrap:parse 'description line) line)))
       else when rogue do (setf (cdr rogue)
				(if (zerop (length (cdr rogue)))
				    line
				    (format nil "~A ~A" (cdr rogue) line)))
            end
       else
     when (string= line "ROGUES GALLERY")
     do (setf state :rogues)
     until (string= line "ROGUE TEMPLATES")))


(defun get-bestiary-pages (pdffile)
  (fccgparse::get-column-page-text pdffile (loop for i from 255 to 288 collect i)
				   :device 'bold-column-pdf-device
				   :even-start t))

(defun get-spell-pages (pdffile)
  (nconc
   (fccgparse::get-column-page-text pdffile (loop for i from 117 to 153 collect i)
				    :device 'bold-column-pdf-device
				    :even-start t) (list "")))

(defun get-class-abilities (pdffile)
  (let ((pages (delete-if (curry #'find #\Tab)
			  (get-class-pages2 pdffile))))
    (loop
	 with state = :outer
	 with ability = nil
	 with description = (make-string-output-stream)
       for line in pages
	 ;;do (format t "~A ~A ~A~%" state ability line)
       when (cl-ppcre:scan "^[A-Z 0-9:]*$" line)
	 if ability collect (cons ability (get-output-stream-string description))
	 end
	 and do (setf state :outer ability nil)
       when (and (eql state :inner)
		 (cl-ppcre:scan ":$" line)
		 (< (length line) 30))
	 if ability collect (cons ability (get-output-stream-string description)) end
	 and do (setf ability (string-trim '(#\:) line))
       else if (eql state :inner) do (write-line line description) end
       when (cl-ppcre:scan "ABILIT(IES|Y)$" line)
       do (setf state :inner))))
	 

(defun get-class-pages2 (pdffile)
  (fccgparse::get-column-page-text pdffile (loop for i from 32 to 63 collect i)
				   :device 'bold-column-pdf-device
				   :even-start nil))
(defun extract-bestiary-info (lines)
  ;(return-from extract-bestiary-info lines)
  (loop
     with state = :out
     with beast = nil
     for line in lines
     until (string= line "MONSTER TEMPLATES")
     when (eq state :out)
     do (when (alexandria:ends-with-subseq "):" line)
	  (setf beast (cons (esrap:parse 'description line) line)
		state :in))
     else ;; Inside description
     do (setf (cdr beast) (format nil "~A ~A" (cdr beast) line))
     when (alexandria:starts-with-subseq "Treasure:" line)
     collect beast and
     do (setf state :out
	      beast nil)))
	   
(defun parse-sloppy-int (str)
  (parse-integer
   (remove-if (lambda (x) (member x '(#\,)))
	      str)))

(defun is-capitalized (str)
  (and (> (length str) 2)
       (not (position-if (lambda (x)
			   (position x "qwertyuiopasdfghjklzxcvbnm"))
			 str))))

(defun extract-spell-info (lines)
  (loop
     with state = :out
     with spell-start = nil
     with spell = nil
     with school = nil
     with level = nil
     with casting-time = nil
     with saving-throw = nil
     with distance = nil
     with area = nil
     with duration = nil
     with preparation-cost = nil
     with effect = nil
     for (line next next-next) on lines
     unless (position #\Tab line)
     when
       (cond
	 ((and
	       (alexandria:starts-with-subseq "Level" next-next)
	       (is-capitalized line)
	       (is-capitalized next))
	  (setf spell-start line)
	  nil)
	 ((or (null next)
	      (alexandria:starts-with-subseq "Level:" next))
	  (prog2
	      (cl-ppcre:register-groups-bind (parent) (":? ?As ([^,]*), except" effect)
		(let ((parent-info 
		       (assoc parent sofar :test #'equalp)))
		  (when parent-info
		    (destructuring-bind (pspell plevel pschool pcasting-time pdistance
						parea psaving-throw pduration
						ppreparation-cost peffect)
			parent-info
			(declare (ignore pspell))
		      (setf ;;spell (or spell pspell)
		       level (or level plevel)
		       school (or school pschool)
		       casting-time (or casting-time pcasting-time)
		       distance (or distance pdistance)
		       area (or area parea)
		       saving-throw (or saving-throw psaving-throw)
		       duration (or duration pduration)
		       preparation-cost (or preparation-cost ppreparation-cost)
		       effect (or effect peffect))))))
	      (when spell
		(list spell level school casting-time distance  area saving-throw duration
		      preparation-cost effect))
	    (setf spell (if spell-start
			    (format nil "~A ~A" spell-start line)
			    line)
		  level nil
		  saving-throw nil
		  school nil
		  casting-time nil
		  distance nil
		  area nil
		  duration nil
		  preparation-cost nil
		  school nil
		  spell-start nil
		  effect nil)))
	 ((alexandria:starts-with-subseq "Level:" line)
	  (setf state :level)
	  nil)
	 ((alexandria:starts-with-subseq "Casting Time:" line)
	  (setf state :casting-time)
	  nil)
	 ((alexandria:starts-with-subseq "Distance:" line)
	  (setf state :distance)
	  nil)
	 ((alexandria:starts-with-subseq "Area:" line)
	  (setf state :area)
	  nil)
	 ((alexandria:starts-with-subseq "Duration:" line)
	  (setf state :duration)
	  nil)
	 ((alexandria:starts-with-subseq "Preparation Cost:" line)
	  (setf state :preparation-cost)
	  nil)
	 ((alexandria:starts-with-subseq "Saving Throw:" line)
	  (setf state :saving-throw)
	  nil)
	 ((alexandria:starts-with-subseq "Effect" line)
	  (setf state :effect)
	  nil)
	 ((eql state :level)
	  (cl-ppcre:register-groups-bind ((#'parse-sloppy-int plevel) pschool)
	      ("([^ ]*) (.*)" line)
	    (setf level plevel
		  school pschool
		  state :dummy))
	  nil)
	 ((eql state :casting-time)
	  (setf casting-time (format nil "~@[~A ~]~A" casting-time line))
	  nil)
	 ((eql state :distance)
	  (setf distance (format nil "~@[~A ~]~A" distance line))
	  nil)
	 ((eql state :area)
	  (setf area (format nil "~@[~A ~]~A" area line))
	  nil)
	 ((eql state :duration)
	  (setf duration (format nil "~@[~A ~]~A" duration line))
	  nil)
	 ((eql state :preparation-cost)
	  (setf preparation-cost (format nil "~@[~A ~]~A" preparation-cost line))
	  nil)
	 ((eql state :saving-throw)
	  (setf saving-throw (format nil "~@[A ~]~A" saving-throw line))
	  nil)
	 ((eql state :effect)
	  (setf effect (format nil "~@[~A ~]~A" effect
			       (cl-ppcre:regex-replace "•" line "
•")))
	  nil))
     collect it into sofar
       finally (return sofar)))
  

(defun write-data-file (fname pdffname)
  (let ((*print-readably* t))
  (with-open-file (outf fname :direction :output :if-exists :supersede)
    ;(print '(cl:in-package :fccg) outf)
    (print `(cl:defparameter fccg::+feats+
	      ',(extract-feat-info (preprocess-feat-lines (get-feat-pages2 pdffname))))
	   outf)
    (print `(cl:defparameter fccg::+class+
	      ',(extract-class-info (preprocess-class-lines (get-class-pages pdffname))))
	   outf)
    (print `(cl:defparameter fccg::+class-tables+
                             ',(extract-class-tables pdffname))
           outf)
    (print `(cl:defparameter fccg::+career-levels+
	      ',(get-career-level-table pdffname))
	   outf)
    (print `(cl:defparameter fccg::+skill-table+
			     ',(get-skills-table pdffname))
	   outf)
    (print `(cl:defparameter fccg::+class-abilities+
	      ',(get-class-abilities pdffname))
	   outf)
    (print `(cl:defparameter fccg::+species+
	      ',(extract-species-info (preprocess-species-lines (get-species-pages pdffname))))
	   outf)
    (print `(cl:defparameter fccg::+ability-info+
	      ',(extract-ability-info
		 (append
		  (preprocess-species-lines (get-species-pages pdffname))
		  (preprocess-talent-lines (get-talent-pages pdffname))
		  (preprocess-specialty-lines (get-specialty-pages pdffname)))))
	   outf)
    (print `(cl:defparameter fccg::+specialty+
	      ',(extract-specialty-info (preprocess-specialty-lines (get-specialty-pages pdffname))))
	   outf)
    (print `(cl:defparameter fccg::+talent+
	      ',(extract-talent-info (preprocess-talent-lines (get-talent-pages pdffname))))
	   outf)
    (print `(cl:defparameter fccg::+gear+
	      ',(loop for (name . func) in (reverse *table-funcs*)
		    collect (cons name (funcall func pdffname))))
	   outf)
    (print `(cl:defparameter fccg::+spells+
	      ',(extract-spell-info (get-spell-pages pdffname)))
	   outf)
  (values))))

(defun extract-ability-info (lines)
  (loop for line in lines
     append (cl-ppcre:register-groups-bind (name desc)
		("• ([^:]*): (.*)" line)
	      (when (not (member name '("Attributes" "Base Speed" "Restricted Actions")
				 :test #'equalp))
		`((,name . ,desc))))))


(defun main-cmdline ()
  (let ((file (second (uiop:raw-command-line-arguments))))
    ;(print 
     ;(extract-rogue-info (get-rogue-pages file)))
    ;(terpri)
    ;(print (extract-bestiary-info (get-bestiary-pages file)))
    ;(print (extract-class-tables file))
    ;(trace extract-class-tables)
    (write-data-file
	   (third (uiop:raw-command-line-arguments))
	   file)
    (uiop:quit)))

