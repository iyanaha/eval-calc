(defun extract-parenthesis (str)
  (let ((depth 0) (len 0))
    (do-char-in-str (c str)
		    (incf len)
		    (cond ((char= c #\() (incf depth))
			  ((char= c #\)) (decf depth)))
		    (if (zerop depth)
		      (return (values
				(subseq str 0 len)
				(subseq str len)))))))

(defmacro do-char-in-str ((c str) &body body)
  (let ((count-var (gensym)) (str-var (gensym)))
    `(let ((,str-var ,str))
       (dotimes (,count-var (length ,str-var))
	 (let ((,c (char ,str-var ,count-var)))
	   ,@body)))))

(defun remove-punctuations (str)
  (let ((len 0))
    (do-char-in-str (c str)
		    (if (punctuation-p c)
		      (incf len)
		      (return)))
    (subseq str len)))

(defconstant punctuations '(#\Space #\Newline #\Tab))

(defun punctuation-p (c)
  (member c punctuations))

(defun number-char/dot-p (c)
  (or (char= c #\.)
      (<= (char-code #\0) (char-code c) (char-code #\9))))

(defun extract-number (str)
  (let ((len 0))
    (do-char-in-str (c str)
		    (if (number-char/dot-p c)
		      (incf len)
		      (return)))
    (values (parse-number (subseq str 0 len))
	    (subseq str len))))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun decimal-number (str)
  (float
    (/ (parse-integer str) (expt 10 (length str)))))

(defun parse-number (str)
  (aif (position #\. str)
       (if (zerop it)
	 (decimal-number (subseq str 1))
	 (+ (parse-integer (subseq str 0 it))
	    (decimal-number (subseq str (1+ it)))))
       (parse-integer str)))

(defun strcar (str)
  (char str 0))

(defun (setf strcar) (val str)
  (setf (char str 0) val)
  val)

(defun strcdr (str)
  (subseq str 1))

(defun strnull (str)
  (zerop (length str)))

(defun strbutlast (str)
  (subseq str 0 (1- (length str))))

(defconstant operators '(#\+ #\- #\* #\/))

(defun operator-p (c)
  (member c operators))

(defun char->symbol (c)
  (intern (let ((str (make-string 1)))
	    (setf (strcar str) c)
	    str)))

(defun extract-operator (str)
  (if (operator-p (strcar str))
    (values (char->symbol (strcar str))
	    (subseq str 1))
    (error "No operator")))

(defconstant fn-mark (gensym))

(defun extract (str)
  (labels ((ext (str)
		(cond ((operator-p (strcar str))
		       (extract-operator str))
		      ((number-char/dot-p (strcar str))
		       (extract-number str))
		      ((function-call-p str)
		       (multiple-value-bind (fn-name args rest) (extract-fn-name-and-args str)
			 (values (list* fn-mark
					(intern fn-name) 
					(mapcar #'convert-str args))
				 rest)))
		      ((var-p str)
		       (multiple-value-bind (var-name rest) (extract-alpha-chars str)
			 (values (intern (string-upcase var-name)) rest)))
		      ((char= (strcar str) #\()
		       (multiple-value-bind (val rest) (extract-parenthesis str)
			 (values (str->list (extract-content val))
				 rest))))))
    (ext (remove-punctuations str))))

(defun extract-fn-name-and-args (str)
  (let ((pos (position #\( str)))
    (multiple-value-bind (pare rest) (extract-parenthesis (subseq str pos))
      (values (string-upcase (subseq str 0 pos)) 
	      (extract-arguments pare) 
	      rest))))

(defun function-call-p (str)
  (and (alpha-char-p (strcar str))
       (find #\( (extract-non-punctuation-chars str))))

(defun only-function-call-p (str)
  (and (function-call-p str)
       (multiple-value-bind1 (nil rest) (extract-alpha-chars str)
			     (multiple-value-bind1 (nil rest1) (extract-parenthesis rest)
						   (only-punctuations-p rest1)))))

(defun var-p (str)
  (and (alpha-char-p (strcar str))
       (not (find #\( (extract-non-punctuation-chars str)))))

(defun extract-content (str)
  (strcdr (strbutlast str)))

(defun str->list (str)
  (labels ((s->l (str)
		 (if (only-punctuations-p str)
		   nil
		   (multiple-value-bind (val rest) (extract str)
		     (cons val (s->l rest))))))
    (if (only-function-call-p str)
      (multiple-value-bind1 (val nil) (extract str)
			    val)
      (s->l str))))

(defun only-punctuations-p (str)
  (let ((only-punctuations? t))
    (do-char-in-str (c str)
		    (if (not (punctuation-p c))
		      (setf only-punctuations? nil)))
    only-punctuations?))

(defun *-/-member? (lst)
  (or (member '* lst) (member '/ lst)))

(defun *-/-pos (lst)
  (let ((pos* (position '* lst))
	(pos/ (position '/ lst)))
    (cond ((and pos* pos/) (min pos* pos/))
	  (pos* pos*)
	  (pos/ pos/))))

(defun analyze-list (lst)
  (if (or (<= (length lst) 3)
	  (not (*-/-member? lst)))
    lst
    (analyze-list
      (subst-by-list (*-/-pos lst) lst))))

(defun subst-by-list (pos lst)
  (insert (1- pos)
	  (subseq lst (1- pos) (+ pos 2))
	  (remove-nths (list (1- pos) pos (1+ pos))
		       lst)))

(defun remove-nths (ns lst)
  (labels ((rem-nths (ns lst)
		     (cond ((null ns) lst)
			   ((zerop (car ns))
			    (rem-nths (mapcar #'1- (cdr ns)) (cdr lst)))
			   (t (cons (car lst) 
				    (rem-nths (mapcar #'1- ns) (cdr lst)))))))
    (rem-nths (sort ns #'<) lst)))

(defun insert (n elt lst)
  (if (zerop n)
    (cons elt lst)
    (cons (car lst) (insert (1- n) elt (cdr lst)))))

(defun analyze-str (str)
  (analyze-list (str->list str)))

(defun convert (lst)
  (labels ((convert-converted (lst)
			      (cond ((fn-call-p lst) (cdr lst))
				    ((< (length lst) 3) (car lst))
				    (t (list (nth-from-end 1 lst)
					     (convert-converted
					       (subseq lst 0 (- (length lst) 2)))
					     (nth-from-end 0 lst)))))
	   (convert-necessary? (lst)
			       (and (not (fn-call-p lst))
				    (member-if #'listp lst)))
	   (conv (lst)
		 (cond ((atom lst) lst)
		       ((convert-necessary? lst)
			(convert-converted (mapcar #'conv lst)))
		       (t (convert-converted lst)))))
    (conv lst)))

(defun fn-call-p (lst)
  (and (listp lst)
       (eq fn-mark (car lst))))

(defun nth-from-end (n lst)
  (nth (- (length lst) n 1) lst))

(defun convert-str (str)
  (convert (analyze-str str)))

(defmacro repeat (n &body body)
  (let ((count-var (gensym)))
    `(dotimes (,count-var ,n)
       ,@body)))

(defun positions (obj lst)
  (if (strnull lst)
    '()
    (let ((result (mapcar #'1+ (positions obj (strcdr lst)))))
      (if (eql (strcar lst) obj)
	(cons 0 result)
	result))))

(defun list-lines (filename)
  (with-open-file (s (make-pathname :name filename) :direction :input)
    (let ((acc nil))
      (do ((line (read-line s nil :eof)
		 (read-line s nil :eof)))
	((eql line :eof) (nreverse acc))
	(push line acc)))))

(defun gen (lst)
  (cond ((declare-statement? (car lst))
	 (multiple-value-bind (var-name expr) (extract-var-name-and-expr (car lst))
	   `(let ((,(intern var-name) ,(convert-str expr)))
	      ,(gen (cdr lst)))))
	((define-statement? (car lst))
	 (multiple-value-bind (fn-name args expr) (extract-fn-name-args-and-expr (car lst))
	   `(labels ((,(intern fn-name) ,args ,(convert-str expr)))
	      ,(gen (cdr lst)))))
	(t (convert-str (car lst)))))

(defun gen-code (filename)
  (gen (list-lines filename)))

(defun declare-statement? (str)
  (find #\= str))

(defun define-statement? (str)
  (start-with? "DEF" (remove-punctuations str)))

(defun extract-fn-name-args-and-expr (str)
  (multiple-value-bind1 (nil rest) (extract-alpha-chars str)
			(multiple-value-bind (fn-name rest1) (extract-alpha-chars1
							       (remove-punctuations rest))
			  (multiple-value-bind (pare expr) (extract-parenthesis
							     (remove-punctuations rest1))
			    (values fn-name
				    (mapcar #'intern 
					    (mapcar #'string-upcase (extract-arguments pare)))
				    expr)))))

(defun remove-punctuations-and-comma (str)
  (let ((len 0))
    (do-char-in-str (c str)
		    (if (or (char= c #\,) (punctuation-p c))
		      (incf len)
		      (return)))
    (subseq str len)))

(defun extract-arguments (str)
  (labels ((ext (content)
		(if (only-punctuations-p content)
		  nil
		  (multiple-value-bind (arg rest) (extract-chars-until-comma content)
		    (cons arg (ext (remove-punctuations-and-comma rest)))))))
    (ext (extract-content str))))

(defun extract-chars-until-comma (str)
  (let ((len 0))
    (do-char-in-str (c str)
		    (if (char= c #\,)
		      (return)
		      (incf len)))
    (values (subseq str 0 len)
	    (subseq str len))))

(defmacro multiple-value-bind1 (args expr &body body)
  `(multiple-value-bind ,(mapcar #'(lambda (symb)
				     (if (null symb)
				       (gensym)
				       symb))
				 args) 
     ,expr ,@body))


(defun start-with? (keystr str)
  (let ((start-with? t)
	(pos 0))
    (do-char-in-str (c keystr)
		    (setf start-with? (alpha-char= c (char str pos)))
		    (incf pos))
    start-with?))

(defun alpha-char= (c1 c2)
  (char= (char-downcase c1) (char-downcase c2)))

(defun extract-var-name-and-expr (str)
  (let ((pos (position #\= str)))
    (values (string-upcase
	      (extract-alpha-chars 
		(remove-punctuations (subseq str 0 pos))))
	    (subseq str (1+ pos)))))

(defun extract-alpha-chars (str)
  (let ((len 0))
    (do-char-in-str (c str)
		    (if (alpha-char-p c)
		      (incf len)
		      (return)))
    (values (subseq str 0 len)
	    (subseq str len))))

(defun extract-alpha-chars1 (str)
  (multiple-value-bind (val rest) (extract-alpha-chars str)
    (values (string-upcase val) rest)))

(defun extract-non-punctuation-chars (str)
  (let ((len 0))
    (do-char-in-str (c str)
		    (if (punctuation-p c)
		      (return)
		      (incf len)))
    (values (subseq str 0 len)
	    (subseq str len))))

(defun eval-calc (filename)
  (eval (gen-code filename)))

(defun cat (filename)
  (with-open-file (str (make-pathname :name filename) :direction :input)
    (do ((line (read-line str nil :eof)
	       (read-line str nil :eof)))
      ((eq line :eof))
      (print line))))
