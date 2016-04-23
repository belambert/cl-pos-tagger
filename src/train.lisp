;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :pos-tagger)

(defun count-history (tags table n)
  "Given a list of tags, a count table, and a window size (n),
   count tag n-grams and increment the count table."
  (loop for i from 0 below (- (length tags) n)
     for history = (subseq tags i (+ i n)) do
       (incf (gethash history table 0))))

(defun count-words (tokens table)
  "Given a list of tokens (i.e. word tag pairs), and a count table, increment the
   counts in the table for each word-tag pair."
  (loop for word-tag-pair in tokens do
       (incf (gethash word-tag-pair table 0))))

(defun line->pos-token-list (line &key (delimiter #\_) (add-sentence-boundaries t) (substitute-numbers "##NUMBER##"))
  "Given a line, split it into tokens, and the tokens into word/POS pairs.
   Convert the POS tag into a keyword."
  (let ((tokens (split-sequence:split-sequence #\Space line)))
    (setf tokens (mapcar (lambda (x) (split-sequence:split-sequence delimiter x)) tokens))
    (when substitute-numbers
      (setf tokens (mapcar (lambda (x) (list (if (number-string-p (first x)) substitute-numbers (first x)) (second x))) tokens)))
    (when add-sentence-boundaries
      (setf tokens (append '(("<s-1>" "<s-1>") ("<s>" "<s>")) tokens '(("</s>" "</s>")))))
    (setf tokens (mapcar (lambda (x) (list (first x) (intern (second x) "KEYWORD"))) tokens))
    tokens))

(defun compute-interpolation-weights (model)
  "Compute the best interpolation weights for the model."
  (let ((lambda1 0) (lambda2 0) (lambda3 0))
    (loop for t0 from 0 below (pos-model-tag-count model) do
	 (loop for t-1 from 1 below (pos-model-tag-count model) do
	      (loop for t-2 from 2 below (pos-model-tag-count model) do
		   (let* ((count-t1-t2-t3 (aref (pos-model-3gram-counts model) t-2 t-1 t0))
			  (count-t1-t2 (aref (pos-model-2gram-counts model) t-2 t-1))
			  (count-t2-t3 (aref (pos-model-2gram-counts model) t-1 t0))
			  (count-t2 (aref (pos-model-1gram-counts model) t-1))
			  (count-t3 (aref (pos-model-1gram-counts model) t0))
			  (n (pos-model-token-count model))
			  (3gram-prob (if (= (1- count-t1-t2) 0)
					  0.0
					  (/ (1- count-t1-t2-t3) (1- count-t1-t2))))
			  (2gram-prob (if (= (1- count-t2) 0)
					  0.0
					  (/ (1- count-t2-t3) (1- count-t2))))
			  (1gram-prob (/ (1- count-t3) (1- n)))
			  (max (max 3gram-prob 2gram-prob 1gram-prob)))
		     (cond
		       ((= max 3gram-prob) (incf lambda3 count-t1-t2-t3))
		       ((= max 2gram-prob) (incf lambda2 count-t1-t2-t3))
		       ((= max 1gram-prob) (incf lambda1 count-t1-t2-t3))
		       )))))
    (let ((sum (+ lambda1 lambda2 lambda3)))
      (alexandria:coercef sum 'single-float)
      (setf lambda1 (/ lambda1 sum))
      (setf lambda2 (/ lambda2 sum))
      (setf lambda3 (/ lambda3 sum)))
    (format t "INTERPOLATION WEIGHTS: ~f ~f ~f~%" lambda1 lambda2 lambda3)
    (setf (pos-model-lambda1 model) lambda1)
    (setf (pos-model-lambda2 model) lambda2)
    (setf (pos-model-lambda3 model) lambda3)
    (values lambda1 lambda2 lambda3)))

(defun count-table->count-array (table n tag-table tag-count)
  "Creates an n-dimensional array of the tag counts, so that we can access them
   very fast.  If it's a 3-gram model, with  50 tags, then we createa a 50x50x50
   count array."
  (let* ((dimensions (make-list n :initial-element tag-count))
	 (array (make-array dimensions :initial-element 0 :element-type 'fixnum)))
    (loop for key being the hash-keys of table
	 for value = (gethash key table)
	 for indices = (mapcar (lambda (x) (gethash x tag-table)) key) do
	 (setf (apply #'aref array indices) value))
    array))

(defun lex-table->lex-array (table tag-table tag-count vocab-table vocab-count)
  "Given a lexical count table, convert it to a count array for fast access.  This creates
   a 2d array for pairs of words and tags.  The final table... for 50 tags, and 100k words
   would be an array with dimensions 50x100k."
  (let* ((array (make-array (list vocab-count tag-count) :initial-element 0 :element-type 'fixnum)))
    (loop for key being the hash-keys of table
       for count = (gethash key table)
       for word = (gethash (first key) vocab-table)
       for tag = (gethash (second key) tag-table) do
	 (setf (aref array word tag) count))
    array))

(defun train-model (filename)
  "Given a filename of a POS tagged file, read in the file, compute all the counts,
   and create a model object."
  (let ((lex-counts (make-hash-table :test 'equalp))
	(1gram-counts (make-hash-table :test 'equalp))
	(2gram-counts (make-hash-table :test 'equalp))
	(3gram-counts (make-hash-table :test 'equalp))
	(total-count 0)
	(tag-table (make-hash-table :test 'equalp))
	(word-table (make-hash-table :test 'equalp)))
    ;; Do all the counting
    (do-lines (line filename)
      (unless (string-equal line "")
	(let* ((tokens (line->pos-token-list line))
	       (tag-seq nil))
	  (setf tokens (remove-if (lambda (x) (or (string-equal (first x) "") (string-equal (second x) ""))) tokens))
	  (setf tag-seq (mapcar 'second tokens))
	  (assert (every 'identity tag-seq))
	  (assert (every (lambda (x) (or (keywordp x) (> (length x) 0))) tag-seq))
	  (incf total-count (length tokens))
	  (count-history tag-seq 1gram-counts 1)
	  (count-history tag-seq 2gram-counts 2)
	  (count-history tag-seq 3gram-counts 3)
	  (count-words tokens lex-counts)
	  (loop for (word tag) in tokens do	     
	       (incf (gethash tag tag-table 0))
	       (incf (gethash word word-table 0))))))
    ;; Build the actual POS model
    (let* ((tags (sort (alexandria:hash-table-keys tag-table) 'string-lessp))
	   (tag-table (bl::seq->index-table tags))
	   (tag-count (length tags))
	   (vocab (sort (alexandria:hash-table-keys word-table) 'string-lessp))
	   (vocab-count (length vocab))
	   (vocab-table (bl::seq->index-table vocab))
	   (model (make-instance 'pos-model
				 :lex-counts (lex-table->lex-array lex-counts tag-table tag-count vocab-table vocab-count)
				 :1gram-counts (count-table->count-array 1gram-counts 1 tag-table tag-count)
				 :2gram-counts (count-table->count-array 2gram-counts 2 tag-table tag-count)
				 :3gram-counts (count-table->count-array 3gram-counts 3 tag-table tag-count)				 
				 :token-count total-count
				 :tag-set (make-array (length tags) :initial-contents tags)
				 :tag-count (length tags)
				 :tag-table tag-table
				 :vocab (make-array (length vocab) :initial-contents vocab)
				 :vocab-size (length vocab)
				 :vocab-table vocab-table)))
      (compute-interpolation-weights model)
      (setf *pos-model* model)
      model)))

(defun save-model (model filename)
  "Save the given model to a filename using the package CL-STORE"
  (cl-store:store model filename))

(defun load-model (filename)
   "Load the model in the given filename and return. Model must have been saved in CL-STORE format."
  (cl-store:restore filename))


	
