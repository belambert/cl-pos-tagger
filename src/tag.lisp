;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(declaim (optimize (debug 3)))
(in-package :pos-tagger)
(cl-user::file-summary "Running a POS tagger...")

(defun tag-sentence-string (string)
  (tag-sentence (split-sequence:split-sequence #\Space string)))

(defun tag-sentence (words)
  (tag-sentence-viterbi-initial words))

(defun tag-sentence-basic (words)
  (let ((history (list :|<s>| :|<s-1>|)))
    (loop for word in words
       for effective-history = (reverse (subseq history 0 2))
       for tag = (tag-word word effective-history *pos-model*) do
	 (unless tag
	   (format t "NO TAG FOR WORD: ~A~%" word))
	 (assert tag)
	 (push tag history))
    (subseq (reverse history) 2)))

(defun tag-sentence-viterbi-string (string)
  (tag-sentence-viterbi-initial (split-sequence:split-sequence #\Space string)))

(defun follow-back-pointers (bp-array final-state model)
  (let ((path (list final-state))
	(prev final-state))
    (loop for i from (- (array-dimension bp-array 0) 2) downto 1 do
	   (setf prev (aref bp-array i prev))
	   (push prev path))
    (setf path (mapcar (lambda (x) (get-tag x model)) path))
    ;;(format t "PATH:~{ ~A~}~%" path)
    path))

(defun tag-sentence-viterbi-initial (words)
  (let* ((back-pointers (make-array (list (1+ (length words)) (pos-model-tag-count *pos-model*)  )))
	 (scores (make-array (list (1+ (length words)) (pos-model-tag-count *pos-model*)) :initial-element 0.0))
	 (model *pos-model*)
	 (tag-count (pos-model-tag-count model))
	 (word-count (length words))
	 (s-tag (get-tag-id :|<s>| model))
	 (s-1-tag (get-tag-id :|<s-1>| model))
	 (tag-set (pos-model-tag-set *pos-model*)))
    (loop for tag across tag-set
       for i from 0 below tag-count
       for tag-id = i then i do
	 (setf (aref back-pointers 0 i) s-tag)
	 (setf (aref scores 0 i) (log-p-hat-model (first words) tag-id s-tag s-1-tag model)))
    (loop for word in (rest words)
       for i from 1 below (length words) do
	 (setf word (get-word-id word *pos-model*))
	 (loop for tag across tag-set
	    for j from 0 below tag-count 
	    for tag-id = j then j
	    for best-score = sb-ext:single-float-negative-infinity
	    for best-tag = nil do
	      (loop for prev-tag across tag-set
		 for k from 0 below tag-count
		 for prev-tag-id = k then k
		 for prev-score = (aref scores (1- i) k) 
		 for t-2-pos = (aref back-pointers (1- i) k)
		 for history = (list (get-tag t-2-pos model) prev-tag)
		 for this-prob = (if  t-2-pos
				      (log-p-hat-model word tag-id prev-tag-id t-2-pos model)
				      sb-ext:single-float-negative-infinity)
		 for net-score = (+ this-prob prev-score) do
		   (when (> net-score best-score)
		     (setf best-score net-score)
		     (setf best-tag prev-tag)))
	      (setf (aref back-pointers i j) (get-tag-id best-tag model))
	      (setf (aref scores i j) best-score)))
    (let ((best-overall-score SB-EXT:SINGLE-FLOAT-NEGATIVE-INFINITY)
	  (best-last-tag nil))
      (loop for tag across tag-set
	 for i from 0 below tag-count 
	 for cumulative-score = (aref scores (1- word-count) i)
	 for prev-tag-id = (aref back-pointers (1- word-count) i)
	 for history = (list (get-tag prev-tag-id model) tag)
	 ;;for last-score = (log (p-hat-interpolated (append history (list :|<\s>|)) model))
	 ;;for last-score = (log (p-hat-interpolated prev-tag-id i :|<\\s>| model))
	 for last-score = (if prev-tag-id
			      (log (p-hat-interpolated prev-tag-id i (get-tag-id :|</s>| model) model))
			      sb-ext:single-float-negative-infinity)
	 for net-score = (+ last-score cumulative-score) do
	   (setf (aref scores word-count i) net-score)
	   (when (> net-score best-overall-score)
	     (setf best-overall-score net-score)
	     (setf best-last-tag i)))
      (follow-back-pointers back-pointers best-last-tag model))))


(defun tag-file (filename &key (separator #\_))
  (let ((total-word-count 0)
	(sentence-count 0)
	(start-time (get-universal-time)))
    (do-lines (line filename)
      (let* (;;(words (line->pos-token-list line :add-sentence-boundaries nil :substitute-numbers (if (get-word-id "##NUMBER##") "##NUMBER##" nil)))
	     ;;(words (split-sequence:split-sequence #\Space line))
	     (words (if (get-word-id "##NUMBER##" *pos-model*)
			(mapcar (lambda (x) (if (number-string-p x) "##NUMBER##" x)) (split-sequence:split-sequence #\Space line))
			(split-sequence:split-sequence #\Space line)))
	     (tags (tag-sentence-viterbi-initial words))
	     (word-count (length words)))
	(incf sentence-count)
	(incf total-word-count word-count)
	(loop for word in words
	   for tag in tags 
	   for i from 1 to word-count do
	     (format t "~A~c~A" word separator tag)
	     (unless (= i word-count)
	       (format t " ")))
	(terpri)))    
    (let* ((end-time (get-universal-time))
	   (total-time (- end-time start-time)))
      (format *error-output* "Tagged ~:D sentences, ~:D words, in ~,2f seconds (~,2f words / second)~%" sentence-count total-word-count total-time (/ total-word-count total-time))
      )))

(defun viterbi-one-step (word scores back-pointers prev-scores prev-back-pointers)
  (let* ((model *pos-model*)
	 (tag-count (pos-model-tag-count model))
	 (tag-set (pos-model-tag-set model))
	 (word-id (get-word-id word *pos-model*)))
    (loop for tag across tag-set
       for j from 0 below tag-count 
       for tag-id = j then j
       for best-score = sb-ext:single-float-negative-infinity
       for best-tag = nil do
	 (loop for prev-tag across tag-set
	    for k from 0 below tag-count
	    for prev-tag-id = k then k
	    for prev-score = (aref prev-scores k)
	    for t-2-pos = (aref prev-back-pointers k)
	    for this-prob = (if (/= prev-score SB-EXT:SINGLE-FLOAT-NEGATIVE-INFINITY)
				(log-p-hat-model word-id tag-id prev-tag-id t-2-pos model)
				0.0)
	    for net-score = (+ this-prob prev-score) do
	      (when (> net-score best-score)
		(setf best-score net-score)
		(setf best-tag prev-tag)))
	 (setf (aref back-pointers j) (get-tag-id best-tag model))
	 (setf (aref scores j) best-score))))

(defun viterbi-last-step (word scores back-pointers prev-scores prev-back-pointers)
  (assert (string-equal word "</s>"))
  (let* ((model *pos-model*)
	 (tag-count (pos-model-tag-count model))
	 (tag-set (pos-model-tag-set *pos-model*)))
    (let ((best-overall-score sb-ext:single-float-negative-infinity)
	  (best-last-tag nil))
      (loop for tag across tag-set
	 for i from 0 below tag-count
	 for cumulative-score = (aref prev-scores i)
	 for prev-tag-id = (aref prev-back-pointers i)
	 for last-score = (if prev-tag-id
			      (log (p-hat-interpolated prev-tag-id i (get-tag-id :|</s>| model) model))
			      sb-ext:single-float-negative-infinity)
	 for net-score = (+ last-score cumulative-score) do
	   (setf (aref scores i) net-score)
	   (when (> net-score best-overall-score)
	     (setf best-overall-score net-score)
	     (setf best-last-tag i)))
      ;; Save the best back pointer to the end sentence tag location
      (setf (aref back-pointers (get-tag-id :|</s>| model)) best-last-tag)
      (setf (aref scores (get-tag-id :|</s>| model)) best-overall-score)
      best-last-tag)))

(defun position-of-reduced-seq-item (seq function)
  (let* ((reduced (reduce function seq))
	 (position (position reduced seq)))
    position))

(defun position-of-min (seq)
  (position-of-reduced-seq-item seq #'min))

(defun position-of-max (seq)
  (position-of-reduced-seq-item seq #'max))



