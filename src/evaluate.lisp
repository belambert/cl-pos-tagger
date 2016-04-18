;;;; Ben Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :pos-tagger)
(cl-user::file-summary "Evaluating POS taggings")

(defun print-confusion-matrix (matrix)
  "Given a matrix of confusions, print it readably."
  (write-string "     ")
  (loop for y from 0 below (pos-model-tag-count *pos-model*) do
       (format t "~5@A" (get-tag y *pos-model*)))
  (terpri)
  (loop for x from 0 below (pos-model-tag-count *pos-model*) do
       (format t "~5A " (get-tag x *pos-model*))
       (loop for y from 0 below (pos-model-tag-count *pos-model*)
	  for count = (aref matrix x y) do
	    (if (< count 10000)
		(format t "~4D " (aref matrix x y))		
		(format t "~4,1,,,'*,,e " (aref matrix x y))))
       (terpri)))

(defun simplify-pos (pos)
  "Map Penn Treebank POS tags for various forms of noun, adjective,
   verb, and adverb to a single tag for each of the four categories."
  (case pos
    ((:NN :NNS :NNP :NNPS) :NN)
    ((:VB :VBD :VBG :VBN :VBP :VBZ) :VB)
    ((:JJ :JJR :JJS) :JJ)
    ((:RB :RBR :RBS) :RB)
    (otherwise pos)))

(defun simplify-sentence-pos (tokens)
  "Given a tagged sentence, convert the tags to the simplified version."
  (mapcar (lambda (x) (list (first x) (simplify-pos (second x)))) tokens))

(defun evaluate (hyp-file ref-file &key model (print-instances nil) (print-words nil) (print-predictions t) (simplify-pos nil) (print-singleton-words nil) (print-confusion-matrix nil))
  "Huge gnarly function for printing out a POS tagger evaluation."
  (when model
    (load-model model))
  (let ((correct-table (make-array (list (pos-model-tag-count *pos-model*) (pos-model-tag-count *pos-model*)) :initial-element 0))
	(incorrect-table (make-array (list (pos-model-tag-count *pos-model*) (pos-model-tag-count *pos-model*)) :initial-element 0))
	(matrix (make-array (list (pos-model-tag-count *pos-model*) (pos-model-tag-count *pos-model*)) :initial-element 0))
	(sentence-count 0)
	(total-count 0)
	(correct-count 0)
	(in-vocab-count 0)
	(in-vocab-correct-count 0)
	(out-vocab-count 0)
	(out-vocab-correct-count 0)
	(in-vocab-incorrect (make-hash-table :test 'equalp))
	(out-vocab-incorrect (make-hash-table :test 'equalp))
	(mistakes (make-hash-table :test 'equalp))
	(mistakes-shoulda-been (make-hash-table :test 'equalp))
	(word-counts (make-hash-table :test 'equalp))
	(word-counts-correct (make-hash-table :test 'equalp))
	(nnp-oovs 0)
	(oov-mistakes 0))
  (with-open-file (ref-stream ref-file :direction :input)
    (with-open-file (hyp-stream hyp-file :direction :input)      
      (loop for ref = (read-line ref-stream nil)
	 for hyp = (read-line hyp-stream nil)
	 for perfect-p = t then t
	 for i = 1 then (1+ i)
	 while (and ref hyp) do
	   (incf sentence-count)
	   (setf ref (line->pos-token-list ref :add-sentence-boundaries nil))
	   (setf hyp (line->pos-token-list hyp :add-sentence-boundaries nil))
	   (when simplify-pos
	     (setf ref (simplify-sentence-pos ref))
	     (setf hyp (simplify-sentence-pos hyp)))
	   (assert (= (length ref) (length hyp)))
	   (loop for (ref-word ref-tag) in ref
	      for (hyp-word hyp-tag) in hyp do
		(when (and ref-word hyp-word (string-not-equal ref-tag "") (string-not-equal hyp-tag "")
			   (string-not-equal ref-tag "."))
		(assert (string-equal hyp-word ref-word))
		(setf hyp-tag (alexandria:make-keyword hyp-tag))
		(setf ref-tag (alexandria:make-keyword ref-tag))
		(let ((hyp-id (get-tag-id hyp-tag *pos-model*))
		      (ref-id (get-tag-id ref-tag *pos-model*)))
		  (incf total-count)
		  (incf (gethash ref-word word-counts 0))
		  (if (in-vocab-p ref-word)
		      (incf in-vocab-count)
		      (incf out-vocab-count))
		  (incf (aref matrix ref-id hyp-id))
		  (if (equal ref-tag hyp-tag)
		      ;; CORRECT
		      (progn
			(incf correct-count)
			(incf (gethash ref-word word-counts-correct 0))
			(if (in-vocab-p ref-word)
			    (incf in-vocab-correct-count)
			    (incf out-vocab-correct-count))
			(incf (aref correct-table ref-id hyp-id)))
		      ;; INCORRECT
		      (progn 
			(setf perfect-p nil)
			(incf (aref incorrect-table ref-id hyp-id))
			(push hyp-tag (gethash ref-word mistakes))
			(push ref-tag (gethash ref-word mistakes-shoulda-been))
			(when (not (in-vocab-p ref-word))
			  (when (or (string-equal ref-tag "NNP") (string-equal ref-tag "NNPS"))
			    (incf nnp-oovs))
			  (incf oov-mistakes))
			(if (in-vocab-p ref-word)
			    (incf (gethash ref-word in-vocab-incorrect 0))
			    (incf (gethash ref-word out-vocab-incorrect 0)))
			)
		  ))))
	   (when (and print-instances (not perfect-p))
	     (format t "~6:D REF: ~{~{~A~^_~} ~}~%" i ref)
	     (format t "~6:D HYP: ~{~{~A~^_~} ~}~%" i hyp)))))
  ;; Print the specific words we got wrong
  (when print-words
    (setf in-vocab-incorrect (stable-sort (hash-table-alist in-vocab-incorrect) '> :key 'cdr))
    (setf out-vocab-incorrect (stable-sort (hash-table-alist out-vocab-incorrect) '> :key 'cdr))
    (format t "~%~%IN VOCAB (~:D word types):~%" (length in-vocab-incorrect))
    (format t "~20A ~4A [ ~6A / ~6A ]  ...~%" "WORD" "INCOR" "COR" "TOTAL")
    (loop for (word . count) in in-vocab-incorrect do
	 (when (or (> count 1) print-singleton-words)
	   (format t "~20A ~:D [ ~6:d / ~6:d ] [CORRECT SET: ~{~{~A~^ ~}~^, ~}] PREDICTED SET: ~{~{~A~^ ~}~^, ~}~%" 
		   word count (gethash word word-counts-correct 0) (gethash word word-counts)
		   (list->count-list (gethash word mistakes-shoulda-been))
		   (when print-predictions (list->count-list (gethash word mistakes))))))
    (format t "~%~%OUT OF VOCAB (~:D word types):~%" (length out-vocab-incorrect))
    (format t "# that are NNP(s): ~:D of ~:D tokens~%" nnp-oovs oov-mistakes)
    (format t "~20A ~4A [ ~6A / ~6A ]  ...~%" "WORD" "INCOR" "COR" "TOTAL")
    (loop for (word . count) in out-vocab-incorrect do
	 (when (or (> count 1) print-singleton-words)
	   (format t "~20A ~:D [ ~6:d / ~6:d ] [CORRECT SET: ~{~{~A~^ ~}~^, ~}] PREDICTED SET: ~{~{~A~^ ~}~^, ~}~%"
		   word count (gethash word word-counts-correct 0) (gethash word word-counts)
		   (list->count-list (gethash word mistakes-shoulda-been))
		   (when print-predictions (list->count-list (gethash word mistakes)))))))

  ;; Print conf matrix:
  (when print-confusion-matrix
    (print-confusion-matrix matrix))
  ;; Print a summary
  (format t "Sentence count:         ~:D~%" sentence-count)
  (format t "Accuracy:               ~,2,2f % (~:D / ~:D)~%" (/ correct-count total-count) correct-count total-count)
  (format t "In vocab accuracy:      ~,2,2f % (~:D / ~:D)~%" (/ in-vocab-correct-count in-vocab-count) in-vocab-correct-count in-vocab-count)
  (format t "Out of vocab accuracy:  ~,2,2f % (~:D / ~:D)~%" (/ out-vocab-correct-count out-vocab-count) out-vocab-correct-count out-vocab-count)))
		      


			




