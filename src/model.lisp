;; Copyright 2012-2018 Ben Lambert

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(in-package :pos-tagger)
(pushnew :pos-tagger *features*)

(defvar *pos-model* nil)

(defclass pos-model ()
  ((lex-probs)
   (history-probs)
   (lex-counts :initarg :lex-counts :reader pos-model-lex-counts)
   (history-counts :initarg :history-counts :reader pos-model-history-counts)
   (1gram-counts :initarg :1gram-counts :reader pos-model-1gram-counts :type (simple-array fixnum))
   (2gram-counts :initarg :2gram-counts :reader pos-model-2gram-counts)
   (3gram-counts :initarg :3gram-counts :reader pos-model-3gram-counts)
   ;; The tags, their counts, and tables
   (token-count :initarg :token-count :reader pos-model-token-count)
   (tag-set :initarg :tag-set :reader pos-model-tag-set)
   (tag-count :initarg :tag-count :reader pos-model-tag-count)
   (tag-table :initarg :tag-table :reader pos-model-tag-table)
   ;; We use this table to map from words to indicies
   (vocab-size :initarg :vocab-size :reader pos-model-vocab-size)
   (vocab :initarg :vocab :reader pos-model-vocab)
   (vocab-table :initarg :vocab-table :reader pos-model-vocab-table)
   (lambda1 :initarg :lambda1 :accessor pos-model-lambda1)
   (lambda2 :initarg :lambda2 :accessor pos-model-lambda2)
   (lambda3 :initarg :lambda3 :accessor pos-model-lambda3)))

;;;; Generic methods
(defgeneric p-hat-1gram (tag pos-model))
(defgeneric p-hat-2gram (tag tag-1 pos-model))
(defgeneric p-hat-3gram (tag tag-1 tag-2 pos-model))
(defgeneric p-hat-interpolated (tag tag-1 tag-2 pos-model))
(defgeneric p-hat-lexical (word tag pos-model &key smoothing-param))
(defgeneric p-hat-model (word tag tag-1 tag-2 pos-model))
(defgeneric log-p-hat-model (word tag tag-1 tag-2 pos-model))
(defgeneric get-pos-tag-prob-array (word history pos-model &key array))
(defgeneric get-tag-id (tag pos-model))
(defgeneric tag-word (word history pos-model))
(defgeneric get-tag (tag pos-model))
(defgeneric get-word (word pos-model))

(defun number-string-p (string)
  "Check if the string is composed entirely of numeric characters, and common, period, and hyphen."
  (every (lambda (x) (or (digit-char-p x) (char-equal x #\,) (char-equal x #\.)  (char-equal x #\-))) string))

(defmethod get-tag-id (tag (model pos-model))
  "Given a tag, get its numerical ID. e.g. (get-tag-id :NN) --> 3"
  (gethash tag (pos-model-tag-table model)))

(defmethod get-word-id (word (model pos-model))
  "Given a word, get its numerical ID. e.g. (get-word-id 'the') --> 15"
  (gethash word (pos-model-vocab-table model)))

(defmethod get-tag (id (model pos-model))
  "Given a numerical tag ID, get the tag. e.g. (get-tag 3) --> :NN"
  (if (numberp id)
      (aref (pos-model-tag-set model) id)
      nil))

(defmethod get-word (id (model pos-model))
  "Given a numerical word ID, get the word. e.g. (get-word 15) --> 'the'"
  (if (numberp id)
      (aref (pos-model-vocab model) id)
      nil))

(defun in-vocab-p (word model)
  "Predicate, returns true iff the word is in the model vocab."
  (gethash word (pos-model-vocab-table model)))

(defmethod p-hat-1gram (tag (model pos-model))
  (declare (optimize (speed 3))
	   ((or fixnum keyword) tag))
  (let ((n (aref (the (simple-array fixnum) (pos-model-1gram-counts model)) tag))
	(d (pos-model-token-count model)))
    (declare (fixnum n d))
    (if (zerop d)
	0.0
	(/ (coerce n 'single-float) d))))

(defmethod p-hat-2gram (tag tag-1 (model pos-model))
  (declare (optimize (speed 3))
	   ((or fixnum keyword) tag tag-1))
  (let ((n (aref (the (simple-array fixnum) (pos-model-2gram-counts model)) tag-1 tag))
	(d (aref (the (simple-array fixnum) (pos-model-1gram-counts model)) tag-1)))
    (declare (fixnum n d))
    (if (zerop d)
	0.0
	(/ (coerce n 'single-float) d))))

(defmethod p-hat-3gram (tag tag-1 tag-2 (model pos-model))
  (declare (optimize (speed 3))
	   ((or fixnum keyword) tag tag-1 tag-2))
  (let ((n (aref (the (simple-array fixnum) (pos-model-3gram-counts model)) tag-2 tag-1 tag))
	(d (aref (the (simple-array fixnum) (pos-model-2gram-counts model)) tag-2 tag-1)))
    (declare (fixnum n d))
    (if (zerop d)
	0.0
	(/ (coerce n 'single-float) d))))

(defmethod p-hat-interpolated (tag tag-1 tag-2 (model pos-model))
  (declare (optimize (speed 3)))
  (+ (* (the single-float (pos-model-lambda1 model)) (the single-float (p-hat-1gram tag model)))
     (* (the single-float (pos-model-lambda2 model)) (the single-float (p-hat-2gram tag tag-1 model)))
     (* (the single-float (pos-model-lambda3 model)) (the single-float (p-hat-3gram tag tag-1 tag-2 model)))))

(defmethod p-hat-lexical (word tag (model pos-model) &key (smoothing-param 0.5))
  (declare (optimize (speed 3))
	   (single-float smoothing-param))
  
  (unless (numberp tag)
    (setf tag (get-tag-id tag model)))
  (unless (numberp word)
    (setf word (get-word-id word model)))

  (let ((n (if word      ;;; # of times we saw this word with this tag
	       (coerce (the fixnum (aref (the (simple-array fixnum) (pos-model-lex-counts model)) word tag)) 'single-float)
	       smoothing-param))
	(d (coerce (aref (the (simple-array fixnum) (pos-model-1gram-counts model)) tag) 'single-float))   ;;; # of times we saw this tag
	(lex-prob 0.0))
    (declare (single-float n d lex-prob))
    (incf n smoothing-param)
    (incf d (* smoothing-param (the fixnum (pos-model-vocab-size model))))
    (assert (not (zerop d)))
    (setf lex-prob (/ n d))
    (when (zerop lex-prob)
      (error "ZERO LEXICAL PROBABILITY FOR WORD/TAG:  ~A / ~A~%" word tag))
    lex-prob))

(defmethod p-hat-model (word tag tag-1 tag-2 (model pos-model))
  (let ((tag-prob (p-hat-interpolated tag tag-1 tag-2 model))
	(lex-prob (p-hat-lexical word tag model)))
    (declare (single-float tag-prob lex-prob))
    (* tag-prob lex-prob)))

(defmethod log-p-hat-model (word tag tag-1 tag-2 (model pos-model))
  (let ((prob (p-hat-model word tag tag-1 tag-2 model)))
    (if (zerop prob)
	sb-ext:single-float-negative-infinity
	(log prob))))

;; We may not really using these
(defmethod tag-word (word history (model pos-model))
  (let ((best-tag nil)
	(best-prob 0.0))
  (loop for tag in (pos-model-tag-set model)
       for tag-prob = (p-hat-model word tag
				   (elt history (- (length history) 2))
				   (elt history (- (length history) 3))
				   model) do
       (when (> tag-prob best-prob)
	 (setf best-prob tag-prob)
	 (setf best-tag tag)))
  best-tag))

(defmethod get-pos-tag-prob-array (word history (model pos-model) &key array)
  ;; If an array is given, use it, rather than cons a new one
  (let ((prob-array (if array
			array
			(make-array (pos-model-tag-count model) :element-type 'single-float))))
    (loop for tag in (pos-model-tag-set model)
       for i from 0 below (pos-model-tag-count model)
       for tag-prob = (p-hat-model word tag 
				   (elt history (- (length history) 2))
				   (elt history (- (length history) 3))
				   model) do
	 (setf (aref prob-array i) tag-prob))
    prob-array))
