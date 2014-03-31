;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(declaim (optimize (debug 3)))
(in-package :pos-tagger)
(cl-user::file-summary "Training a POS tagger...")

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

   (token-count :initarg :token-count :reader pos-model-token-count)
   (tag-set :initarg :tag-set :reader pos-model-tag-set)
   (tag-count :initarg :tag-count :reader pos-model-tag-count)

   (vocab-size :initarg :vocab-size :reader pos-model-vocab-size)
   (vocab :initarg :vocab :reader pos-model-vocab)
   (vocab-table :initarg :vocab-table :reader pos-model-vocab-table)

   (lambda1 :initarg :lambda1 :accessor pos-model-lambda1)
   (lambda2 :initarg :lambda2 :accessor pos-model-lambda2)
   (lambda3 :initarg :lambda3 :accessor pos-model-lambda3)
   (tag-table :initarg :tag-table :reader pos-model-tag-table)))

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
  (every (lambda (x) (or (digit-char-p x) (char-equal x #\,) (char-equal x #\.)  (char-equal x #\-))) string))

(defmethod get-tag-id (tag (model pos-model))
  (gethash tag (pos-model-tag-table model)))

(defmethod get-word-id (word (model pos-model))
  (gethash word (pos-model-vocab-table model)))

(defmethod get-tag (id (model pos-model))
  (if (numberp id)
      (aref (pos-model-tag-set model) id)
      nil))

(defmethod get-word (id (model pos-model))
  (if (numberp id)
      (aref (pos-model-vocab model) id)
      nil))

(defun in-vocab-p (word)
  (gethash word (pos-model-vocab-table *pos-model*)))

(defmethod p-hat-1gram (tag (model pos-model))
  (declare (optimize (speed 3))
	   ((or fixnum keyword) tag))
  ;; (unless (numberp tag)
  ;;   (setf tag (get-tag-id tag model)))
  (let ((n (aref (the (simple-array fixnum) (pos-model-1gram-counts model)) tag))
	(d (pos-model-token-count model)))
    (declare (fixnum n d))
    (if (zerop d)
	0.0
	(/ (coerce n 'single-float) d))))

(defmethod p-hat-2gram (tag tag-1 (model pos-model))
  (declare (optimize (speed 3))
	   ((or fixnum keyword) tag tag-1))

  ;; (unless (numberp tag)
  ;;   (setf tag (get-tag-id tag model)))
  ;; (unless (numberp tag-1)
  ;;   (setf tag-1 (get-tag-id tag-1 model)))

  (let (;;(n (aref (the (simple-array fixnum) (pos-model-2gram-counts model)) tag tag-1))
	(n (aref (the (simple-array fixnum) (pos-model-2gram-counts model)) tag-1 tag))
	(d (aref (the (simple-array fixnum) (pos-model-1gram-counts model)) tag-1)))
    (declare (fixnum n d))
    (if (zerop d)
	0.0
	(/ (coerce n 'single-float) d))))

(defmethod p-hat-3gram (tag tag-1 tag-2 (model pos-model))
  (declare (optimize (speed 3))
	   ((or fixnum keyword) tag tag-1 tag-2))
  ;; (unless (numberp tag)
  ;;   (setf tag (get-tag-id tag model)))
  ;; (unless (numberp tag-1)
  ;;   (setf tag-1 (get-tag-id tag-1 model)))
  ;; (unless (numberp tag-2)
  ;;   (setf tag-2 (get-tag-id tag-2 model)))

  (let (;;(n (aref (the (simple-array fixnum) (pos-model-3gram-counts model)) tag tag-1 tag-2))
	;;(d (aref (the (simple-array fixnum) (pos-model-2gram-counts model)) tag-1 tag-2))
	(n (aref (the (simple-array fixnum) (pos-model-3gram-counts model)) tag-2 tag-1 tag))
	(d (aref (the (simple-array fixnum) (pos-model-2gram-counts model)) tag-2 tag-1))
	)
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
	(d (coerce (aref (the (simple-array fixnum) (pos-model-1gram-counts model)) tag) 'single-float))   ;;; # of times we saw this tag...
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
  ;;(declare (optimize (speed 3)))
  (let ((tag-prob (p-hat-interpolated tag tag-1 tag-2 model))
	(lex-prob (p-hat-lexical word tag model)))
    (declare (single-float tag-prob lex-prob))
    ;;(assert (not (zerop tag-prob)))
    ;;(assert (not (zerop lex-prob)))
    (* tag-prob lex-prob)))

(defmethod log-p-hat-model (word tag tag-1 tag-2 (model pos-model))
  ;;(declare (optimize (speed 3)))
  ;;(log (the (single-float (0.0) *) (p-hat-model word tag tag-1 tag-2 model)))
  ;;(format t "~A ~A ~A ~A~%" (get-word word model) (get-tag tag model) (get-tag tag-1 model) (get-tag tag-2 model))
  (let ((prob (p-hat-model word tag tag-1 tag-2 model)))
    ;;(pprint prob)
    (if (zerop prob)
	sb-ext:single-float-negative-infinity
	(log prob)
	)))

;; We're probably not really using these... right?
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
