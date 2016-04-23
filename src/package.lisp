;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(defpackage :pos-tagger
  (:use :common-lisp :blambert-util :alexandria)
  (:export :train-model :tag-sentence-string :tag-sentence :get-tag :save-model :load-model :tag-file))


