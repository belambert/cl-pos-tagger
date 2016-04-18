;;;; Ben Lambert (ben@benjaminlambert.com)

(cl-user::file-summary "Defines the pos-tagger package.")

(defpackage :pos-tagger
  (:use :common-lisp :blambert-util :alexandria) ;; :bordeaux-fft :cl-ppcre )
  (:export :train-model :tag-sentence-string :tag-sentence :get-tag :save-model :load-model :tag-file))


