;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(cl-user::file-summary "Defines the pos-tagger package.")

(defpackage :pos-tagger
  (:use :common-lisp :blambert-util :alexandria) ;; :bordeaux-fft :cl-ppcre )
  ;;(:import-from :metatilities :defclass* :defclass-brief)
  (:export :train-model :tag-sentence-string :tag-sentence :get-tag :save-model :load-model :tag-file)
  )


