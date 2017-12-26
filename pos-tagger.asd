;;;; -*- Mode: Lisp -*- 

(defsystem "pos-tagger"
  :description "Basic HMM-based POS tagger"
  :version "0.2.0"
  :author "Ben Lambert"
  :licence "Apache-2.0"
  :serial t
  :components
  ((:module src
	    :serial t
	    :components ((:file "package")
			 (:file "model")
			 (:file "train")
			 (:file "tag")
			 (:file "evaluate"))))
  :depends-on (:blambert-util :cl-fad :split-sequence :alexandria :cl-store))
