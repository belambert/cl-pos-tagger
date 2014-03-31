;;-*- Mode: Lisp -*- 
;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(defsystem "pos-tagger"
  :description "Basic HMM-based POS tagger"
  :version "0.2.0"
  :author "Benjamin Lambert"
  :licence "All rights reserved"
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

