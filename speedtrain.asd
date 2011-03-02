;; speedtrain ASDF definition file.

(in-package #:cl-user)
(defpackage #:speedtrain-system (:use #:cl #:asdf))
(in-package #:speedtrain-system)

(defsystem speedtrain
  :name "speedtrain"
  :maintainer "Jonathan Fischer <jonathan@mohiji.org>"
  :license "TBD"
  :description "Tiny web framework, built on Mongrel2"
  :depends-on (:split-sequence :zeromq :st-json :html-template :cl-ppcre)
  :serial t
  :components ((:module "speedtrain"
                        :components ((:file "package")
                                     (:file "cookies")
                                     (:file "http")
                                     (:file "mongrel2")
                                     (:file "request")
                                     (:file "test")))))


