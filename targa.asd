(defpackage :targa-asd
  (:use :cl :asdf))

(in-package :targa-asd)

(defsystem :targa
  :name "targa"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Targa Image Loading for Common Lisp."
  :serial t
  :components ((:file "targa")))
