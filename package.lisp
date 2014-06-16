;;;; package.lisp

(defpackage #:cl-itc
  (:use #:cl
        #:optima)
  (:export
   :+seed+
   :event
   :join
   :fork
   :peek
   :leq
   :len
   :str
   :encode
   :decode))
