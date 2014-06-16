
;;;; cl-itc.asd

(asdf:defsystem #:cl-itc
  :serial t
  :description "Interval Tree Clocks for Common Lisp, ported from Erlang source"
  :author "Miko Kuijn <miko@pareidolia.nl>"
  :license "EPL 1.1"
  :depends-on (#:optima)
  :components ((:file "package")
               (:file "classes")
               (:file "bitvector-integer")
               (:file "cl-itc")))

