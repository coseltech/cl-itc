;;;; bitvector-integer.lisp
;;;; thanks to edgar-rft, http://lispforum.com/viewtopic.php?f=2&t=1205#p6269 

(in-package #:cl-itc)

(defun bit-vector->integer (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))

(defun integer-bits-to-size (integer size)
  (let* ((bin (integer->bit-vector integer))
         (len (length bin)))
    (if (> len size)
        (subseq bin (- len size) len)
        (concatenate 'bit-vector
                     (make-list (- size len) :initial-element 0)
                     bin))))
