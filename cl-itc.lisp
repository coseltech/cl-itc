;;;; cl-itc.lisp
;;;; Transcribed by Miko Kuijn <miko@pareidolia.nl> from original
;;;; Erlang source. Original description follows:
;;;;
;;;; -author("Paulo Sergio Almeida <psa@di.uminho.pt>").
;;;; ``The contents of this file are subject to the Erlang Public License,
;;;; Version 1.1, (the "License"); you may not use this file except in
;;;; compliance with the License. You should have received a copy of the
;;;; Erlang Public License along with this software. If not, it can be
;;;; retrieved via the world wide web at http://www.erlang.org/. 
;;;;  Software distributed under the License is distributed on an "AS IS"
;;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
;;;; the License for the specific language governing rights and limitations
;;;;  under the License.

(in-package #:cl-itc)
  
(defun make-stamp (id event)
  (make-instance 'stamp :id id :event event))

(defun make-id-node (left right)
  (make-instance 'id :leaf-p nil :value nil :left left :right right))

(let ((leaf-zero (make-instance 'id :leaf-p t :value 0))
      (leaf-one (make-instance 'id :leaf-p t :value 1)))
  (defun make-id-zero ()
    leaf-zero)
  (defun make-id-one ()
    leaf-one))

(defun make-event-node (value left right)
  (make-instance 'event :leaf-p nil :value value :left left :right right))

(defun make-event-leaf (value)
  (make-instance 'event :leaf-p t :value value))

(let ((leaf-zero (make-event-leaf 0)))
  (defun make-event-zero ()
    leaf-zero))

(defpattern id-leaf (value)
  `(id :leaf-p (not nil) ,@(when value (list :value value))))

(defpattern id-node (left right)
  `(id :leaf-p nil
       ,@(when left (list :left left))
       ,@(when right (list :right right))))

(defpattern id-zero ()
  '(id-leaf 0))

(defpattern id-one ()
  '(id-leaf 1))

(defpattern event-node (value left right)
  `(event :leaf-p nil
          ,@(when value (list :value value))
          ,@(when left (list :left left))
          ,@(when right (list :right right))))

(defpattern event-leaf (value)
  `(event :leaf-p t ,@(when value (list :value value))))

(defpattern event-zero ()
  '(event-leaf 0))

(defmacro make-bits (&rest elts)
  `(concatenate 'bit-vector
                ,@(map 'list
                       (lambda (elt)
                         (ematch elt
                           ((list :b a b) (list 'integer-bits-to-size a b))
                           (e e))) elts)))

(defpattern bits (&rest elts)
  `(list* ,@(mapcan (lambda (elt)
                      (if (and (listp elt) (= 2 (length elt)))
                          (destructuring-bind (int len) elt
                            (coerce (integer-bits-to-size int len) 'list))
                          (list elt))) elts)))

;;;

(eval-when (:compile-toplevel)
  (defconstant +seed+ (make-instance 'stamp)))
(defconstant +large-constant+ 1000)

(defun join (s1 s2)
  (with-slots ((i1 id) (e1 event)) s1
    (with-slots ((i2 id) (e2 event)) s2
      (make-stamp (sum i1 i2) (join-ev e1 e2)))))

(defun fork (s)
  (with-slots (id event) s
    (with-slots ((i1 left) (i2 right)) (split id)
      (values (make-stamp i1 event)
            (make-stamp i2 event)))))

(defun peek (s)
  (with-slots (event) s
    (values
     (make-stamp (make-id-zero) event)
     s)))

(defun event (s)
  (with-slots (id (old-event event)) s
    (let ((filled-event (itc-fill id old-event)))
      (make-stamp id (if (equals old-event filled-event)
                         (grow id old-event)
                         filled-event)))))

(defun equals (e1 e2)
  (or (and (leaf-p e1) (leaf-p e2) (eq (value e1) (value e2)))
      (and (not (leaf-p e1))
           (not (leaf-p e2))
           (eq (value e1) (value e2))
           (equals (left e1) (left e2))
           (equals (right e1) (right e2)))))

;;; Lesser-equal

(defun leq (s1 s2) (leq-ev (slot-value s1 'event) (slot-value s2 'event)))

(defun leq-ev (e1 e2)
  (ematch (cons e1 e2)
    ((cons
      (event-node n1 l1 r1)
      (event-node n2 l2 r2))
     (and (<= n1 n2)
          (leq-ev (lift n1 l1) (lift n2 l2))
          (leq-ev (lift n1 r1) (lift n2 r2))))
    ((cons
      (event-node n1 l1 r1)
      (event :value n2))
     (and (<= n1 n2)
          (leq-ev (lift n1 l1) n2)
          (leq-ev (lift n1 r1) n2)))
    ((cons
      (event :value n1)
      (event :value n2))
     (<= n1 n2))))

;;; Normal-form

(defun norm-id (id)
  (ematch id
    ((id-node (id-zero) (id-zero)) (make-id-zero))
    ((id-node (id-one) (id-one)) (make-id-one))
    ((id) id)))

(defun norm-ev (e)
  (ematch e
    ((event-node n
            (event-leaf m)
            (event-leaf (eql m)))
     (make-event-leaf (+ n m)))
    ((event-node n l r)
     (let ((m (min (base l) (base r))))
       (make-event-node (+ n m) (drop m l) (drop m r))))
    ((event) e)))

;;;

(defun sum (i1 i2)
  (ematch (cons i1 i2)
    ((cons (id-zero) x) x)
    ((cons x (id-zero)) x)
    ((cons (id-node l1 r1) (id-node l2 r2))
     (norm-id (make-id-node (sum l1 l2) (sum r1 r2))))))

(defun split (id)
  (ematch id
    ((id-zero)
     (make-id-node (make-id-zero) (make-id-zero)))
    ((id-one)
     (make-id-node (make-id-node (make-id-one) (make-id-zero))
                   (make-id-node (make-id-zero) (make-id-one))))
    ((id-node (id-zero) i)
     (with-slots ((i1 left) (i2 right)) (split i)
       (make-id-node (make-id-node (make-id-zero) i1)
                     (make-id-node (make-id-zero) i2))))
    ((id-node i (id-zero))
     (with-slots ((i1 left) (i2 right)) (split i)
       (make-id-node (make-id-node i1 (make-id-zero))
                     (make-id-node i2 (make-id-zero)))))
    ((id-node i1 i2)
     (make-id-node (make-id-node i1 (make-id-zero))
                   (make-id-node (make-id-zero) i2)))))

;;;

(defun join-ev (e1 e2)
  (ematch (cons e1 e2)
    ((cons (and (event-node n1 _ _) e1)
           (and (event-node (guard n2 (> n1 n2)) _ _) e2))
     (join-ev e2 e1))
    ((cons (event-node n1 l1 r1)
           (event-node (guard n2 (<= n1 n2)) l2 r2))
     (let ((d (- n2 n1)))
       (norm-ev (make-event-node n1 (join-ev l1 (lift d l2)) (join-ev r1 (lift d r2))))))
    ((cons (event-leaf n1)
           (and (event-node _ _ _) e2))
     (join-ev (make-event-node n1 (make-event-zero) (make-event-zero))
              e2))
    ((cons (and (event-node _ _ _) e1)
           (event-leaf n2))
     (join-ev e1
              (make-event-node n2 (make-event-zero) (make-event-zero))))
    ((cons (event-leaf n1)
           (event-leaf n2))
     (make-event-leaf (max n1 n2)))))

(defun itc-fill (id event)
  (ematch (cons id event)
    ((cons (id-zero)
           (and (event) e))
     e)
    ((cons (id-one)
           (and (event-node _ _ _) e))
     (make-event-leaf (height e)))
    ((cons (id)
           (and (event-leaf _) n))
     n)
    ((cons (id-node (id-one) r)
           (event-node n el er))
     (let* ((er1 (itc-fill r er))
            (d (max (height el) (base er1))))
       (norm-ev (make-event-node n (event-leaf d) er1))))
    ((cons (id-node l (id-one))
           (event-node n el er))
     (let* ((el1 (itc-fill l el))
            (d (max (height er) (base el1))))
       (norm-ev (make-event-node n el1 (event-leaf d)))))
    ((cons (id-node l r)
           (event-node n el er))
     (norm-ev (make-event-node n (itc-fill l el) (itc-fill r er))))))

(defun grow (id event)
  (ematch (cons id event)
    ((cons (id-one)
           (event-leaf n))
     (values
      (make-event-leaf (+ n 1))
      0))
    ((cons (id-node (id-zero) i)
           (event-node n l r))
     (multiple-value-bind (e1 h) (grow i r)
       (values
        (make-event-node n l e1)
        (+ h 1))))
    ((cons (id-node i (id-zero))
           (event-node n l r))
     (multiple-value-bind (e1 h) (grow i l)
       (values
        (make-event-node n e1 r)
        (+ h 1))))
    ((cons (id-node il ir)
           (event-node n l r))
     (multiple-value-bind (el hl) (grow il l)
       (multiple-value-bind (er hr) (grow ir r)
         (if (< hl hr)
             (values
              (make-event-node n el r)
              (+ hl 1))
             (values
              (make-event-node n l er)
              (+ hr 1))))))
    ((cons (and (id) i)
           (event-leaf n))
     (multiple-value-bind (e h)
         (grow i (make-event-node n (make-event-zero) (make-event-zero)))
       (values e (+ h +large-constant+))))))

;;;

(defun height (e)
  (ematch e
    ((event-node n l r)
     (+ n (max (height l) (height r))))
    ((event-leaf n)
     n)))

(defun base (e) (slot-value e 'value))

(defun lift (m e)
  (ematch e
    ((event-node n l r)
     (make-event-node (+ n m) l r))
    ((event-leaf n)
     (make-event-leaf (+ n m)))))

(defun drop (m e)
  (ematch e
    ((event-node (guard n (<= m n)) l r)
     (make-event-node (- n m) l r))
    ((event-leaf (guard n (<= m n)))
     (make-event-leaf (- n m)))))

;;;

(defun encode (s)
  (with-slots (id event) s
      (concatenate 'bit-vector (enci id) (ence event))))

(defun decode (b)
  (multiple-value-bind (i be) (deci (coerce b 'list))
    (multiple-value-ematch (dece be)
                           ((e nil)
                            (make-stamp i e)))))

(defun enci (id)
  (ematch id
    ((id-zero) (make-bits (:b 0 2) (:b 0 1)))
    ((id-one) (make-bits (:b 0 2) (:b 1 1)))
    ((id-node (id-zero) i) (make-bits (:b 1 2) (enci i)))
    ((id-node i (id-zero)) (make-bits (:b 2 2) (enci i)))
    ((id-node l r) (make-bits (:b 3 2) (enci l) (enci r)))))

(defun deci (code)
  (ematch code
    ((bits (0 2) (0 1) b) (values (make-id-zero) b))
    ((bits (0 2) (1 1) b) (values (make-id-one) b))
    ((bits (1 2) b)
     (multiple-value-bind (i b1) (deci b)
       (values (make-id-node (make-id-zero) i) b1)))
    ((bits (2 2) b)
     (multiple-value-bind (i b1) (deci b)
       (values (make-id-node i (make-id-zero)) b1)))
    ((bits (3 2) b)
     (multiple-value-bind (l b1) (deci b)
       (multiple-value-bind (r b2) (deci b1)
         (values (make-id-node l r) b2))))))

(defun ence (event)
  (ematch event
    ((event-node 0 (event-zero) r)
     (make-bits (:b 0 1) (:b 0 2) (ence r)))
    ((event-node 0 l (event-zero))
     (make-bits (:b 0 1) (:b 1 2) (ence l)))
    ((event-node 0 l r)
     (make-bits (:b 0 1) (:b 2 2) (ence l) (ence r)))
    ((event-node n (event-zero) r)
     (make-bits (:b 0 1) (:b 3 2) (:b 0 1) (:b 0 1) (encn n 2) (ence r)))
    ((event-node n l (event-zero))
     (make-bits (:b 0 1) (:b 3 2) (:b 0 1) (:b 1 1) (encn n 2) (ence l)))
    ((event-node n l r)
     (make-bits (:b 0 1) (:b 3 2) (:b 1 1) (encn n 2) (ence l) (ence r)))
    ((event-leaf n) (encn n 2))))

(defun dece (code)
  (ematch code
    ((bits (0 1) (0 2) b)
     (multiple-value-bind (r b1) (dece b)
       (values (make-event-node 0 (make-event-zero) r)
               b1)))
    ((bits (0 1) (1 2) b)
     (multiple-value-bind (l b1) (dece b)
       (values (make-event-node 0 l (make-event-zero))
               b1)))
    ((bits (0 1) (2 2) b)
     (multiple-value-bind (l b1) (dece b)
       (multiple-value-bind (r b2) (dece b1)
         (values (make-event-node 0 l r)
                 b2))))
    ((bits (0 1) (3 2) (0 1) (0 1) b)
     (multiple-value-bind (n b1) (dece b)
       (multiple-value-bind (r b2) (dece b1)
         (values (make-event-node n (make-event-zero) r)
                 b2))))
    ((bits (0 1) (3 2) (0 1) (1 1) b)
     (multiple-value-bind (n b1) (dece b)
       (multiple-value-bind (l b2) (dece b1)
         (values (make-event-node n l (make-event-zero))
                b2))))
    ((bits (0 1) (3 2) (1 1) b)
     (multiple-value-bind (n b1) (dece b)
       (multiple-value-bind (l b2) (dece b1)
         (multiple-value-bind (r b3) (dece b2)
           (values (make-event-node n l r)
                   b3)))))
    ((bits (1 1) b)
     (multiple-value-bind (n b1) (decn b 2 0)
       (values (make-event-leaf n)
               b1)))))

(defun encn (n b)
  (let ((m (ash 1 b)))
    (if (< n m)
        (make-bits (:b (- m 2) b) (:b n b))
        (encn (- n m) (+ b 1)))))

(defun decn (code b acc)
  (ematch code
    ((bits (0 1) r)
     (let ((n (bit-vector->integer (subseq r 0 b)))
           (r1 (subseq r b)))
       (values (+ n acc) r1)))
    ((bits (1 1) r)
     (decn r (+ b 1) (+ acc (ash 1 b))))))

;;;

(defun str (stamp)
  (with-slots (id event) stamp
    (list (stri id) (stre event))))

(defun stri (id)
  (ematch id
    ((id-zero) '("0"))
    ((id-one) nil)
    ((id-node (id-zero) i) `("R" ,@(stri i)))
    ((id-node i (id-zero)) `("L" ,@(stri i)))
    ((id-node l r) `(("L" ,@(stri l) "R" ,@(stri r))))))

(defun stre (event)
  (ematch event
    ((event-node n l (event-zero))
     `(,@(stre n) "L" ,@(stre l)))
    ((event-node n (event-zero) r)
     `(,@(stre n) "R" ,@(stre r)))
    ((event-node n l r)
     `(,@(stre n) ("L" ,@(stre l) "R" ,@ (stre r))))
    ((and (event-leaf n))
     `(,n))
    ((and (integer) (guard n (> n 0)))
     `(,n))
    (_ nil)))

(defmethod print-object ((obj stamp) stream)
  (print-unreadable-object (obj stream :type t)
    (princ (str obj) stream)))

(defmethod print-object ((obj id) stream)
  (print-unreadable-object (obj stream :type t)
    (princ (stri obj) stream)))

(defmethod print-object ((obj event) stream)
  (print-unreadable-object (obj stream :type t)
    (princ (stre obj) stream)))
