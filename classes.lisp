;;;; classes.lisp

(in-package #:cl-itc)

(defclass id ()
  ((leaf-p :initform t :initarg :leaf-p)
   (value :initform 1 :initarg :value :type integer)
   (left :initform nil :initarg :left :type id)
   (right :initform nil :initarg :right :type id)))

(closer-mop:finalize-inheritance (find-class 'id))

(defclass event ()
  ((leaf-p :initform t :initarg :leaf-p :reader leaf-p)
   (value :initform 0 :initarg :value :reader value :type integer)
   (left :initform nil :initarg :left :reader left :type event)
   (right :initform nil :initarg :right :reader right :type event)))

(closer-mop:finalize-inheritance (find-class 'event))

(defclass stamp ()
  ((id :initform (make-instance 'id) :initarg :id :type id)
   (event :initform (make-instance 'event) :initarg :event :type event)))

(closer-mop:finalize-inheritance (find-class 'stamp))
