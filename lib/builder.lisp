;; Copyright (c) 2018 tikubonn.
;; Released under the MIT license 
;; http://opensource.org/licenses/mitlicense.php 

;; package

(provide 'snake-feet-builder)

(defpackage snake-feet-builder
  (:use :common-lisp)
  (:export :list-builder :make-list-builder :push-list-builder 
    :push-last-list-builder :length-list-builder :result-list-builder 
    :result-list-builder-as-array))

(in-package :snake-feet-builder)

;; structure 

(defstruct list-builder 
  (top nil :type list)
  (bottom nil :type list)
  (length 0 :type fixnum))

;; functions 

(defun push-list-builder (item builder)
  (declare
    (type list-builder builder)
    (optimize (speed 3)))
  (cond 
    ((and (null (list-builder-top builder))
       (null (list-builder-bottom builder)))
      (let ((seq (cons item nil)))
        (setf (list-builder-top builder) seq)
        (setf (list-builder-bottom builder) seq)))
    (t 
      (let ((seq (cons item (list-builder-top builder))))
        (setf (list-builder-top builder) seq)))))

(defun push-last-list-builder (item builder)
  (declare
    (type list-builder builder)
    (optimize (speed 3)))
  (cond 
    ((and (null (list-builder-top builder))
       (null (list-builder-bottom builder)))
      (let ((seq (cons item nil)))
        (setf (list-builder-top builder) seq)
        (setf (list-builder-bottom builder) seq)
        (incf (list-builder-length builder))))
    (t 
      (let ((seq (cons item nil)))
        (setf (cdr (list-builder-bottom builder)) seq)
        (setf (list-builder-bottom builder) seq)
        (incf (list-builder-length builder))))))

(defun length-list-builder (builder)
  (declare
    (type list-builder builder)
    (optimize (speed 3)))
  (list-builder-length builder))

(defun result-list-builder (builder)
  (declare
    (type list-builder builder)
    (optimize (speed 3)))
  (list-builder-top builder))

(defun result-list-builder-as-array (builder &rest options)
  (declare
    (type list-builder builder))
  (let ((sequence (apply #'make-array (length-list-builder builder) options)))
    (prog1 sequence 
      (loop for element in (result-list-builder builder)
        for index from 0 below (length-list-builder builder) do 
        (setf (aref sequence index) element)))))

;; (defmacro result-list-builder-as-array (builder-form &rest options)
;;   (let ((builder (gensym)))
;;     `(let ((,builder ,builder-form))
;;        (let ((seq (make-array (length-list-builder ,builder) ,@options)))
;;          (prog1 seq
;;            (loop for element in (result-list-builder ,builder) 
;;              for index from 0 below (length-list-builder ,builder) do
;;              (setf (aref seq index) element)))))))
