;; Copyright (c) 2018 tikubonn.
;; Released under the MIT license 
;; http://opensource.org/licenses/mitlicense.php 

;; package 

(defpackage snake-feet 
  (:use :common-lisp)
  (:export :iterator :range :repeat :imap
    :ifiler :islice :iappend :ireverse :isort
    :ireduce :ifind-if :ifind :iposition-if :iposition
    :icount-if :icount :next :skip))

(in-package snake-feet)

;; condition 

(define-condition iterator-error (simple-error)
  ((message
     :initarg :message
     :initform ""
     :accessor iterator-error-message))
  (:report
    (lambda (condition stream)
      (format stream "iterator-error: ~a~%" 
        (iterator-error-message condition)))))

;; constant

(defconstant *stop-iteration* (make-symbol "*stop-iteration*"))

;; iterator

(defclass iterator nil nil)
(defmethod next ((iter iterator)) nil)
(defmethod skip ((iter iterator)) (next iter))
(defmethod copy (something) something)
(defmethod copy ((iter iterator)))
(defmethod copy-from ((iter iterator) (iter-from iterator)))

(defmethod iterator ((iter iterator))
  iter)

(defmethod to-list ((iter iterator))
  (let ((top nil) (bottom nil))
    (do 
      ((element))
      ((eq (setq element (next iter)) *stop-iteration*))
      (if (and (null top) (null bottom))
        (let ((con (cons element nil)))
          (setq top con)
          (setq bottom con))
        (let ((con (cons element nil)))
          (setf (cdr bottom) con)
          (setq bottom con))))
    top))

(defmethod to-array ((iter iterator))
  (let ((sequence (make-array 0 :fill-pointer 0 :adjustable t)))
    (prog1 sequence
      (do
        ((element))
        ((eq  (setq element (next iter)) *stop-iteration*))
        (vector-push-extend element sequence)))))

;; range

(defclass iterator-range (iterator)
  ((current
     :initarg :start
     :initform 0
     :accessor iterator-current)
    (start
      :initarg :start
      :initform 0
      :accessor iterator-start)
    (end
      :initarg :end
      :initform 0
      :accessor iterator-end)
    (step
      :initarg :step
      :initform 1
      :accessor iterator-step)))

(defmethod iterator-range-end? ((ran iterator-range))
  (cond 
    ((< (iterator-step ran) 0)
      (< (iterator-current ran) (iterator-end ran)))
    ((> (iterator-step ran) 0)
      (> (iterator-current ran) (iterator-end ran)))))

(defmethod next ((ran iterator-range))
  (if (iterator-range-end? ran) *stop-iteration*
    (prog1 (iterator-current ran)
      (incf (iterator-current ran)
        (iterator-step ran)))))

(defmethod copy ((iter iterator-range))
  (let ((niter (make-instance 'iterator-range)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-range) (iter-from iterator-range))
  (setf (iterator-current iter) (iterator-current iter-from))
  (setf (iterator-start iter) (iterator-start iter-from))
  (setf (iterator-end iter) (iterator-end iter-from))
  (setf (iterator-step iter) (iterator-step iter-from)))

(defmethod range (start &optional end  step)
  (cond 
    (step (make-instance 'iterator-range :start start :end end :step step))
    (end (make-instance 'iterator-range :start start :end end :step (if (< start end) 1 -1)))
    (t (make-instance 'iterator-range :start 0 :end start :step 1))))

;; repeat 

(defclass iterator-repeat (iterator)
  ((count
     :initarg :element
     :initform 0
     :accessor iterator-count)
    (element 
     :initarg :element
     :initform nil
     :accessor iterator-element)))

(defmethod next ((iter iterator-repeat))
  (if (> 0 (iterator-count iter)) *stop-iteration*
    (prog1 (iterator-element iter)
      (decf (iterator-count iter)))))

(defmethod copy ((iter iterator-repeat))
  (let ((niter (make-instance 'iterator-repeat)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-repeat) (iter-from iterator-repeat))
  (setf (iterator-count iter) (iterator-count iter-from))
  (setf (iterator-element iter) (iterator-element iter-from)))

(defmethod irepeat (count &optional element)
  (make-instance 'iterator-repeat :count count :element element))

;; list

(defclass iterator-list (iterator)
  ((cons
     :initarg :cons
     :initform nil
     :accessor iterator-cons)))

(defmethod next ((iter iterator-list))
  (if (null (iterator-cons iter)) *stop-iteration*
    (pop (iterator-cons iter))))

(defmethod to-list ((iter iterator-list))
  (copy-list (iterator-cons iter)))

(defmethod copy ((iter iterator-list))
  (let ((niter (make-instance 'iterator-list)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-list) (iter-from iterator-list))
  (setf (iterator-cons iter) (iterator-cons iter-from)))

(defmethod iterator ((lis list))
  (make-instance 'iterator-list :cons lis))

;; tree 

(defclass iterator-cons (iterator)
  ((nodes 
     :initform nil
     :accessor iterator-nodes)))

(defmethod next ((iter iterator-cons))
  (loop  with node 
    if (null (iterator-nodes iter)) return *stop-iteration* 
    else do (setq node (pop (iterator-nodes iter)))
    if (consp node) do 
    (push (cdr node) (iterator-nodes iter))
    (push (car node) (iterator-nodes iter))
    else return node))

(defmethod copy ((iter iterator-cons))
  (let ((niter (make-instance 'iterator-cons)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-cons) (iter-from iterator-cons))
  (setf (iterator-nodes iter) (iterator-nodes iter-from)))

;; vector 

(defclass iterator-vector (iterator)
  ((vector 
     :initarg :vector
     :accessor iterator-vector)
    (index
      :initform 0
      :accessor iterator-index)))

(defmethod next ((lis iterator-vector))
  (if (<= (length (iterator-vector lis)) (iterator-index lis)) *stop-iteration*
    (prog1 (aref (iterator-vector lis) (iterator-index lis))
      (incf (iterator-index lis)))))

(defmethod to-array ((iter iterator-vector))
  (subseq (iterator-vector iter) (iterator-index iter)))

(defmethod copy ((iter iterator-vector))
  (let ((niter (make-instance 'iterator-vector)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-vector) (iter-from iterator-vector))
  (setf (iterator-vector iter) (iterator-vector iter-from))
  (setf (iterator-index iter) (iterator-vector iter-from)))

(defmethod iterator ((vec vector))
  (make-instance 'iterator-vector :vector vec))

;; iterator 

(defclass iterator-iterator (iterator)
  ((iterator
     :initarg :iterator
     :initform nil
     :accessor iterator-iterator)))

(defmethod next ((iter iterator-iterator))
  (let ((element (next (iterator-iterator iter))))
    (if (eq element *stop-iteration*)
      *stop-iteration*
      element)))

(defmethod copy ((iter iterator-iterator))
  (let ((niter (make-instance 'iterator-iterator)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-iterator) (iter-from iterator-iterator))
  (setf (iterator-iterator iter) (copy (iterator-iterator iter-from))))

;; iterator-cache 

(defclass iterator-iterator-cache (iterator-iterator)
  ((collection
     :initform nil
     :accessor iterator-collection)))

(defmethod load-from-iterator ((iter iterator-iterator-cache))
  (let ((collection (to-array (iterator-iterator iter))))
    (setf (iterator-collection iter) collection)))

(defmethod did-load-from-iterator? ((iter iterator-iterator-cache))
  (if (iterator-collection iter) t nil))

(defmethod aref-cache ((iter iterator-iterator-cache) index)
  (if (< index (length (iterator-collection iter)))
    (values (aref (iterator-collection iter) index) t)
    (values nil nil)))

;; map 

(defclass iterator-map (iterator-iterator)
  ((function
     :initarg :function
     :accessor iterator-function)))

(defmethod next ((iter iterator-map))
  (let ((element (next (iterator-iterator iter))))
    (if (eq element *stop-iteration*) *stop-iteration*
      (funcall (iterator-function iter) element))))

(defmethod skip ((iter iterator-map))
  (next (iterator-iterator iter)))

(defmethod copy ((iter iterator-map))
  (let ((niter (make-instance 'iterator-map)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-map) (iter-from iterator-map))
  (setf (iterator-function iter) (iterator-function iter-from)))

(defmethod imap (function iter)
  (imap function (iterator iter)))

(defmethod imap (function (iter iterator))
  (make-instance 'iterator-map :function function :iterator iter))

;; filter

(defclass iterator-filter (iterator-iterator)
  ((function
     :initarg :function
     :accessor iterator-function)))

(defmethod next ((iter iterator-filter))
  (let ((element (next (iterator-iterator iter))))
    (if (eq element *stop-iteration*) *stop-iteration*
      (if (funcall (iterator-function iter) element) element
        (next iter)))))

(defmethod copy ((iter iterator-filter))
  (let ((niter (make-instance 'iterator-filter)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-filter) (iter-from iterator-filter))
  (setf (iterator-function iter) (iterator-function iter-from)))

(defmethod ifilter (function iter)
  (ifilter function (iterator iter)))

(defmethod ifilter (function (iter iterator))
  (make-instance 'iterator-filter :function function :iterator iter))

;; slice

(defclass iterator-slice (iterator-iterator)
  ((current
     :initform 0
     :accessor iterator-current)
    (start
      :initarg :start
      :initform 0
      :accessor iterator-start)
    (end
      :initarg :end
      :initform 0
      :accessor iterator-end)))

(defmethod next ((lis iterator-slice))
  (loop while (< (iterator-current lis) (iterator-start lis)) do
    (skip (iterator-iterator lis))
    (incf (iterator-current lis)))
  (if (< (iterator-end lis) (iterator-current lis)) *stop-iteration*
    (let ((element (next (iterator-iterator lis))))
      (if (eq element *stop-iteration*) 
        *stop-iteration*
        (prog1 element
          (incf (iterator-current lis)))))))

(defmethod copy ((iter iterator-slice))
  (let ((niter (make-instance 'iterator-slice)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-slice) (iter-from iterator-slice))
  (setf (iterator-current iter) (iterator-current iter-from))
  (setf (iterator-start iter) (iterator-start iter-from))
  (setf (iterator-end iter) (iterator-end iter-from)))

(defmethod islice (start end iter)
  (islice start end (iterator iter)))

(defmethod islice (start end (iter iterator))
  (make-instance 'iterator-slice :start start :end end :iterator iter))

;; append 

(defclass iterator-append (iterator-iterator)
  ((iterators 
     :initarg :iterators
     :initform nil
     :accessor iterator-iterators)))

(defmethod current-iterator ((iter iterator-append))
  (car (iterator-iterators iter)))

(defmethod next-current-iterator ((iter iterator-append))
  (next (current-iterator iter)))

(defmethod skip-current-iterator ((iter iterator-append))
  (skip (current-iterator iter)))

(defmethod select-next-iterator ((iter iterator-append))
  (setf (iterator-iterators iter)
    (cdr (iterator-iterators iter))))

(defmethod next ((iter iterator-append))
  (loop with element
    unless (iterator-iterators iter) return *stop-iteration*
    if (eq (setq element (next-current-iterator iter)) *stop-iteration*)
    do (select-next-iterator iter)
    else return element))

(defmethod skip ((iter iterator-append))
  (loop with element
    unless (iterator-iterators iter) return *stop-iteration*
    if (eq (setq element (skip-current-iterator iter)) *stop-iteration*)
    do (select-next-iterator iter)
    else return element))

(defmethod copy ((iter iterator-append))
  (let ((niter (make-instance 'iterator-append)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-append) (iter-from iterator-append))
  (setf (iterator-iterators iter) (mapcar 'copy (iterator-iterators iter-from))))

(defmethod iappend (&rest iters)
  (make-instance 'iterator-append :iterators (mapcar 'iterator iters)))

;; reverse (slow)

(defclass iterator-reverse-cache (iterator-iterator-cache) ())

(defclass iterator-reverse (iterator)
  ((cache
     :initarg :cache
     :initform nil
     :accessor iterator-cache)
     (index 
     :initform 0
     :accessor iterator-index)))

(defmethod load-from-iterator ((iter iterator-reverse))
  (load-from-iterator (iterator-cache iter)))

(defmethod did-load-from-iterator? ((iter iterator-reverse))
  (did-load-from-iterator (iterator-cache iter)))

(defmethod aref-cache ((iter iterator-reverse) index)
  (aref-cache (iterator-cache iter) index))

(defmethod next ((iter iterator-reverse))
  (unless (did-load-from-iterator? iter)
    (load-from-iterator iter))
  (multiple-value-bind
    (element found?)
    (aref-cache (iterator-index iter) iter)
    (if (null found?) *stop-iteration*
      (prog1 element 
        (incf (iterator-index iter))))))

(defmethod copy ((iter iterator-reverse))
  (let ((niter (make-instance 'iterator-reverse)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-reverse) (iter-from iterator-reverse))
  (setf (iterator-cache iter) (iterator-cache iter-from))
  (setf (iterator-index iter) (iterator-index iter-from)))

(defmethod ireverse (iter)
  (ireverse (iterator iter)))

(defmethod ireverse ((iter iterator))
  (make-instance 'iterator-reverse :cache
    (make-instance 'iterator-reverse-cache :iterator iter)))

;; sort (slow)

(defclass iterator-sort-cache (iterator-iterator-cache) ())

(defclass iterator-sort (iterator)
  ((function 
     :initarg :function
     :accessor iterator-function)
    (cache
      :initarg :cache 
      :initform nil
      :accessor iterator-sort)
    (index 
      :initform 0
      :accessor iterator-sort)))

(defmethod load-from-iterator ((iter iterator-sort))
  (load-from-iterator (iterator-cache iter))
  (sort (iterator-function iter)
    (iterator-collection (iterator-cache iter))))

(defmethod did-load-from-iterator? ((iter iterator-sort))
  (did-load-from-iterator? (iterator-cache iter)))

(defmethod aref-cache ((iter iterator-sort) index)
  (aref-cache (iterator-cache iter) index))

(defmethod next ((iter iterator-sort))
  (unless (did-load-from-iterator? iter)
    (load-from-iterator iter))
  (multiple-value-bind
    (element found?)
    (aref-cache iter (iterator-index iter))
    (if (null found?) *stop-iteration*
      (prog1 element (incf (iterator-index iter))))))

(defmethod copy ((iter iterator-sort))
  (let ((niter (make-instance 'iterator-sort)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-sort) (iter-from iterator-sort))
  (setf (iterator-function iter) (iterator-function iter-from))
  (setf (iterator-cache iter) (iterator-cache iter-from))
  (setf (iterator-index iter) (iterator-index iter)))

(defmethod isort (iter)
  (isort (iterator iter)))

(defmethod isort ((iter iterator))
  (make-instance 'iterator-sort :cache
    (make-instance 'iterator-sort-cache :iterator iter)))

;; cache 

(defclass iterator-cache (iterator-iterator)
  ((collection
     :initform (make-array 0 :fill-pointer 0 :adjustable t)
     :accessor iterator-collection)
    (index
      :initform 0
      :accessor iterator-index)))

(defmethod load-from-iterator ((iter iterator-cache))
  (let ((element (next (iterator-iterator iter))))
    (vector-push-extend element (iterator-collection iter))))

(defmethod next ((iter iterator-cache))
  (unless (< (iterator-index iter) (length (iterator-collection iter)))
    (load-from-iterator))
  (if (< (iterator-index iter) (length (iterator-collection iter)))
    (prog1 (aref (iterator-index iter) (iterator-collection iter))
      (incf (iterator-index iter)))
    *stop-iteration*))

(defmethod copy ((iter iterator-cache))
  (let ((niter (make-instance 'iterator-cache)))
    (prog1 niter (copy-from niter iter))))

(defmethod copy-from :after ((iter iterator-cache) (iter-from iterator-cache))
  (setf (iterator-collection iter) (iterator-collection iter-from))
  (setf (iterator-index iter) (iterator-index iter-from)))

(defmethod icache (iter)
  (icache (iterator iter)))

(defmethod icache ((iter iterator))
  (make-instance 'iterator-cache :iterator iter))
  
;; file 

(define-condition iterator-file-error (iterator-error)
  ()
  (:report
    (lambda (condition stream)
      (form stream "iterator-file-error: ~a~%"
        (iterator-error-message condition)))))

(defconstant *iterator-file-eof* (make-symbol "iterator-file-eof"))

(defclass iterator-file (iterator)
  ((file 
     :initarg :file
     :accessor iterator-file)
    (read-function
      :initarg :read-function
      :initform 'read-char
      :accessor iterator-read-function)))

(defmethod next ((iter iterator-file))
  (let ((element (funcall (iterator-read-function iter) (iterator-file iter) nil *iterator-file-eof*)))
    (if (eq element *iterator-file-eof*) *stop-iteration* element)))

(defmethod copy ((iter iterator-file))
  (error 
    (make-instance 'iterator-file-error 
      :message "iterator-file class cannot coping.")))

(defmethod copy-from :after ((iter iterator-file) (iter-from iterator-file))
  (error
    (make-instance 'iterator-file-error
      :message "iterator-file class cannot coping.")))

(defmethod ifile ((file stream) &key (read-function 'read-char))
  (make-instance 'iterator-file :file file :read-function read-function))

;; every 

(defmethod ievery (function iter)
  (ievery function (iterator iter)))

(defmethod ievery (function (iter iterator))
  (loop with element 
    if (eq (setq element (next (iterator-iterator iter))) *stop-iteration*) return t
    else unless (funcall function element) return nil))

;; some 

(defmethod isome (function iter)
  (isome function (iterator iter)))

(defmethod isome (function (iter iterator))
  (loop with element 
    if (eq (setq element (next (iterator-iterator iter))) *stop-iteration*) return nil
    else when (funcall function element) return t))

;; reduce

(defmethod ireduce-in (function result (iter iterator))
  (let ((element (next iter)))
    (if (eq element *stop-iteration*) result
      (ireduce-in function (funcall function result element) iter))))

(defmethod ireduce (function iter)
  (ireduce function (iterator iter)))

(defmethod ireduce (function (iter iterator))
  (let*
    ((element1 (next iter))
      (element2 (next iter)))
    (if (eq element1 *stop-iteration*) nil
      (if (eq element2 *stop-iteration*) element1
        (ireduce-in function element1 iter)))))

;; findif 

(defmethod ifind-if (function iter)
  (ifind-if function (iterator iter)))

(defmethod ifind-if (function (iter iterator))
  (loop with element
    until (eq (setq element (next iter)) *stop-iteration*)
    when (funcall function element)
    return element))

;; find

(defmethod ifind (item iter)
  (ifind item (iterator iter)))

(defmethod ifind (item (iter iterator))
  (ifind-if 
    (lambda (element)
      (equal element  item))
    iter))

;; position if 

(defmethod iposition-if (function iter)
  (iposition-if function (iterator iter)))

(defmethod iposition-if (function (iter iterator))
  (loop with element with index = 0
    until (eq (setq element (next iter)) *stop-iteration*)
    when (funcall function element)
    return index
    do (incf index)))

;; position 

(defmethod iposition (item iter)
  (iposition-if item iter))

(defmethod iposition (item (iter iterator))
  (iposition-if 
    (lambda (element)
      (equal element iter))
    iter))

;; icount if 

(defmethod icount-if (function iter)
  (icount-if function (iterator  iter)))

(defmethod icount-if (function (iter iterator))
  (loop with element
    until (eq (setq element (next iter)) *stop-iteration*)
    when (funcall function element)
    count t))

;; icount 

(defmethod icount (item iter)
  (icount item (iterator iter)))

(defmethod icount (item (iter iterator))
  (icount-if 
    (lambda (element)
      (equal element item))
    iter))

;; macro utilities

(defmacro doiterator (argument &body body)
  (let
    ((variable (nth 0 argument))
      (formula (nth 1 argument))
      (result (nth 2 argument))
      (iter (gensym)))
    `(loop with ,iter = (iterator ,formula) with ,variable do
       (setq ,variable (next ,iter))
       if (eq ,variable *stop-iteration*) return ,result 
       do (progn ,@body))))
