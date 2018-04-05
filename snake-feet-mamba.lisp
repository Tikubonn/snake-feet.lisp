
(provide 'snake-feet-more-optimized)

(defpackage snake-feet-more-optimized 
  (:use :common-lisp)
  (:shadow :next :skip :copy)
  (:export :next :skip :copy :iterator :range :imap :islice :doiterator))

(in-package :snake-feet-more-optimized)

;; define generic

(defgeneric iterator (iter))

;; iterator

(defstruct iterator 
  (next-function nil :type function)
  (skip-function nil :type function)
  (copy-function nil :type function))

(defconstant *stop-iteration*
  (make-symbol "*stop-iteration*"))

(defun next (iter)
  (declare 
    (optimize (speed 3)))
  (funcall (the function (iterator-next-function iter)) iter))

(defun skip (iter)
  (declare 
    (optimize (speed 3)))
  (funcall (the function (iterator-skip-function iter)) iter))

(defun copy (iter)
  (declare 
    (optimize (speed 3)))
  (funcall (the function (iterator-copy-function iter)) iter))

(defmethod iterator ((iter iterator))
  (declare 
    (type iterator iter)
    (optimize (speed 3)))
  iter)

;; list 

(defstruct 
  (iterator-list 
    (:include iterator))
  (node))

(defmethod iterator ((iter iterator-list))
  (declare 
    (type iterator-list iter)
    (optimize (speed 3)))
  iter)

(defmethod iterator ((lis list))
  (declare 
    (type list lis)
    (optimize (speed 3)))
  (the iterator-list (ilist lis)))

(defun ilist (lis)
  (declare 
    (type list lis)
    (optimize (speed 3)))
  (the iterator-list 
    (make-iterator-list 
      :next-function #'next-iterator-list 
      :skip-function #'skip-iterator-list 
      :copy-function #'copy-iterator-list
      :node lis)))

(defun next-iterator-list (iter)
  (declare 
    (type iterator-list iter)
    (optimize (speed 3)))
  (if (null (iterator-list-node iter)) *stop-iteration*
    (prog1 (car (iterator-list-node iter))
      (setf (iterator-list-node iter)
        (cdr (iterator-list-node iter))))))

(defun skip-iterator-list (iter)
  (declare 
    (type iterator-list iter)
    (optimize (speed 3)))
  (prog1 nil
    (setf (iterator-list-node iter)
      (cdr (iterator-list-node iter)))))

(defun copy-iterator-list (iter)
  (declare 
    (type iterator-list iter)
    (optimize (speed 3)))
  (make-iterator-list 
    :next-function #'next-iterator-list
    :skip-function #'skip-iterator-list 
    :copy-function #'copy-iterator-list 
    :node (iterator-list-node iter)))

(compile 'next-iterator-list)
(compile 'skip-iterator-list)
(compile 'copy-iterator-list)

;; vector 

(defstruct 
  (iterator-vector 
    (:include iterator))
  (vector nil :type vector)
  (index 0 :type integer))

(defun next-iterator-vector (iter)
  (declare 
    (type iterator-vector iter)
    (optimize (speed 3)))
  (if (< (iterator-vector-index iter) (length (iterator-vector-vector iter)))
    (prog1 (aref (iterator-vector-vector iter) (iterator-vector-index iter))
      (incf (iterator-vector-index iter)))
    *stop-iteration*))

(defun skip-iterator-vector (iter)
  (declare 
    (type iterator-vector iter)
    (optimize (speed  3)))
  (the nil 
    (prog1 nil
      (when (< (iterator-vector-index iter) (length (iterator-vector-vector iter)))
        (incf (iterator-vector-index iter))))))

(defun copy-iterator-vector (iter)
  (declare 
    (type iterator-vector iter)
    (optimize (speed 3)))
  (the iterator-vector 
    (make-iterator-vector 
      :next-function #'next-iterator-vector 
      :skip-function #'skip-iterator-vector 
      :copy-function #'copy-iterator-vector 
      :vector (iterator-vector-vector iter)
      :index (iterator-vector-index iter))))

(defun ivector (vec)
  (declare 
    (type vector vec)
    (optimize (speed 3)))
  (the iterator-vector 
    (make-iterator-vector 
      :next-function #'next-iterator-vector 
      :skip-function #'skip-iterator-vector 
      :copy-function #'copy-iterator-vector 
      :vector vec)))

(defmethod iterator ((iter iterator-vector))
  (declare 
    (type iterator-vector iter)
    (optimize (speed 3)))
  iter)

(defmethod iterator ((vec vector))
  (declare 
    (type vector vec)
    (optimize (speed 3)))
  (the iterator-vector (ivector vec)))

;; range 

(defstruct 
  (iterator-range
    (:include iterator))
  (current 0 :type number)
  (start 0 :type number)
  (end 0 :type number)
  (step 1 :type number))

(defmethod iterator ((iter iterator-range))
  (declare 
    (type iterator-range iter)
    (optimize (speed 3)))
  iter)

(defun range (start &optional end step)
  (declare 
    (type number start)
    (optimize (speed 3)))
  (the iterator-range
    (cond 
      (step (make-iterator-range :next-function #'next-iterator-range :skip-function #'skip-iterator-range :copy-function #'copy-iterator-range :current start :start start :end end :step step))
      (end (make-iterator-range :next-function #'next-iterator-range :skip-function #'skip-iterator-range :copy-function #'copy-iterator-range :current 0 :start 0 :end end :step (if (< start end) 1 -1)))
      (t (make-iterator-range :next-function #'next-iterator-range :skip-function #'skip-iterator-range :copy-function #'copy-iterator-range :current 0 :start 0 :end start :step (if (< 0 start) 1 -1))))))

(defun iterator-range-end? (iter)
  (declare 
    (type iterator-range iter)
    (optimize (speed 3)))
  (the boolean
    (cond 
      ((< (iterator-range-step iter) 0)
        (<  (iterator-range-current iter) 
          (iterator-range-end iter)))
      ((>  (iterator-range-step iter) 0) 
        (>  (iterator-range-current iter) 
          (iterator-range-end iter))))))

(defun next-iterator-range (iter)
  (declare 
    (type iterator-range iter)
    (optimize (speed 3)))
  (if (iterator-range-end? iter) *stop-iteration*
    (prog1 (iterator-range-current iter)
      (incf (iterator-range-current iter)
        (iterator-range-step iter)))))

(defun skip-iterator-range (iter)
  (declare 
    (type iterator-range iter)
    (optimize (speed 3)))
  (if (iterator-range-end? iter) *stop-iteration*
    (prog1 nil
      (incf (iterator-range-current iter)
        (iterator-range-step iter)))))

(defun copy-iterator-range (iter)
  (declare 
    (type iterator-range iter)
    (optimize (speed 3)))
  (the iterator-range
    (make-iterator-range 
      :next-function #'next-iterator-range
      :skip-function #'skip-iterator-range
      :copy-function #'copy-iterator-range
      :current (iterator-range-current iter)
      :start (iterator-range-start iter)
      :end (iterator-range-end iter)
      :step (iterator-range-step iter))))

;; map 

(defstruct 
  (iterator-map 
    (:include iterator))
  (function nil :type function)
  (iterator))

(defmethod iterator ((iter iterator-map))
  (declare 
    (type iterator-map iter)
    (optimize (speed 3)))
  iter)

(defun imap (function iter)
  (declare 
    (type function function)
    (optimize (speed 3)))
  (the iterator-map
    (make-iterator-map 
      :next-function #'next-iterator-map
      :skip-function #'skip-iterator-map
      :copy-function #'copy-iterator-map
      :function function
      :iterator (iterator iter))))

;; (defun imap (function iter)
;;   (declare 
;;     (type function function)
;;     (optimize (speed 3)))
;;   (the iterator-map
;;     (make-iterator-map 
;;       :next-function #'next-iterator-map
;;       :skip-function #'skip-iterator-map
;;       :copy-function #'copy-iterator-map
;;       :function function
;;       :iterator iter)))

(defun next-iterator-map (iter)
  (declare 
    (type iterator-map iter)
    (optimize (speed 3)))
  (let ((element (next (iterator-map-iterator iter))))
    (if (eq element *stop-iteration*) *stop-iteration*
      (funcall (iterator-map-function iter) element))))

(defun skip-iterator-map (iter)
  (declare 
    (type iterator-map iter)
    (optimize (speed 3)))
  (skip (iterator-map-iterator iter)))

(defun copy-iterator-map (iter)
  (declare 
    (type iterator-map iter)
    (optimize (speed 3)))
  (the iterator-map
    (make-iterator-map 
      :next-function #'next-iterator-map
      :skip-function #'skip-iterator-map
      :copy-function #'copy-iterator-map
      :function (iterator-map-function iter)
      :iterator (funcall 
                  (iterator-copy-function (iterator-map-iterator iter)) 
                  (iterator-map-iterator iter)))))

(compile 'next-iterator-map)
(compile 'skip-iterator-map)
(compile 'copy-iterator-map)

;; slice 

(defstruct 
  (iterator-slice
    (:include iterator))
  (current 0 :type integer)
  (start 0 :type integer)
  (end 0 :type integer)
  (iterator))

(defmethod iterator ((iter iterator-slice))
  (declare 
    (type iterator-slice iter)
    (optimize (speed 3)))
  iter)

(defun islice (start end iter)
  (declare
    (type integer start end)
    (optimize (speed 3)))
  (the iterator-slice
    (make-iterator-slice
      :next-function #'next-iterator-slice
      :skip-function #'skip-iterator-slice
      :copy-function #'copy-iterator-slice
      :current 0
      :start start
      :end end 
      :iterator (iterator iter))))

;; (defun islice (start end iter)
;;   (declare
;;     (type integer start end)
;;     (optimize (speed 3)))
;;   (the iterator-slice
;;     (make-iterator-slice
;;       :next-function #'next-iterator-slice
;;       :skip-function #'skip-iterator-slice
;;       :copy-function #'copy-iterator-slice
;;       :current 0
;;       :start start
;;       :end end 
;;       :iterator iter)))

(defun setup-iterator-slice (iter)
  (declare
    (type iterator-slice iter)
    (optimize (speed 3)))
  (loop while (< (iterator-slice-current iter) (iterator-slice-start iter))
    do (incf (iterator-slice-current iter))))

(defun next-iterator-slice  (iter)
  (declare
    (type iterator-slice iter)
    (optimize (speed 3)))
  (setup-iterator-slice iter)
  (let ((element (next (iterator-slice-iterator iter))))
    (if (eq element *stop-iteration*) *stop-iteration*
      (prog1 element 
        (incf (iterator-slice-current iter))))))

(defun skip-iterator-slice (iter)
  (declare
    (type iterator-slice iter)
    (optimize (speed 3)))
  (setup-iterator-slice iter)
  (prog1 nil
    (skip (iterator-slice-iterator iter))
    (incf (iterator-slice-current iter))))

(defun copy-iterator-slice (iter)
  (declare
    (type iterator-slice iter)
    (optimize (speed 3)))
  (the iterator-slice
    (make-iterator-slice
      :next-function #'next-iterator-slice
      :skip-function #'skip-iterator-slice
      :copy-function #'copy-iterator-slice
      :current (iterator-slice-current iter)
      :start (iterator-slice-start iter)
      :end (iterator-slice-end iter)
      :iterator (copy (iterator-slice-iterator iter)))))

(compile 'setup-iterator-slice)
(compile 'next-iterator-slice)
(compile 'skip-iterator-slice)
(compile 'copy-iterator-slice)

;; macro 

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