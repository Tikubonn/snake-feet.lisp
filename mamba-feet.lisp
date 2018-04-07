;; Copyright (c) 2018 tikubonn.
;; Released under the MIT license 
;; http://opensource.org/licenses/mitlicense.php 

;; package 

(provide 'mamba-feet)

(defpackage mamba-feet 
  (:use :common-lisp)
  (:shadow :next :skip :copy)
  (:export :next :skip :copy :iterator :ilist :iarray :ifunction 
    :range :repeat :imap :ifilter :iappend :izip :islice :istep
    :icache :ireverse :isort :ifile :doiterator :icount-if :icount 
    :iposition-if :iposition :ifind-if :ifind :ireduce :isome :ievery))

(in-package :mamba-feet)

;; define generic

(defgeneric iterator (iter))

;; iterator

(defstruct iterator 
  (next-function nil :type function)
  (skip-function nil :type function)
  (copy-function nil :type function))

(defconstant +stop-iteration+
  (make-symbol "+stop-iteration+"))

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

;; function 

(defstruct 
  (iterator-function 
    (:include iterator))
  (function))

(defmethod iterator ((iter iterator-function))
  (declare 
    (type iterator-function iter)
    (optimize (speed 3)))
  iter)

(defmethod iterator ((function function))
  (declare 
    (type function function)
    (optimize (speed 3)))
  (the iterator-function 
    (ifunction function)))

(defmethod iterator ((name symbol))
  (declare 
    (type symbol name)
    (optimize (speed 3)))
  (the iterator-function 
    (ifunction name)))

(defmacro ilambda (arguments &body body)
  `(ifunction (lambda ,arguments ,@body)))

(defun ifunction (function)
  (declare 
    (optimize (speed 3)))
  (the iterator-function
    (make-iterator-function
      :next-function #'next-iterator-function
      :skip-function #'skip-iterator-function
      :copy-function #'copy-iterator-function
      :function function)))

(defun next-iterator-function (iter)
  (declare 
    (type iterator-function iter)
    (optimize (speed 3)))
  (funcall (iterator-function-function iter)))

(defun skip-iterator-function (iter)
  (declare 
    (type iterator-function iter)
    (optimize (speed 3)))
  (the boolean
    (let ((element (funcall (iterator-function-function iter))))
      (if (eq element +stop-iteration+) nil t))))

(defun copy-iterator-function (iter)
  (error "iterator-function cannot copy, because I cannot copy inner functions status."))

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
  (if (null (iterator-list-node iter)) +stop-iteration+
    (prog1 (car (iterator-list-node iter))
      (setf (iterator-list-node iter)
        (cdr (iterator-list-node iter))))))

(defun skip-iterator-list (iter)
  (declare 
    (type iterator-list iter)
    (optimize (speed 3)))
  (if (null (iterator-list-node iter)) nil ;; when reached eoi
    (prog1 t 
      (setf (iterator-list-node iter)
        (cdr (iterator-list-node iter))))))

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
  (iterator-array
    (:include iterator))
  (array nil :type array)
  (index 0 :type integer))

(defun iarray (arr)
  (declare 
    (type array arr)
    (optimize (speed 3)))
  (the iterator-array 
    (make-iterator-array 
      :next-function #'next-iterator-array 
      :skip-function #'skip-iterator-array 
      :copy-function #'copy-iterator-array 
      :array arr)))

(defmethod iterator ((iter iterator-array))
  (declare 
    (type iterator-array iter)
    (optimize (speed 3)))
  iter)

(defmethod iterator ((arr array))
  (declare 
    (type array arr)
    (optimize (speed 3)))
  (the iterator-array (iarray arr)))

(defun next-iterator-array (iter)
  (declare 
    (type iterator-array iter)
    (optimize (speed 3)))
  (if (< (iterator-array-index iter) (length (iterator-array-array iter)))
    (prog1 (aref (iterator-array-array iter) (iterator-array-index iter))
      (incf (iterator-array-index iter)))
    +stop-iteration+))

(defun skip-iterator-array (iter)
  (declare 
    (type iterator-array iter)
    (optimize (speed  3)))
  (the boolean
    (if (< (iterator-array-index iter) (length (iterator-array-array iter)))
      (prog1 t (incf (iterator-array-index iter)))
      nil))) ;; when reached eoi

(defun copy-iterator-array (iter)
  (declare 
    (type iterator-array iter)
    (optimize (speed 3)))
  (the iterator-array 
    (make-iterator-array 
      :next-function #'next-iterator-array 
      :skip-function #'skip-iterator-array 
      :copy-function #'copy-iterator-array 
      :array (iterator-array-array iter)
      :index (iterator-array-index iter))))

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
  (if (iterator-range-end? iter) +stop-iteration+
    (prog1 (iterator-range-current iter)
      (incf (iterator-range-current iter)
        (iterator-range-step iter)))))

(defun skip-iterator-range (iter)
  (declare 
    (type iterator-range iter)
    (optimize (speed 3)))
  (the boolean
    (if (iterator-range-end? iter) nil ;; when reached eoi
      (prog1 t
        (incf (iterator-range-current iter)
          (iterator-range-step iter))))))

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

(compile 'iterator-range-end?)
(compile 'next-iterator-range)
(compile 'skip-iterator-range)
(compile 'copy-iterator-range)

;; repeat 

(defstruct 
  (iterator-repeat
    (:include iterator))
  (count 0 :type integer)
  (element))

(defmethod iterator ((iter iterator-repeat))
  (declare 
    (type iterator-repeat iter)
    (optimize (speed 3)))
  iter)

(defun repeat (count &optional element)
  (declare
    (type integer count)
    (optimize (speed 3)))
  (the iterator-repeat
    (make-iterator-repeat 
      :next-function #'next-iterator-repeat
      :skip-function #'skip-iterator-repeat
      :copy-function #'copy-iterator-repeat
      :count count 
      :element element)))

(defun next-iterator-repeat (iter)
  (declare 
    (type iterator-repeat iter)
    (optimize (speed 3)))
  (if (zerop (iterator-repeat-count iter)) +stop-iteration+
    (prog1 (iterator-repeat-element iter)
      (decf (iterator-repeat-count iter)))))

(defun skip-iterator-repeat (iter)
  (declare 
    (type iterator-repeat iter)
    (optimize (speed 3)))
  (the boolean
    (if (zerop (iterator-repeat-count iter)) nil ;; when reached eoi
      (prog1 t (decf (iterator-repeat-count iter))))))

(defun copy-iterator-repeat (iter)
  (declare 
    (type iterator-repeat iter)
    (optimize (speed 3)))
  (the iterator-repeat
    (make-iterator-repeat
      :next-function #'next-iterator-repeat
      :skip-function #'skip-iterator-repeat
      :copy-function #'copy-iterator-repeat
      :count (iterator-repeat-count iter)
      :element (iterator-repeat-element iter))))

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

(defun next-iterator-map (iter)
  (declare 
    (type iterator-map iter)
    (optimize (speed 3)))
  (let ((element (next (iterator-map-iterator iter))))
    (if (eq element +stop-iteration+) +stop-iteration+
      (funcall (iterator-map-function iter) element))))

(defun skip-iterator-map (iter)
  (declare 
    (type iterator-map iter)
    (optimize (speed 3)))
  (the boolean (skip (iterator-map-iterator iter))))

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

;; filter 

(defstruct
  (iterator-filter
    (:include iterator))
  (function nil :type function)
  (iterator))

(defmethod iterator ((iter iterator-filter))
  (declare
    (type iterator-filter iter)
    (optimize (speed 3)))
  iter)

(defun ifilter (function iter)
  (declare 
    (type function function)
    (optimize (speed 3)))
  (the iterator-filter
    (make-iterator-filter
      :next-function #'next-iterator-filter
      :skip-function #'skip-iterator-filter
      :copy-function #'copy-iterator-filter
      :function function
      :iterator (iterator iter))))

(defun next-iterator-filter (iter)
  (declare
    (type iterator-filter iter)
    (optimize (speed 3)))
  (let ((element (next (iterator-filter-iterator iter))))
    (if (eq element +stop-iteration+) +stop-iteration+
      (if (funcall (iterator-filter-function iter) element) element
        (next-iterator-filter iter)))))

(defun skip-iterator-filter (iter)
  (declare 
    (type iterator-filter iter)
    (optimize (speed 3)))
  (let ((element (next (iterator-filter-iterator iter))))
    (if (eq element +stop-iteration+) nil ;; when reached eoi
      (if (funcall (iterator-filter-function iter) element) t
        (skip-iterator-filter iter)))))

(defun copy-iterator-filter (iter)
  (declare 
    (type iterator-filter iter)
    (optimize (speed 3)))
  (the iterator-filter
    (make-iterator-filter
      :next-function #'next-iterator-filter
      :skip-function #'skip-iterator-filter
      :copy-function #'copy-iterator-filter
      :function (iterator-filter-function iter)
      :iterator (iterator-filter-iterator iter))))

(compile 'skip-iterator-filter)
(compile 'next-iterator-filter)
(compile 'copy-iterator-filter)

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
    (if (eq element +stop-iteration+) +stop-iteration+
      (prog1 element 
        (incf (iterator-slice-current iter))))))

(defun skip-iterator-slice (iter)
  (declare
    (type iterator-slice iter)
    (optimize (speed 3)))
  (setup-iterator-slice iter)
  (the boolean
    (if (skip (iterator-slice-iterator iter))
      (prog1 t (incf (iterator-slice-current iter))) 
      nil))) ;; when reached eoi

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

;; step 

(defstruct 
  (iterator-step 
    (:include iterator))
  (current 0 :type integer)
  (offset 0 :type integer)
  (step 1 :type integer)
  (iterator))

(defmethod iterator ((iter iterator-step))
  (declare
    (type iterator-step iter)
    (optimize (speed 3)))
  iter)

(defun istep (offset step iter)
  (declare 
    (type integer offset step)
    (optimize (speed 3)))
  (the iterator-step
    (make-iterator-step
      :next-function #'next-iterator-step
      :skip-function #'skip-iterator-step
      :copy-function #'copy-iterator-step
      :current 0 
      :offset offset
      :step step
      :iterator iter)))

(defun setup-iterator-step (iter)
  (declare 
    (type iterator-step iter)
    (optimize (speed 3)))
  (loop while (< (iterator-step-current iter) (iterator-step-offset iter)) do 
    (skip (iterator-step-iterator iter))
    (incf (iterator-step-current iter))))

(defun next-iterator-step (iter)
  (declare 
    (type iterator-step iter)
    (optimize (speed 3)))
  (setup-iterator-step iter)
  (let ((element (next (iterator-step-iterator iter))))
    (if (eq element +stop-iteration+) +stop-iteration+
      (prog1 element
        (loop repeat (iterator-step-step iter) do 
          (skip (iterator-step-iterator iter))
          (incf (iterator-step-current iter)))))))

(defun skip-iterator-step-in (count iter)
  (declare 
    (type iterator-step iter)
    (type integer count)
    (optimize (speed 3)))
  (the boolean
    (if (= 0 count) t
      (if (skip (iterator-step-iterator iter))
        (progn
          (incf (iterator-step-current iter))
          (skip-iterator-step-in (1- count) iter))
        nil)))) ;; when reached eoi

(defun skip-iterator-step (iter)
  (declare 
    (type iterator-step iter)
    (optimize (speed 3)))
  (setup-iterator-step iter) 
  (the boolean 
    (skip-iterator-step-in 
      (iterator-step-step iter) iter)))

(defun copy-iterator-step (iter)
  (declare 
    (type iterator-step iter)
    (optimize (speed 3)))
  (the iterator-step
    (make-iterator-step
      :next-function #'next-iterator-step
      :skip-function #'skip-iterator-step
      :copy-function #'copy-iterator-step
      :current (iterator-step-current iter)
      :offset (iterator-step-offset iter)
      :step (iterator-step-step iter)
      :iterator (copy (iterator-step-iterator iter)))))

(compile 'setup-iterator-step)
(compile 'next-iterator-step)
(compile 'skip-iterator-step)
(compile 'copy-iterator-step)

;; append 

(defstruct 
  (iterator-append
    (:include iterator))
  (iterators))

(defmethod iterator ((iter iterator-append))
  (declare 
    (type iterator-append iter)
    (optimize (speed 3)))
  iter)

(defun iappend (&rest iters)
  (declare 
    (optimize (speed 3)))
  (the iterator-append
    (make-iterator-append 
      :next-function #'next-iterator-append
      :skip-function #'skip-iterator-append
      :copy-function #'copy-iterator-append
      :iterators (mapcar 'iterator iters))))

(defun next-iterator-append (iter)
  (declare 
    (type iterator-append iter)
    (optimize (speed 3)))
  (if (null (iterator-append-iterators iter)) +stop-iteration+
    (let ((element (next (car (iterator-append-iterators iter)))))
      (cond 
        ((eq element +stop-iteration+)
          (setf (iterator-append-iterators iter)
            (cdr (iterator-append-iterators iter))) 
          (next-iterator-append iter))
        (t element)))))

(defun skip-iterator-append (iter)
  (declare 
    (type iterator-append iter)
    (optimize (speed 3)))
  (the boolean
    (if (null (iterator-append-iterators iter)) nil ;; when reached eoi
      (if (skip (car (iterator-append-iterators iter))) t
        (progn
          (setf (iterator-append-iterators iter)
            (cdr (iterator-append-iterators iter)))
          (skip-iterator-append iter))))))

(defun copy-iterator-append (iter)
  (declare 
    (type iterator-append iter)
    (optimize (speed 3)))
  (the iterator-append
    (make-iterator-append
      :next-function #'next-iterator-append
      :skip-function #'skip-iterator-append
      :copy-function #'copy-iterator-append
      :iterators (mapcar 'copy (iterator-append-iterators iter)))))

(compile 'next-iterator-append)
(compile 'skip-iterator-append)
(compile 'copy-iterator-append)

;; zip 

(defstruct 
  (iterator-zip 
    (:include iterator))
  (iterators))

(defmethod iterator ((iter iterator-zip))
  (declare 
    (type iterator-zip iter)
    (optimize (speed 3)))
  iter)

(defun izip (&rest iters)
  (declare 
    (optimize (speed 3)))
  (the iterator-zip
    (make-iterator-zip
      :next-function #'next-iterator-zip
      :skip-function #'skip-iterator-zip
      :copy-function #'copy-iterator-zip
      :iterators (mapcar 'iterator iters))))

(defun next-iterator-zip (iter)
  (loop with element
    for citer in (iterator-zip-iterators iter) do 
    (setq element (next citer)) 
    if (eq element +stop-iteration+) return +stop-iteration+
    collect element))

(defun skip-iterator-zip (iter)
  (let ((status t))
    (loop for citer in (iterator-zip-iterators iter)
      unless (skip citer) do
      (setq status nil)) ;; when reached eoi
    status))

(defun copy-iterator-zip (iter)
  (declare 
    (type iterator-zip iter)
    (optimize (speed 3)))
  (the iterator-zip
    (make-iterator-zip 
      :next-function #'next-iterator-zip
      :skip-function #'skip-iterator-zip
      :copy-function #'copy-iterator-zip
      :iterators (mapcar 'copy (iterator-zip-iterators iter)))))

(compile 'next-iterator-zip)
(compile 'skip-iterator-zip)
(compile 'copy-iterator-zip)

;; cache 

(defstruct 
  (iterator-cache 
    (:include iterator))
  (index 0 :type integer)
  (collection nil :type array)
  (iterator))

(defmethod iterator ((iter iterator-cache))
  (declare 
    (type iterator-cache iter)
    (optimize (speed 3)))
  iter)

(defun icache (iter)
  (declare
    (type iterator-cache iter)
    (optimize (speed  3)))
  (the iterator-cache 
    (make-iterator-cache 
      :next-function #'next-iterator-cache
      :skip-function #'skip-iterator-cache
      :copy-function #'copy-iterator-cache
      :collection (make-array 128 :fill-pointer 0 :adjustable t)
      :iterator iter)))

(defun next-iterator-cache (iter)
  (declare 
    (type iterator-cache iter)
    (optimize (speed 3)))
  (if (< (iterator-cache-index iter) (length (iterator-cache-collection iter)))
    (prog1 (aref (iterator-cache-collection iter) (iterator-cache-index iter))
      (incf (iterator-cache-index iter)))
    (let ((element (next (iterator-cache-iterator iter))))
      (if (eq element +stop-iteration+) +stop-iteration+
        (prog1 element 
          (vector-push-extend element 
            (iterator-cache-collection iter)))))))

(defun skip-iterator-cache (iter)
  (declare 
    (type iterator-cache iter)
    (optimize (speed 3)))
  (the boolean
    (if (< (iterator-cache-index iter) (length (iterator-cache-collection iter)))
      (prog1 t 
        (incf (iterator-cache-index iter)))
      (if (skip (iterator-cache-iterator iter))
        (prog1 t
          (incf (iterator-cache-index iter)))
        nil)))) ;; when reached eoi

(defun copy-iterator-cache (iter)
  (declare 
    (type iterator-cache iter)
    (optimize (speed 3)))
  (the iterator-cache 
    (make-iterator-cache 
      :next-function #'next-iterator-cache
      :skip-function #'skip-iterator-cache
      :copy-function #'copy-iterator-cache
      :index (iterator-cache-index iter)
      :collection (iterator-cache-collection iter)
      :iterator (iterator-cache-iterator iter))))

(compile 'next-iterator-cache)
(compile 'skip-iterator-cache)
(compile 'copy-iterator-cache)

;; reverse 

(defstruct 
  (iterator-reverse 
    (:include iterator))
  (collection nil :type array)
  (index 0 :type integer)
  (iterator))

(defmethod iterator ((iter iterator-reverse))
  (declare 
    (type iterator-reverse iter)
    (optimize (speed 3)))
  iter)

(defun ireverse (iter)
  (declare 
    (optimize (speed 3)))
  (make-iterator-reverse 
    :next-function #'next-iterator-reverse 
    :skip-function #'skip-iterator-reverse 
    :copy-function #'copy-iterator-reverse 
    :collection (make-array 128 :fill-pointer 0 :adjustable t)
    :iterator (iterator iter)))

(defun init-iterator-reverse (iter)
  (declare 
    (type iterator-reverse iter)
    (optimize (speed 3)))
  (let ((element (next (iterator-reverse-iterator iter))))
    (unless (eq element +stop-iteration+)
      (vector-push-extend element 
        (iterator-reverse-collection iter))
      (init-iterator-reverse iter))))

(defun next-iterator-reverse (iter)
  (declare 
    (type iterator-reverse iter)
    (optimize (speed 3)))
  (if (= 0 (length (iterator-reverse-collection iter)))
    (init-iterator-reverse iter))
  (if (< (iterator-reverse-index iter) (length (iterator-reverse-collection iter)))
    (prog1 
      (aref (iterator-reverse-collection iter)
        (- (length (iterator-reverse-collection iter)) 1
          (iterator-reverse-index iter)))
      (incf (iterator-reverse-index iter)))
    +stop-iteration+))

(defun skip-iterator-reverse (iter)
  (declare 
    (type iterator-reverse iter)
    (optimize (speed 3)))
  (if (= 0 (length (iterator-reverse-collection iter)))
    (init-iterator-reverse iter))
  (the boolean
    (if (< (iterator-reverse-index iter) (length (iterator-reverse-collection iter)))
      (prog1 t (incf (iterator-reverse-index iter)))
      nil))) ;; when reached eoi 

(defun copy-iterator-reverse (iter)
  (declare 
    (type iterator-reverse iter)
    (optimize (speed 3)))
  (the iterator-reverse
    (make-iterator-reverse 
      :next-function #'next-iterator-reverse 
      :skip-function #'skip-iterator-reverse 
      :copy-function #'copy-iterator-reverse 
      :collection (iterator-reverse-collection iter)
      :index (iterator-reverse-index iter)
      :iterator (iterator-reverse-iterator iter))))

(compile 'init-iterator-reverse)
(compile 'next-iterator-reverse)
(compile 'skip-iterator-reverse)
(compile 'copy-iterator-reverse)

;; sort 

(defstruct 
  (iterator-sort 
    (:include iterator))
  (collection nil :type array)
  (index 0 :type integer)
  (function)
  (iterator))

(defmethod iterator ((iter iterator-sort))
  (declare 
    (type iterator-sort iter)
    (optimize (speed 3)))
  iter)

(defun isort (function iter)
  (declare 
    (type iterator-sort iter)
    (optimize (speed 3)))
  (the iterator-sort
    (make-iterator-sort 
      :next-function #'next-iterator-sort
      :skip-function #'skip-iterator-sort
      :copy-function #'copy-iterator-sort
      :collection (make-array 128 :fill-pointer 0 :adjustable t)
      :function function 
      :iterator (iterator iter))))

(defun init-iterator-sort-collect (iter)
  (declare 
    (type iterator-sort iter)
    (optimize (speed 3)))
  (let ((element (next (iterator-sort-iterator iter))))
    (unless (eq element +stop-iteration+)
      (vector-push-extend element 
        (iterator-sort-collection iter))
      (init-iterator-sort-collect iter))))

(defun init-iterator-sort (iter)
  (declare 
    (type iterator iter)
    (optimize (speed 3)))
  (init-iterator-sort-collect iter)
  (sort (iterator-sort-function iter)
    (iterator-sort-collection)))

(defun next-iterator-sort (iter)
  (declare 
    (type iterator iter)
    (optimize (speed 3)))
  (if (= 0 (length (iterator-sort-collection iter)))
    (init-iterator-sort iter))
  (if (< (iterator-sort-index iter) (length (iterator-sort-collection iter)))
    (prog1
      (aref (iterator-sort-collection iter) (iterator-sort-index iter))
      (incf (iterator-sort-index iter)))
    +stop-iteration+))

(defun skip-iterator-sort (iter)
  (declare 
    (type iterator iter)
    (optimize (speed 3)))
  (if (= 0 (length (iterator-sort-collection iter)))
    (init-iterator-sort iter))
  (the boolean
    (if (< (iterator-sort-index iter) (length (iterator-sort-collection iter)))
      (prog1 t
        (incf (iterator-sort-index iter)))
      nil)))

(defun copy-iterator-sort (iter)
  (declare
    (type iterator-sort iter)
    (optimize (speed 3)))
  (the iterator-sort
    (make-iterator-sort 
      :next-function #'next-iterator-sort
      :skip-function #'skip-iterator-sort
      :copy-function #'copy-iterator-sort
      :collection (iterator-sort-collection iter)
      :function (iterator-sort-function iter)
      :iterator (iterator-sort-iterator iter))))

(compile 'init-iterator-sort-collect)
(compile 'init-iterator-sort)
(compile 'next-iterator-sort)
(compile 'skip-iterator-sort)
(compile 'copy-iterator-sort)
 
;; file 

(defstruct 
  (iterator-file
    (:include iterator))
  (file)
  (read-function))

(defmethod iterator ((iter iterator-file))
  (declare 
    (type iterator-file iter)
    (optimize (speed 3)))
  iter)

(defun ifile (file &optional (function 'read-char))
  (declare
    (type stream file)
    (optimize (speed 3)))
  (the iterator-file 
    (make-iterator-file 
      :next-function #'next-iterator-file
      :skip-function #'skip-iterator-file 
      :copy-function #'copy-iterator-file 
      :read-function function
      :file file)))

(defconstant +iterator-file-eof+
  (make-symbol "+iterator-file-eof+"))

(defun next-iterator-file (iter)
  (declare 
    (type iterator-file iter)
    (optimize (speed 3)))
  (let ((char (funcall (iterator-file-read-function iter) nil +iterator-file-eof+)))
    (if (eq char +iterator-file-eof+) +stop-iteration+
      char)))

(defun skip-iterator-file (iter)
  (declare
    (type iterator-file iter)
    (optimize (speed 3)))
  (the boolean
    (let ((char (funcall (iterator-file-read-function iter) nil +iterator-file-eof+)))
      (if (eq char +iterator-file-eof+) nil t))))

(defun copy-iterator-file (iter)
  (error "cannot copy an iterator-file, because it cannot copy a file status."))

(compile 'next-iterator-file)
(compile 'skip-iterator-file)
(compile 'copy-iterator-file)

;; macro 

(defmacro doiterator (argument &body body)
  (let
    ((variable (nth 0 argument))
      (formula (nth 1 argument))
      (result (nth 2 argument))
      (iter (gensym)))
    `(loop with ,iter = (iterator ,formula) with ,variable do
       (setq ,variable (next ,iter))
       if (eq ,variable +stop-iteration+) return ,result 
       do (progn ,@body))))

;; icount 

(defun icount-if-in (function count iter)
  (declare 
    (type integer count)
    (optimize (speed 3)))
  (the integer
    (let ((element (next iter)))
      (if (eq element +stop-iteration+) count 
        (if (funcall function element) 
          (the integer (icount-if-in function (1+ count) iter))
          (the integer (icount-if-in function count iter)))))))

(defun icount-if (function iter)
  (declare 
    (optimize (speed 3)))
  (the integer
    (icount-if-in function 0 iter)))

(defun icount (item iter)
  (declare 
    (optimize (speed 3)))
  (the integer
    (icount-if 
      (lambda (element)
        (eql item element))
      iter)))

(compile 'icount-if-in)
(compile 'icount-if)
(compile 'icount)

;; find 

(defun ifind-if (function iter)
  (declare 
    (optimize (speed  3)))
  (let ((element (next iter)))
    (if (eq element +stop-iteration+) (values nil nil)
      (if (funcall function element) (values element t)
        (ifind-if function iter)))))

(defun ifind (item iter)
  (declare 
    (optimize (speed 3)))
  (ifind-if 
    (lambda (element)
      (eql item element))
    iter))

(compile 'ifind-if)
(compile 'ifind)

;; position 

(defun iposition-if-in (function count iter)
  (declare 
    (type integer count)
    (optimize (speed 3)))
  (let ((element (next iter)))
    (if (eq element +stop-iteration+) nil ;; when not found
      (if (funcall function element) count ;; when found 
        (iposition-if-in function (1+ count) iter)))))

(defun iposition-if (function iter)
  (declare 
    (optimize (speed 3)))
  (iposition-if-in function 0 iter))

(defun iposition (item iter)
  (declare 
    (optimize (speed 3)))
  (iposition-if 
    (lambda (element)
      (eql item element))
    iter))

(compile 'iposition-if-in)
(compile 'iposition-if)
(compile 'iposition)

;; reduce 

(defun ireduce-in (function result iter)
  (declare 
    (optimize (speed 3)))
  (let ((element (next iter)))
    (if (eq element +stop-iteration+) result 
      (ireduce-in function 
        (funcall function result element)
        iter))))

(defun ireduce (function iter)
  (declare 
    (optimize (speed 3)))
  (let*
    ((element1 (next iter))
      (element2 (next iter)))
    (if (eq element1 +stop-iteration+) nil
      (if (eq element2 +stop-iteration+) element2
        (ireduce-in function 
          (funcall function element1 element2)
          iter)))))

(compile 'ireduce-in)
(compile 'ireduce)

;; ievery 

(defun ievery (function iter)
  (declare 
    (optimize (speed 3)))
  (the boolean
    (let ((element (next iter)))
      (if (eq element +stop-iteration+) t
        (if (funcall function element)
          (the boolean
            (ievery function iter))
          nil)))))

(compile 'ievery)

;; isome 

(defun isome (function iter)
  (declare 
    (optimize (speed 3)))
  (the boolean 
    (let ((element (next iter)))
      (if (eq element +stop-iteration+) nil
        (if (funcall function element) t
          (the boolean 
            (isome function iter)))))))

(compile 'isome)
