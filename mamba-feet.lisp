;; Copyright (c) 2018 tikubonn.
;; Released under the MIT license 
;; http://opensource.org/licenses/mitlicense.php 

;; package 

(provide 'mamba-feet)

(defpackage mamba-feet 
  (:use :common-lisp)
  (:shadow :inext :iskip :icopy)
  (:export :inext :iskip :icopy :iterator :ilist :iarray :ifunction :ilambda :*stop-iteration*
    :range :repeat :imap :ifilter :iappend :izip :islice :istep
    :icache :ireverse :isort :ifile :doiterator :icount-if :icount 
    :iposition-if :iposition :ifind-if :ifind :ireduce :isome :ievery))

(in-package :mamba-feet)

;; define generic

(defgeneric iterator (iter))

;; iterator

(defstruct iterator 
  (inext-function nil :type function)
  (iskip-function nil :type function)
  (icopy-function nil :type function))

(defvar *stop-iteration*
  (make-symbol "*stop-iteration*"))

(defun inext (iter)
  (funcall (iterator-inext-function iter) iter))

(defun iskip (iter)
  (funcall (iterator-iskip-function iter) iter))

(defun icopy (iter)
  (funcall (iterator-icopy-function iter) iter))

(defmethod iterator ((iter iterator))
  iter)

;; function 

(defstruct 
  (iterator-function 
    (:include iterator))
  (function))

(defun inext-iterator-function (iter)
  (funcall (iterator-function-function iter)))

(defun iskip-iterator-function (iter)
  (let ((element (funcall (iterator-function-function iter))))
    (if (eq element *stop-iteration*) nil t)))

(defun icopy-iterator-function (iter)
  (declare
    (ignore iter))
  (error "iterator-function cannot icopy, because I cannot icopy inner functions status."))

(defmacro ilambda (arguments &body body)
  `(ifunction (lambda ,arguments ,@body)))

(defun ifunction (function)
  (make-iterator-function
    :inext-function #'inext-iterator-function
    :iskip-function #'iskip-iterator-function
    :icopy-function #'icopy-iterator-function
    :function function))

(defmethod iterator ((iter iterator-function))
  iter)

(defmethod iterator ((function function))  
  (ifunction function))

(defmethod iterator ((name symbol))
  (ifunction name))

;; list 

(defstruct 
  (iterator-list 
    (:include iterator))
  (node))

(defun inext-iterator-list (iter)
  (if (null (iterator-list-node iter)) *stop-iteration*
    (prog1 (car (iterator-list-node iter))
      (setf (iterator-list-node iter)
        (cdr (iterator-list-node iter))))))

(defun iskip-iterator-list (iter)
  (if (null (iterator-list-node iter)) nil ;; when reached eoi
    (prog1 t 
      (setf (iterator-list-node iter)
        (cdr (iterator-list-node iter))))))

(defun icopy-iterator-list (iter)
  (make-iterator-list 
    :inext-function (iterator-inext-function iter)
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :node (iterator-list-node iter)))

(defun ilist (lis)
  (make-iterator-list 
    :inext-function #'inext-iterator-list 
    :iskip-function #'iskip-iterator-list 
    :icopy-function #'icopy-iterator-list
    :node lis))

(defmethod iterator ((iter iterator-list))
  iter)

(defmethod iterator ((lis list))
  (ilist lis))

;; vector 

(defstruct 
  (iterator-array
    (:include iterator))
  (array nil :type array)
  (index 0 :type integer))

(defun inext-iterator-array (iter)
  (if (< (iterator-array-index iter) (length (iterator-array-array iter)))
    (prog1 (aref (iterator-array-array iter) (iterator-array-index iter))
      (incf (iterator-array-index iter)))
    *stop-iteration*))

(defun iskip-iterator-array (iter)
  (if (< (iterator-array-index iter) (length (iterator-array-array iter)))
    (prog1 t (incf (iterator-array-index iter)))
    nil)) ;; when reached eoi

(defun icopy-iterator-array (iter)
  (make-iterator-array 
    :inext-function (iterator-inext-function iter) 
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :array (iterator-array-array iter)
    :index (iterator-array-index iter)))

(defun iarray (arr)
  (make-iterator-array 
    :inext-function #'inext-iterator-array 
    :iskip-function #'iskip-iterator-array 
    :icopy-function #'icopy-iterator-array 
    :array arr))

(defmethod iterator ((iter iterator-array))
  iter)

(defmethod iterator ((arr array))
  (iarray arr))

;; range 

(defstruct 
  (iterator-range
    (:include iterator))
  (current 0 :type number)
  (start 0 :type number)
  (end 0 :type number)
  (step 1 :type number))

(defun iterator-range-end? (iter)
  (cond 
    ((< (iterator-range-step iter) 0)
      (<  (iterator-range-current iter) 
        (iterator-range-end iter)))
    ((>  (iterator-range-step iter) 0) 
      (>  (iterator-range-current iter) 
        (iterator-range-end iter)))))

(defun inext-iterator-range (iter)
  (if (iterator-range-end? iter) *stop-iteration*
    (prog1 (iterator-range-current iter)
      (incf (iterator-range-current iter)
        (iterator-range-step iter)))))

(defun iskip-iterator-range (iter)
  (if (iterator-range-end? iter) nil ;; when reached eoi
    (prog1 t
      (incf (iterator-range-current iter)
        (iterator-range-step iter)))))

(defun icopy-iterator-range (iter)
  (make-iterator-range 
    :inext-function (iterator-inext-function iter)
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :current (iterator-range-current iter)
    :start (iterator-range-start iter)
    :end (iterator-range-end iter)
    :step (iterator-range-step iter)))

(defun irange (start &optional end step)
  (cond 
    (step (make-iterator-range :inext-function #'inext-iterator-range :iskip-function #'iskip-iterator-range :icopy-function #'icopy-iterator-range :current start :start start :end end :step step))
    (end (make-iterator-range :inext-function #'inext-iterator-range :iskip-function #'iskip-iterator-range :icopy-function #'icopy-iterator-range :current 0 :start 0 :end end :step (if (< start end) 1 -1)))
    (t (make-iterator-range :inext-function #'inext-iterator-range :iskip-function #'iskip-iterator-range :icopy-function #'icopy-iterator-range :current 0 :start 0 :end start :step (if (< 0 start) 1 -1)))))

(defmethod iterator ((iter iterator-range))
  iter)

;; repeat 

(defstruct 
  (iterator-repeat
    (:include iterator))
  (count 0 :type integer)
  (element))

(defun inext-iterator-repeat (iter)
  (if (zerop (iterator-repeat-count iter)) *stop-iteration*
    (prog1 (iterator-repeat-element iter)
      (decf (iterator-repeat-count iter)))))

(defun iskip-iterator-repeat (iter)
  (if (zerop (iterator-repeat-count iter)) nil ;; when reached eoi
    (prog1 t (decf (iterator-repeat-count iter)))))

(defun icopy-iterator-repeat (iter)
  (make-iterator-repeat
    :inext-function (iterator-inext-function iter)
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :count (iterator-repeat-count iter)
    :element (iterator-repeat-element iter)))

(defun irepeat (count &optional element)
  (make-iterator-repeat 
    :inext-function #'inext-iterator-repeat
    :iskip-function #'iskip-iterator-repeat
    :icopy-function #'icopy-iterator-repeat
    :count count 
    :element element))

(defmethod iterator ((iter iterator-repeat))
  iter)

;; map 

(defstruct 
  (iterator-map 
    (:include iterator))
  (function nil :type function)
  (iterator))

(defun inext-iterator-map (iter)
  (let ((element (inext (iterator-map-iterator iter))))
    (if (eq element *stop-iteration*) *stop-iteration*
      (funcall (iterator-map-function iter) element))))

(defun iskip-iterator-map (iter)
  (iskip (iterator-map-iterator iter)))

(defun icopy-iterator-map (iter)
  (make-iterator-map 
    :inext-function (iterator-inext-function iter)
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :function (iterator-map-function iter)
    :iterator (icopy (iterator-map-iterator iter))))

(defun imap (function iter)
  (make-iterator-map 
    :inext-function #'inext-iterator-map
    :iskip-function #'iskip-iterator-map
    :icopy-function #'icopy-iterator-map
    :function function
    :iterator (iterator iter)))

(defmethod iterator ((iter iterator-map))
  iter)

;; filter 

(defstruct
  (iterator-filter
    (:include iterator))
  (function nil :type function)
  (iterator))

(defun inext-iterator-filter (iter)
  (let ((element (inext (iterator-filter-iterator iter))))
    (if (eq element *stop-iteration*) *stop-iteration*
      (if (funcall (iterator-filter-function iter) element) element
        (inext-iterator-filter iter)))))

(defun iskip-iterator-filter (iter)
  (let ((element (inext (iterator-filter-iterator iter))))
    (if (eq element *stop-iteration*) nil ;; when reached eoi
      (if (funcall (iterator-filter-function iter) element) t
        (iskip-iterator-filter iter)))))

(defun icopy-iterator-filter (iter)
  (make-iterator-filter
    :inext-function (iterator-inext-function iter)
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :function (iterator-filter-function iter)
    :iterator (iterator-filter-iterator iter)))

(defun ifilter (function iter)
  (make-iterator-filter
    :inext-function #'inext-iterator-filter
    :iskip-function #'iskip-iterator-filter
    :icopy-function #'icopy-iterator-filter
    :function function
    :iterator (iterator iter)))

(defmethod iterator ((iter iterator-filter))
  iter)

;; slice 

(defstruct 
  (iterator-slice
    (:include iterator))
  (current 0 :type integer)
  (start 0 :type integer)
  (end 0 :type integer)
  (iterator))

(defun setup-iterator-slice (iter)
  (loop while (< (iterator-slice-current iter) (iterator-slice-start iter))
    do (incf (iterator-slice-current iter))))

(defun inext-iterator-slice  (iter)
  (setup-iterator-slice iter)
  (let ((element (inext (iterator-slice-iterator iter))))
    (if (eq element *stop-iteration*) *stop-iteration*
      (prog1 element 
        (incf (iterator-slice-current iter))))))

(defun iskip-iterator-slice (iter)
  (setup-iterator-slice iter)
  (if (iskip (iterator-slice-iterator iter))
    (prog1 t (incf (iterator-slice-current iter))) 
    nil)) ;; when reached eoi

(defun icopy-iterator-slice (iter)
  (make-iterator-slice
    :inext-function (iterator-inext-function iter)
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :current (iterator-slice-current iter)
    :start (iterator-slice-start iter)
    :end (iterator-slice-end iter)
    :iterator (icopy (iterator-slice-iterator iter))))

(defun islice (start end iter)
  (make-iterator-slice
    :inext-function #'inext-iterator-slice
    :iskip-function #'iskip-iterator-slice
    :icopy-function #'icopy-iterator-slice
    :current 0
    :start start
    :end end 
    :iterator (iterator iter)))

(defmethod iterator ((iter iterator-slice))
  iter)

;; step 

(defstruct 
  (iterator-step 
    (:include iterator))
  (current 0 :type integer)
  (offset 0 :type integer)
  (step 1 :type integer)
  (iterator))

(defun setup-iterator-step (iter)
  (loop while (< (iterator-step-current iter) (iterator-step-offset iter)) do 
    (iskip (iterator-step-iterator iter))
    (incf (iterator-step-current iter))))

(defun inext-iterator-step (iter)
  (setup-iterator-step iter)
  (let ((element (inext (iterator-step-iterator iter))))
    (if (eq element *stop-iteration*) *stop-iteration*
      (prog1 element
        (loop repeat (iterator-step-step iter) do 
          (iskip (iterator-step-iterator iter))
          (incf (iterator-step-current iter)))))))

(defun iskip-iterator-step-in (count iter)
  (if (= 0 count) t
    (if (iskip (iterator-step-iterator iter))
      (progn
        (incf (iterator-step-current iter))
        (iskip-iterator-step-in (1- count) iter))
      nil))) ;; when reached eoi

(defun iskip-iterator-step (iter)
  (setup-iterator-step iter)  
  (iskip-iterator-step-in 
    (iterator-step-step iter) iter))

(defun icopy-iterator-step (iter)
  (make-iterator-step
    :inext-function (iterator-inext-function iter)
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :current (iterator-step-current iter)
    :offset (iterator-step-offset iter)
    :step (iterator-step-step iter)
    :iterator (icopy (iterator-step-iterator iter))))

(defun istep (offset step iter)
  (make-iterator-step
    :inext-function #'inext-iterator-step
    :iskip-function #'iskip-iterator-step
    :icopy-function #'icopy-iterator-step
    :current 0 
    :offset offset
    :step step
    :iterator iter))

(defmethod iterator ((iter iterator-step))
  iter)

;; append 

(defstruct 
  (iterator-append
    (:include iterator))
  (iterators))

(defun inext-iterator-append (iter)
  (if (null (iterator-append-iterators iter)) *stop-iteration*
    (let ((element (inext (car (iterator-append-iterators iter)))))
      (cond 
        ((eq element *stop-iteration*)
          (setf (iterator-append-iterators iter)
            (cdr (iterator-append-iterators iter))) 
          (inext-iterator-append iter))
        (t element)))))

(defun iskip-iterator-append (iter)
  (if (null (iterator-append-iterators iter)) nil ;; when reached eoi
    (if (iskip (car (iterator-append-iterators iter))) t
      (progn
        (setf (iterator-append-iterators iter)
          (cdr (iterator-append-iterators iter)))
        (iskip-iterator-append iter)))))

(defun icopy-iterator-append (iter)
  (make-iterator-append
    :inext-function (iterator-inext-function iter)
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :iterators (mapcar 'icopy (iterator-append-iterators iter))))

(defun iappend (&rest iters)
  (make-iterator-append 
    :inext-function #'inext-iterator-append
    :iskip-function #'iskip-iterator-append
    :icopy-function #'icopy-iterator-append
    :iterators (mapcar 'iterator iters)))

(defmethod iterator ((iter iterator-append))
  iter)

;; zip 

(defstruct 
  (iterator-zip 
    (:include iterator))
  (iterators))

(defun inext-iterator-zip (iter)
  (loop with element
    for citer in (iterator-zip-iterators iter) do 
    (setq element (inext citer)) 
    if (eq element *stop-iteration*) return *stop-iteration*
    collect element))

(defun iskip-iterator-zip (iter)
  (let ((status t))
    (loop for citer in (iterator-zip-iterators iter)
      unless (iskip citer) do
      (setq status nil)) ;; when reached eoi
    status))

(defun icopy-iterator-zip (iter)
  (make-iterator-zip 
    :inext-function (iterator-inext-function iter)
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :iterators (mapcar 'icopy (iterator-zip-iterators iter))))

(defun izip (&rest iters)
  (make-iterator-zip
    :inext-function #'inext-iterator-zip
    :iskip-function #'iskip-iterator-zip
    :icopy-function #'icopy-iterator-zip
    :iterators (mapcar 'iterator iters)))

(defmethod iterator ((iter iterator-zip))
  iter)

;; cache 

(defstruct 
  (iterator-cache 
    (:include iterator))
  (index 0 :type integer)
  (collection nil :type array)
  (iterator))

(defun inext-iterator-cache (iter)
  (if (< (iterator-cache-index iter) (length (iterator-cache-collection iter)))
    (prog1 (aref (iterator-cache-collection iter) (iterator-cache-index iter))
      (incf (iterator-cache-index iter)))
    (let ((element (inext (iterator-cache-iterator iter))))
      (if (eq element *stop-iteration*) *stop-iteration*
        (prog1 element 
          (vector-push-extend element 
            (iterator-cache-collection iter)))))))

(defun iskip-iterator-cache (iter)
  (if (< (iterator-cache-index iter) (length (iterator-cache-collection iter)))
    (prog1 t 
      (incf (iterator-cache-index iter)))
    (if (iskip (iterator-cache-iterator iter))
      (prog1 t
        (incf (iterator-cache-index iter)))
      nil))) ;; when reached eoi

(defun icopy-iterator-cache (iter)
  (make-iterator-cache 
    :inext-function (iterator-inext-function iter)
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :index (iterator-cache-index iter)
    :collection (iterator-cache-collection iter)
    :iterator (iterator-cache-iterator iter)))

(defun icache (iter)
  (make-iterator-cache 
    :inext-function #'inext-iterator-cache
    :iskip-function #'iskip-iterator-cache
    :icopy-function #'icopy-iterator-cache
    :collection (make-array 128 :fill-pointer 0 :adjustable t)
    :iterator iter))

(defmethod iterator ((iter iterator-cache))
  iter)

;; reverse 

(defstruct 
  (iterator-reverse 
    (:include iterator))
  (collection nil :type array)
  (index 0 :type integer)
  (iterator))

(defun init-iterator-reverse (iter)
  (let ((element (inext (iterator-reverse-iterator iter))))
    (unless (eq element *stop-iteration*)
      (vector-push-extend element 
        (iterator-reverse-collection iter))
      (init-iterator-reverse iter))))

(defun inext-iterator-reverse (iter)
  (if (= 0 (length (iterator-reverse-collection iter)))
    (init-iterator-reverse iter))
  (if (< (iterator-reverse-index iter) (length (iterator-reverse-collection iter)))
    (prog1 
      (aref (iterator-reverse-collection iter)
        (- (length (iterator-reverse-collection iter)) 1
          (iterator-reverse-index iter)))
      (incf (iterator-reverse-index iter)))
    *stop-iteration*))

(defun iskip-iterator-reverse (iter)
  (if (= 0 (length (iterator-reverse-collection iter)))
    (init-iterator-reverse iter))
  (if (< (iterator-reverse-index iter) (length (iterator-reverse-collection iter)))
    (prog1 t (incf (iterator-reverse-index iter)))
    nil)) ;; when reached eoi 

(defun icopy-iterator-reverse (iter)
  (make-iterator-reverse 
    :inext-function (iterator-inext-function iter) 
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :collection (iterator-reverse-collection iter)
    :index (iterator-reverse-index iter)
    :iterator (iterator-reverse-iterator iter)))

(defun ireverse (iter)
  (make-iterator-reverse 
    :inext-function #'inext-iterator-reverse 
    :iskip-function #'iskip-iterator-reverse 
    :icopy-function #'icopy-iterator-reverse 
    :collection (make-array 128 :fill-pointer 0 :adjustable t)
    :iterator (iterator iter)))

(defmethod iterator ((iter iterator-reverse))
  iter)

;; sort 

(defstruct 
  (iterator-sort 
    (:include iterator))
  (collection nil :type array)
  (index 0 :type integer)
  (function)
  (iterator))

(defun init-iterator-sort-collect (iter)
  (let ((element (inext (iterator-sort-iterator iter))))
    (unless (eq element *stop-iteration*)
      (vector-push-extend element 
        (iterator-sort-collection iter))
      (init-iterator-sort-collect iter))))

(defun init-iterator-sort (iter)
  (init-iterator-sort-collect iter)
  (sort (iterator-sort-collection iter)
    (iterator-sort-function iter)))

(defun inext-iterator-sort (iter)
  (if (= 0 (length (iterator-sort-collection iter)))
    (init-iterator-sort iter))
  (if (< (iterator-sort-index iter) (length (iterator-sort-collection iter)))
    (prog1
      (aref (iterator-sort-collection iter) (iterator-sort-index iter))
      (incf (iterator-sort-index iter)))
    *stop-iteration*))

(defun iskip-iterator-sort (iter)  
  (if (= 0 (length (iterator-sort-collection iter)))
    (init-iterator-sort iter))
  (if (< (iterator-sort-index iter) (length (iterator-sort-collection iter)))
    (prog1 t
      (incf (iterator-sort-index iter)))
    nil))

(defun icopy-iterator-sort (iter)
  (make-iterator-sort 
    :inext-function (iterator-inext-function iter)
    :iskip-function (iterator-iskip-function iter)
    :icopy-function (iterator-icopy-function iter)
    :collection (iterator-sort-collection iter)
    :function (iterator-sort-function iter)
    :iterator (iterator-sort-iterator iter)))

(defun isort (function iter)
  (make-iterator-sort 
    :inext-function #'inext-iterator-sort
    :iskip-function #'iskip-iterator-sort
    :icopy-function #'icopy-iterator-sort
    :collection (make-array 128 :fill-pointer 0 :adjustable t)
    :function function 
    :iterator (iterator iter)))

(defmethod iterator ((iter iterator-sort))
  iter)

;; file 

(defstruct 
  (iterator-file
    (:include iterator))
  (file)
  (read-function))

(defvar *iterator-file-eof*
  (make-symbol "*iterator-file-eof*"))

(defun inext-iterator-file (iter)
  (let ((char (funcall (iterator-file-read-function iter) nil *iterator-file-eof*)))
    (if (eq char *iterator-file-eof*) *stop-iteration*
      char)))

(defun iskip-iterator-file (iter)
  (let ((char (funcall (iterator-file-read-function iter) nil *iterator-file-eof*)))
    (if (eq char *iterator-file-eof*) nil t)))

(defun icopy-iterator-file (iter)
  (declare
    (ignore iter))
  (error "cannot icopy an iterator-file, because it cannot icopy a file status."))

(defun ifile (file &optional (function 'read-char))
  (make-iterator-file 
    :inext-function #'inext-iterator-file
    :iskip-function #'iskip-iterator-file 
    :icopy-function #'icopy-iterator-file 
    :read-function function
    :file file))

(defmethod iterator ((iter iterator-file))
  iter)

;; macro 

(defmacro doiterator (argument &body body)
  (let
    ((variable (nth 0 argument))
      (formula (nth 1 argument))
      (result (nth 2 argument))
      (iter (gensym)))
    `(loop with ,iter = (iterator ,formula) with ,variable do
       (setq ,variable (inext ,iter))
       if (eq ,variable *stop-iteration*) return ,result 
       do (progn ,@body))))

;; icount 

(defun icount-if-in (function count iter)
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) count 
      (if (funcall function element) 
        (icount-if-in function (1+ count) iter)
        (icount-if-in function count iter)))))

(defun icount-if (function iter)
  (icount-if-in function 0 iter))

(defun icount (item iter)
  (icount-if 
    (lambda (element)
      (eql item element))
    iter))

;; find 

(defun ifind-if (function iter)
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) (values nil nil)
      (if (funcall function element) (values element t)
        (ifind-if function iter)))))

(defun ifind (item iter)
  (ifind-if 
    (lambda (element)
      (eql item element))
    iter))

;; position 

(defun iposition-if-in (function count iter)
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) nil ;; when not found
      (if (funcall function element) count ;; when found 
        (iposition-if-in function (1+ count) iter)))))

(defun iposition-if (function iter)
  (iposition-if-in function 0 iter))

(defun iposition (item iter)
  (iposition-if 
    (lambda (element)
      (eql item element))
    iter))

;; reduce 

(defun ireduce-in (function result iter)
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) result 
      (ireduce-in function 
        (funcall function result element)
        iter))))

(defun ireduce (function iter)
  (let*
    ((element1 (inext iter))
      (element2 (inext iter)))
    (if (eq element1 *stop-iteration*) nil
      (if (eq element2 *stop-iteration*) element2
        (ireduce-in function 
          (funcall function element1 element2)
          iter)))))

;; ievery 

(defun ievery (function iter)
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) t
      (if (funcall function element)
        (ievery function iter)
        nil))))

;; isome 

(defun isome (func iter)
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) nil
      (if (funcall func element) t
        (isome func iter)))))
