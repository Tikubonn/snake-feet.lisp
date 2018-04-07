;; Copyright (c) 2018 tikubonn.
;; Released under the MIT license 
;; http://opensource.org/licenses/mitlicense.php 

;; package 

(provide 'snake-feet)

(defpackage snake-feet 
  (:use :common-lisp)
  (:export :iterator :ifunction :ilambda :irange :irepeat :imap :*stop-iteration*
    :ifiler :islice :iappend :ireverse :isort
    :ireduce :ifind-if :ifind :iposition-if :iposition
    :icount-if :icount :inext :iskip :icopy :doiterator
    :ievery :isome :istep :izip))

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

(defvar *stop-iteration* (make-symbol "*stop-iteration*"))

;; iterator

(defclass iterator nil nil)
(defmethod inext ((iter iterator)) nil)
(defmethod iskip ((iter iterator)) (inext iter))
(defmethod icopy (something) something)
(defmethod icopy ((iter iterator)))
(defmethod icopy-from ((iter iterator) (iter-from iterator)))

(defmethod iterator ((iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (the iterator iter))

(defmethod iskip ((iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (not (eq (inext iter) *stop-iteration*)))

(defmethod to-list ((iter iterator))
  (let ((top nil) (bottom nil))
    (do 
      ((element))
      ((eq (setq element (inext iter)) *stop-iteration*))
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
        ((eq  (setq element (inext iter)) *stop-iteration*))
        (vector-push-extend element sequence)))))

;; functional 

(defclass iterator-function (iterator)
  ((function
     :initarg :function
     :accessor iterator-function)))

(defmethod inext ((iter iterator-function))
  (funcall (iterator-function iter)))

(defmethod icopy ((iter iterator-function))
  (error "iterator-function cannot icopy, because I cannot icopy inner functions status."))

(defmethod icopy-from :after ((iter iterator-function) (iter-from iterator-function))
  (error "iterator-function cannot icopy, because I cannot icopy inner functions status."))

(defmethod iterator ((function function))
  (make-instance 'iterator-function :function function))

(defmethod iterator ((name symbol))
  (make-instance 'iterator-function :function name))

(defmacro ilambda (arguments &body body)
  `(iterator (lambda ,arguments ,@body)))

(defmethod ifunction ((function function))
  (iterator function))

(defmethod ifunction ((name symbol))
  (iterator  name))

;; range

(defclass iterator-range (iterator)
  ((current
     :initarg :start
     :initform 0
     :type number 
     :accessor iterator-current)
    (start
      :initarg :start
      :initform 0
      :type number 
      :accessor iterator-start)
    (end
      :initarg :end
      :initform 0
      :type number 
      :accessor iterator-end)
    (step
      :initarg :step
      :initform 1
      :type number 
      :accessor iterator-step)))

(defmethod iterator-range-end? ((iter iterator-range))
  (declare
    (type iterator-range iter)
    (optimize))
  (the boolean
    (cond 
      ((= (iterator-step iter) 0) nil)
      ((< (iterator-step iter) 0)
        (< (iterator-current iter) (iterator-end iter)))
      ((> (iterator-step iter) 0)
        (> (iterator-current iter) (iterator-end iter))))))

(defmethod inext ((iter iterator-range))
  (declare
    (type iterator-range iter)
    (optimize))
  (if (iterator-range-end? iter) *stop-iteration*
    (prog1 (iterator-current iter)
      (incf (iterator-current iter)
        (iterator-step iter)))))

(defmethod iskip ((iter iterator-range))
  (declare
    (type iterator-range iter)
    (optimize))
  (the boolean
    (if (iterator-range-end? iter) nil ;; when reached eoi
      (prog1 t 
        (incf (iterator-current iter)
          (iterator-step iter))))))

(defmethod icopy ((iter iterator-range))
  (declare
    (type iterator-range iter)
    (optimize))
  (let ((niter (make-instance 'iterator-range)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-range) (iter-from iterator-range))
  (declare
    (type iterator-range iter iter-from)
    (optimize))
  (setf (iterator-current iter) (iterator-current iter-from))
  (setf (iterator-start iter) (iterator-start iter-from))
  (setf (iterator-end iter) (iterator-end iter-from))
  (setf (iterator-step iter) (iterator-step iter-from)))

(defmethod irange (start &optional end  step)
  (declare
    (type integer start)
    (optimize))
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

(defmethod inext ((iter iterator-repeat))
  (declare
    (type iterator-repeat iter)
    (optimize))
  (if (<= (iterator-count iter) 0) *stop-iteration*
    (prog1 (iterator-element iter)
      (decf (iterator-count iter)))))

(defmethod iskip ((iter iterator-repeat))
  (declare
    (type iterator-repeat iter)
    (optimize))
  (the boolean
    (<= (iterator-count iter) 0)))

(defmethod icopy ((iter iterator-repeat))
  (declare
    (type iterator-repeat iter)
    (optimize))
  (let ((niter (make-instance 'iterator-repeat)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-repeat) (iter-from iterator-repeat))
  (declare
    (type iterator-repeat iter iter-from)
    (optimize))
  (setf (iterator-count iter) (iterator-count iter-from))
  (setf (iterator-element iter) (iterator-element iter-from)))

(defmethod irepeat (count &optional element)
  (declare
    (type integer count)
    (optimize))
  (make-instance 'iterator-repeat :count count :element element))

;; list

(defclass iterator-list (iterator)
  ((cons
     :initarg :cons
     :initform nil
     :accessor iterator-cons)))

(defmethod inext ((iter iterator-list))
  (declare 
    (type iterator-list iter)
    (optimize))
  (if (null (iterator-cons iter)) *stop-iteration*
    (pop (iterator-cons iter))))

(defmethod iskip ((iter iterator-list))
  (declare 
    (type iterator-list iter)
    (optimize))
  (the boolean
    (not (null (iterator-cons iter)))))

(defmethod icopy ((iter iterator-list))
  (declare 
    (type iterator-list iter)
    (optimize))
  (the iterator-list
    (let ((niter (make-instance 'iterator-list)))
      (prog1 niter (icopy-from niter iter)))))

(defmethod icopy-from :after ((iter iterator-list) (iter-from iterator-list))
  (declare 
    (type iterator-list iter iter-from)
    (optimize))
  (setf (iterator-cons iter) (iterator-cons iter-from)))

(defmethod iterator ((lis list))
  (declare 
    (type list lis)
    (optimize))
  (make-instance 'iterator-list :cons lis))

;; array

(defclass iterator-array (iterator)
  ((array
     :initarg :array
     :accessor iterator-array)
    (index
      :initform 0
      :accessor iterator-index)))

(defmethod inext ((iter iterator-array))
  (declare
    (type iterator-array iter)
    (optimize))
  (if (<= (length (iterator-array iter)) (iterator-index iter)) *stop-iteration*
    (prog1 (aref (iterator-array iter) (iterator-index iter))
      (incf (iterator-index iter)))))

(defmethod iskip ((iter iterator-array))
  (declare
    (type iterator-array iter)
    (optimize))
  (the boolean
    (< (iterator-index iter) 
      (length (iterator-array iter)))))

(defmethod icopy ((iter iterator-array))
  (declare
    (type iterator-array iter)
    (optimize))
  (the iterator-array
    (let ((niter (make-instance 'iterator-array)))
      (prog1 niter (icopy-from niter iter)))))

(defmethod icopy-from :after ((iter iterator-array) (iter-from iterator-array))
  (declare
    (type iterator-array iter iter-from)
    (optimize))  
  (setf (iterator-array iter) (iterator-array iter-from))
  (setf (iterator-index iter) (iterator-array iter-from)))

(defmethod iarray ((arr array))
  (declare
    (type array arr)
    (optimize))
  (make-instance 'iterator-array :array arr))

(defmethod iterator ((arr array))
  (declare
    (type array arr)
    (optimize))
  (iarray arr))

;; iterator 

(defclass iterator-iterator (iterator)
  ((iterator
     :initarg :iterator
     :initform nil
     :accessor iterator-iterator)))

(defmethod inext ((iter iterator-iterator))
  (declare
    (type iterator-iterator iter)
    (optimize))
  (inext (iterator-iterator iter)))

(defmethod iskip ((iter iterator-iterator))
  (declare
    (type iterator-iterator iter)
    (optimize)) 
  (iskip (iterator-iterator iter)))

(defmethod icopy ((iter iterator-iterator))
  (declare
    (type iterator-iterator iter)
    (optimize))
  (let ((niter (make-instance 'iterator-iterator)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-iterator) (iter-from iterator-iterator))
  (declare
    (type iterator-iterator iter iter-from)
    (optimize))
  (setf (iterator-iterator iter) (icopy (iterator-iterator iter-from))))

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

(defmethod inext ((iter iterator-map))
  (declare
    (type iterator-map iter)
    (optimize))
  (let ((element (inext (iterator-iterator iter))))
    (if (eq element *stop-iteration*) *stop-iteration*
      (funcall (iterator-function iter) element))))

(defmethod icopy ((iter iterator-map))
  (declare
    (type iterator-map iter)
    (optimize))
  (let ((niter (make-instance 'iterator-map)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-map) (iter-from iterator-map))
  (declare
    (type iterator-map iter iter-from)
    (optimize))  
  (setf (iterator-function iter) (iterator-function iter-from)))

(defmethod imap (function iter)
  (declare
    (optimize))
  (imap function (iterator iter)))

(defmethod imap (function (iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (make-instance 'iterator-map :function function :iterator iter))

;; filter

(defclass iterator-filter (iterator-iterator)
  ((function
     :initarg :function
     :accessor iterator-function)))

(defmethod inext ((iter iterator-filter))
  (declare
    (type iterator-filter iter)
    (optimize))
  (let ((element (inext (iterator-iterator iter))))
    (if (eq element *stop-iteration*) *stop-iteration*
      (if (funcall (iterator-function iter) element) element
        (inext iter)))))

(defmethod icopy ((iter iterator-filter))
  (declare
    (type iterator-filter iter)
    (optimize))
  (let ((niter (make-instance 'iterator-filter)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-filter) (iter-from iterator-filter))
  (declare
    (type iterator-filter iter iter-from)
    (optimize))
  (setf (iterator-function iter) (iterator-function iter-from)))

(defmethod ifilter (function iter)
  (declare
    (optimize))
  (ifilter function (iterator iter)))

(defmethod ifilter (function (iter iterator))
  (declare
    (type iterator-filter iter)
    (optimize))
  (make-instance 'iterator-filter :function function :iterator iter))

;; slice

(defclass iterator-slice (iterator-iterator)
  ((current
     :initform 0
     :type integer
     :accessor iterator-current)
    (start
      :initarg :start
      :initform 0
      :type integer 
      :accessor iterator-start)
    (end
      :initarg :end
      :initform 0
      :type integer
      :accessor iterator-end)))

(defun setup-iterator-slice (iter)
  (declare
    (type iterator-slice iter)
    (optimize))
  (loop while (< (iterator-current iter) (iterator-start iter)) do 
    (iskip (iterator-iterator iter))
    (incf (iterator-current iter))))

(defmethod inext ((iter iterator-slice))
  (declare
    (type iterator-slice iter)
    (optimize))
  (setup-iterator-slice iter)
  (if (< (iterator-end iter) (iterator-current iter)) *stop-iteration*
    (let ((element (inext (iterator-iterator iter))))
      (if (eq element *stop-iteration*) *stop-iteration*
        (prog1 element 
          (incf (iterator-current iter)))))))

(defmethod iskip ((iter iterator-slice))
  (declare
    (type iterator-slice iter)
    (optimize))
  (setup-iterator-slice iter) 
  (if (< (iterator-end iter) (iterator-current iter)) nil 
    (iskip (iterator-iterator iter))))

(defmethod icopy ((iter iterator-slice))
  (declare
    (type iterator-slice iter)
    (optimize))
  (let ((niter (make-instance 'iterator-slice)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-slice) (iter-from iterator-slice))
  (declare
    (type iterator-slice iter)
    (optimize))
  (setf (iterator-current iter) (iterator-current iter-from))
  (setf (iterator-start iter) (iterator-start iter-from))
  (setf (iterator-end iter) (iterator-end iter-from)))

(defmethod islice (start end iter)
  (declare
    (type integer start end)
    (optimize))
  (islice start end (iterator iter)))

(defmethod islice (start end (iter iterator))
  (declare
    (type integer start end)
    (type iterator iter)
    (optimize))
  (make-instance 'iterator-slice :start start :end end :iterator iter))

;; append 

(defclass iterator-append (iterator)
  ((iterators 
     :initarg :iterators
     :initform nil
     :accessor iterator-iterators)))

(defmethod inext ((iter iterator-append))
  (declare
    (type iterator-append iter)
    (optimize))
  (if (null (iterator-iterators iter)) *stop-iteration*
    (let ((element (inext (car (iterator-iterators iter)))))
      (if (eq element *stop-iteration*)
        (progn 
          (setf (iterator-iterators iter)
            (cdr (iterator-iterators iter)))
          (inext iter))
        element))))

(defmethod iskip ((iter iterator-append))
  (declare
    (type iterator-append iter)
    (optimize))
  (if (null (iterator-iterators iter)) nil
    (if (iskip (car (iterator-iterators iter))) t
      (progn 
        (setf (iterator-iterators iter)
          (cdr (iterator-iterators iter)))
        (iskip iter)))))

(defmethod icopy ((iter iterator-append))
  (declare
    (type iterator-append iter)
    (optimize))
  (let ((niter (make-instance 'iterator-append)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-append) (iter-from iterator-append))
  (declare
    (type iterator-append iter iter-from)
    (optimize))
  (setf (iterator-iterators iter) (mapcar 'icopy (iterator-iterators iter-from))))

(defmethod iappend (&rest iters)
  (declare
    (type list iters)
    (optimize))
  (make-instance 'iterator-append :iterators (mapcar 'iterator iters)))

;; zip 

(defclass iterator-zip (iterator)
  ((iterators
     :initarg :iterators
     :initform nil
     :accessor iterator-iterators)))

(defmethod inext ((iter iterator-zip))  
  (declare
    (type iterator-zip iter)
    (optimize))
  (loop with element 
    for iter in (iterator-iterators iter) do
    (setq element (inext iter))
    if (eq element *stop-iteration*)
    return *stop-iteration*
    else collect element))

(defun iskip-iterator-zip-in (iters)
  (declare
    (type list iters)
    (optimize))
  (if (null iters) t
    (if (iskip iters)
      (iskip-iterator-zip-in (cdr iters))
      nil)))

(defmethod iskip ((iter iterator-zip))
  (declare
    (type iterator-zip iter)
    (optimize))
  (iskip-iterator-zip-in 
    (iterator-iterators iter)))

(defmethod icopy ((iter iterator-zip))
  (declare
    (type iterator-zip iter)
    (optimize))
  (let ((niter (make-instance 'iterator-zip)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-zip) (iter-from iterator-zip))
  (declare
    (type iterator-zip iter iter-from)
    (optimize))
  (setf (iterator-iterators iter) (mapcar 'icopy (iterator-iterators iter-from))))

(defmethod izip (&rest iterators)
  (declare
    (type list iterators)
    (optimize))
  (make-instance 'iterator-zip :iterators (mapcar 'iterator iterators)))

;; step 

(defclass iterator-step (iterator-iterator)
  ((current 
     :initform 0
     :accessor iterator-current)
    (offset 
      :initarg :offset
      :initform 0
      :accessor iterator-offset)
    (step 
      :initarg :step
      :initform 1
      :accessor iterator-step)))

(defun init-iterator-step (iter)
  (declare
    (type iterator-step iter)
    (optimize))
  (loop while (< (iterator-current iter) (iterator-offset iter)) do
    (iskip (iterator-iterator iter))))

(defmethod inext ((iter iterator-step))
  (declare
    (type iterator-step iter)
    (optimize))
  (init-iterator-step iter)
  (loop repeat (iterator-step iter) do
    (iskip (iterator-iterator iter)))
  (inext (iterator-iterator iter)))

(defmethod iskip ((iter iterator-step))
  (declare
    (type iterator-step iter)
    (optimize))
  (init-iterator-step iter)
  (loop repeat (iterator-step  iter) do
    (iskip (iterator-iterator iter)))
  (iskip (iterator-iterator iter)))

(defmethod icopy ((iter iterator-step))
  (declare
    (type iterator-step iter)
    (optimize))
  (let ((niter (make-instance 'iterator-iskip)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-step) (iter-from iterator-step))
  (declare
    (type iterator-step iter iter-from)
    (optimize))
  (setf (iterator-current iter) (iterator-current iter-from))
  (setf (iterator-offset iter) (iterator-offset iter-from))
  (setf (iterator-step iter) (iterator-step iter-from)))

(defmethod istep (offset step iter)
  (declare
    (optimize (speed  3)))
  (istep (iterator iter)))

(defmethod istep (offset step (iter iterator))
  (declare
    (type integer offset step)
    (type iterator iter)
    (optimize))
  (make-instance 'iterator-iskip :offset offset :step step))

;; pool 

(defclass iterator-pool (iterator-iterator)
  ((collection
     :initarg :collection
     :accessor iterator-collection)))

(defun load-from-iterator-in (iter)
  (declare
    (type iterator-pool iter)
    (optimize))
  (let ((element (inext (iterator-iterator iter))))
    (unless (eq element *stop-iteration*)
      (vector-push-extend element 
        (iterator-collection iter))
      (load-from-iterator-in iter))))

(defmethod load-from-iterator ((iter iterator-pool))
  (declare
    (type iterator-pool iter)
    (optimize))
  (when (= 0 (length (iterator-collection iter)))
    (load-from-iterator-in iter)))

(defmethod did-load-from-iterator? ((iter iterator-pool))
  (declare
    (type iterator-pool iter)
    (optimize))
  (the boolean 
    (< 0 (length (iterator-collection iter)))))

;; reverse (slow)

(defclass iterator-reverse (iterator-pool)
  ((index
     :initform 0
     :type integer
     :accessor iterator-index)))

(defmethod inext ((iter iterator-reverse))
  (declare
    (type iterator-reverse iter)
    (optimize))
  (unless (did-load-from-iterator? iter)
    (load-from-iterator iter))
  (if (< (iterator-index iter) (length (iterator-collection iter)))
    (prog1 
      (aref (iterator-collection iter)
        (- (length (iterator-collection iter)) 1
          (iterator-index iter)))
      (incf (iterator-index iter)))
    *stop-iteration*))

(defmethod icopy ((iter iterator-reverse))
  (declare 
    (type iterator-reverse iter)
    (optimize))
  (the iterator-reverse
    (let ((niter (make-instance 'iterator-reverse)))
      (prog1 niter (icopy-from niter iter)))))

(defmethod icopy-from :after ((iter iterator-reverse) (iter-from iterator-reverse))
  (declare
    (type iterator-reverse iter iter-from)
    (optimize))
  (setf (iterator-index iter) (iterator-index iter-from)))

(defmethod ireverse (iter)
  (declare
    (optimize))
  (ireverse (iterator iter)))

(defmethod ireverse ((iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (make-instance 'iterator-reverse 
    :collection (make-array 0 :fill-pointer 0 :adjustable t)
    :iterator iter))

;; sort (slow)

(defclass iterator-sort (iterator-pool)
  ((function 
     :initarg :function
     :accessor iterator-function)
    (index
      :initform 0
      :type integer
      :accessor iterator-index)))

(defmethod load-from-iterator :after ((iter iterator-sort))
  (declare 
    (type iterator-sort iter)
    (optimize))
  (sort (iterator-collection iter)
    (iterator-function iter)))

(defmethod inext ((iter iterator-sort))
  (declare
    (type iterator-sort iter)
    (optimize))
  (unless (did-load-from-iterator? iter)
    (load-from-iterator iter))
  (if (< (iterator-index iter) (length (iterator-collection iter)))
    (prog1 
      (aref (iterator-collection iter)
        (- (length (iterator-collection iter)) 1
          (iterator-index iter)))
      (incf (iterator-index iter)))
    *stop-iteration*))

(defmethod icopy ((iter iterator-sort))
  (declare
    (type iterator-sort iter)
    (optimize))
  (let ((niter (make-instance 'iterator-sort)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-sort) (iter-from iterator-sort))
  (declare
    (type iterator-sort iter iter-from)
    (optimize))
  (setf (iterator-index iter) (iterator-index iter-from)))

(defmethod isort (function iter)
  (declare
    (optimize))
  (isort function (iterator iter)))

(defmethod isort (function (iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (make-instance 'iterator-sort 
    :collection (make-array 0 :fill-pointer 0 :adjustable t)
    :iterator iter))

;; cache 

(defclass iterator-cache (iterator-iterator)
  ((collection
     :initform (make-array 0 :fill-pointer 0 :adjustable t)
     :accessor iterator-collection)
    (index
      :initform 0
      :accessor iterator-index)))

(defmethod load-from-iterator ((iter iterator-cache))
  (declare
    (type iterator-cache iter)
    (optimize))
  (let ((element (inext (iterator-iterator iter))))
    (vector-push-extend element (iterator-collection iter))))

(defmethod inext ((iter iterator-cache))
  (declare
    (type iterator-cache iter)
    (optimize))
  (unless (< (iterator-index iter) (length (iterator-collection iter)))
    (load-from-iterator iter))
  (if (< (iterator-index iter) (length (iterator-collection iter)))
    (prog1 (aref (iterator-index iter) (iterator-collection iter))
      (incf (iterator-index iter)))
    *stop-iteration*))

(defmethod icopy ((iter iterator-cache))
  (declare 
    (type iterator-cache iter)
    (optimize))
  (let ((niter (make-instance 'iterator-cache)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-cache) (iter-from iterator-cache))
  (declare
    (type iterator-cache iter iter-from)
    (optimize))
  (setf (iterator-collection iter) (iterator-collection iter-from))
  (setf (iterator-index iter) (iterator-index iter-from)))

(defmethod icache (iter)
  (declare
    (optimize))
  (icache (iterator iter)))

(defmethod icache ((iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (make-instance 'iterator-cache :iterator iter))

;; file 

(define-condition iterator-file-error (iterator-error)
  ()
  (:report
    (lambda (condition stream)
      (format stream "iterator-file-error: ~a~%"
        (iterator-error-message condition)))))

(defvar *iterator-file-eof* (make-symbol "iterator-file-eof"))

(defclass iterator-file (iterator)
  ((file 
     :initarg :file
     :accessor iterator-file)
    (read-function
      :initarg :read-function
      :initform 'read-char
      :accessor iterator-read-function)))

(defmethod inext ((iter iterator-file))
  (declare
    (type iterator-file iter)
    (optimize))
  (let ((element (funcall (iterator-read-function iter) (iterator-file iter) nil *iterator-file-eof*)))
    (if (eq element *iterator-file-eof*) *stop-iteration* element)))

(defmethod icopy ((iter iterator-file))
  (declare
    (ignore iter)
    (type iterator-file iter)
    (optimize))
  (error 
    (make-instance 'iterator-file-error 
      :message "iterator-file class cannot coping.")))

(defmethod icopy-from :after ((iter iterator-file) (iter-from iterator-file))
  (declare
    (ignore iter iter-from)
    (type iterator-file iter iter-from)
    (optimize))
  (error
    (make-instance 'iterator-file-error
      :message "iterator-file class cannot coping.")))

(defmethod ifile (file &key (read-function 'read-char))
  (declare
    (optimize))
  (make-instance 'iterator-file :file file :read-function read-function))

;; every 

(defmethod ievery ((function function) iter)
  (declare
    (type function function)
    (optimize))
  (ievery function (iterator iter)))

(defmethod ievery ((function function) (iter iterator))
  (declare
    (type function function)
    (type iterator iter)
    (optimize))
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) t
      (if (funcall function element)
        (ievery function iter)
        nil))))

;; some 

(defmethod isome ((function function) iter)
  (declare 
    (optimize))
  (isome function (iterator iter)))

(defmethod isome ((function function) (iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) nil
      (if (funcall function element) t
        (isome function iter)))))

;; reduce

(defmethod ireduce-in (function result (iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) result
      (ireduce-in function (funcall function result element) iter))))

(defmethod ireduce (function iter)
  (declare 
    (optimize))
  (ireduce function (iterator iter)))

(defmethod ireduce (function (iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (let*
    ((element1 (inext iter))
      (element2 (inext iter)))
    (if (eq element1 *stop-iteration*) nil
      (if (eq element2 *stop-iteration*) element1
        (ireduce-in function 
          (funcall function element1 element2)
          iter)))))

;; findif 

(defmethod ifind-if (function iter)
  (declare
    (optimize))
  (ifind-if function (iterator iter)))

(defmethod ifind-if (function (iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) (values nil nil)
      (if (funcall function element) (values element t)
        (ifind-if function iter)))))

(defmethod ifind (item iter)
  (declare
    (optimize))
  (ifind item (iterator iter)))

(defmethod ifind (item (iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (ifind-if 
    (lambda (element)
      (equal element  item))
    iter))

;; position if 

(defmethod iposition-if-in (function (count integer) (iter iterator))
  (declare
    (type integer count)
    (type iterator iter)
    (optimize))
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) nil
      (if (funcall function element) count 
        (iposition-if-in function (1+ count) iter)))))

(defmethod iposition-if (function iter)
  (declare
    (type iterator iter)
    (optimize))
  (iposition-if function (iterator iter)))

(defmethod iposition-if (function (iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (iposition-if-in function 0 iter))

(defmethod iposition (item iter)
  (declare
    (optimize))
  (iposition-if item iter))

(defmethod iposition (item (iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (iposition-if 
    (lambda (element)
      (equal element iter))
    iter))

;; icount 

(defmethod icount-if-in (function (count integer) (iter iterator))
  (declare
    (type iterator iter)
    (type integer count)
    (optimize))
  (let ((element (inext iter)))
    (if (eq element *stop-iteration*) count 
      (if (funcall function element)
        (icount-if-in function (1+ count) iter)
        (icount-if-in function count iter)))))

(defmethod icount-if (function iter)
  (declare
    (optimize))
  (icount-if function (iterator iter)))

(defmethod icount-if (function (iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (icount-if-in function 0 iter))

(defmethod icount (item iter)
  (declare
    (optimize))
  (icount item (iterator iter)))

(defmethod icount (item (iter iterator))
  (declare
    (type iterator iter)
    (optimize))
  (icount-if 
    (lambda (element)
      (equal item element))
    iter))

;; macro utilities

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
