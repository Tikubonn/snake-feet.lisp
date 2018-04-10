;; Copyright (c) 2018 tikubonn.
;; Released under the MIT license 
;; http://opensource.org/licenses/mitlicense.php 

;; package 

(provide 'mamba-feet)

(defpackage mamba-feet 
  (:use :common-lisp)
  (:export :inext :iskip :icopy :to-list :to-array
    :iterator :ilist :iarray :ifunction :ilambda :irange :irepeat :ifile :icache
    :imap :ifilter :islice :istep :iappend :izip :ireverse :isort
    :iposition-if :iposition :ifind-if :ifind :icount-if :icount :isome :ievery
    :doiterator))

(in-package :mamba-feet)

;; define generic

(defgeneric iterator (iter))

;; iterator

(defstruct iterator 
  (inext-function nil :type function)
  (iskip-function nil :type function)
  (icopy-function nil :type function))

(defun inext (iter)
  (declare 
    (type iterator iter)
    (optimize (speed 3)))
  (funcall (the function (iterator-inext-function iter)) iter))

(defun iskip (iter)
  (declare
    (type iterator iter)
    (optimize (speed 3)))
  (funcall (the function (iterator-iskip-function iter)) iter))

(defun icopy (iter)
  (declare
    (type iterator iter)
    (optimize (speed 3)))
  (funcall (the function (iterator-icopy-function iter)) iter))

(defmethod iterator ((iter iterator))
  (declare 
    (type iterator iter)
    (optimize))
  (the iterator iter))

;; list-builder 

(defstruct list-builder 
  (top nil :type list)
  (bottom nil :type list)
  (length 0 :type fixnum))

(defun push-list-builder (item iter)
  (declare
    (type list-builder iter)
    (optimize (speed 3)))
  (cond 
    ((and (null (list-builder-top iter))
       (null (list-builder-bottom iter)))
      (let ((seq (cons item nil)))
        (setf (list-builder-top iter) seq)
        (setf (list-builder-bottom iter) seq)))
    (t 
      (let ((seq (cons item (list-builder-top iter))))
        (setf (list-builder-top iter) seq)))))

(defun push-last-list-builder (item iter)
  (declare
    (type list-builder iter)
    (optimize (speed 3)))
  (cond 
    ((and (null (list-builder-top iter))
       (null (list-builder-bottom iter)))
      (let ((seq (cons item nil)))
        (setf (list-builder-top iter) seq)
        (setf (list-builder-bottom iter) seq)
        (incf (list-builder-length iter))))
    (t 
      (let ((seq (cons item nil)))
        (setf (cdr (list-builder-bottom iter)) seq)
        (setf (list-builder-bottom iter) seq)
        (incf (list-builder-length iter))))))

(defun length-list-builder (iter)
  (declare
    (type list-builder iter)
    (optimize (speed 3)))
  (list-builder-length iter))

(defun result-list-builder (iter)
  (declare
    (type list-builder iter)
    (optimize (speed 3)))
  (list-builder-top iter))

(defmacro result-list-builder-as-array (builder-form &rest options)
  (let ((builder (gensym)))
    `(let ((,builder ,builder-form))
       (let ((seq (make-array (length-list-builder ,builder) ,@options)))
         (prog1 seq
           (loop for element in (result-list-builder ,builder) 
             for index from 0 below (length-list-builder ,builder) do
             (setf (aref seq index) element)))))))

;; to-list 

(defun to-list-in (builder iter)
  (declare
    (type list-builder builder)
    (type iterator iter)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (if eoi nil
      (progn 
        (push-last-list-builder element builder)
        (to-list-in builder iter)))))

(defun to-list (iter)
  (declare
    (type iterator iter)
    (optimize (speed 3)))
  (let ((builder (make-list-builder)))
    (to-list-in builder iter)
    (result-list-builder builder)))

;; to-array 

(defun to-array-in (builder iter)
  (declare
    (type list-builder builder)
    (type iterator iter))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (if eoi nil
      (progn
        (push-last-list-builder element builder)
        (to-array-in builder iter)))))

(defmacro to-array (iter-form &rest options)
  (let ((iter (gensym)) (builder (gensym)))
    `(let ((,iter ,iter-form) 
            (,builder (make-list-builder)))
       (to-array-in ,builder ,iter)
       (result-list-builder-as-array ,builder ,@options))))

;; function 

(defstruct 
  (iterator-function 
    (:include iterator))
  (function nil :type function))

(defun inext-iterator-function (iter)
  (declare
    (type iterator-function iter))
  (funcall (iterator-function-function iter)))

(defun iskip-iterator-function (iter)
  (declare
    (type iterator-function iter))
  (multiple-value-bind
    (element eoi)
    (inext-iterator-function iter)
    (declare (ignore element)) ;; declaration for multiple-value-bind 
    (not eoi)))

(defun icopy-iterator-function (iter)
  (declare
    (ignore iter)
    (type iterator-function iter)
    (optimize))
  (error "iterator-function cannot icopy, because I cannot icopy inner functions status."))

(defmacro ilambda (arguments &body body)
  `(ifunction (lambda ,arguments ,@body)))

(defun ifunction (function)
  (declare
    (optimize))
  (the iterator-function 
    (make-iterator-function
      :inext-function #'inext-iterator-function
      :iskip-function #'iskip-iterator-function
      :icopy-function #'icopy-iterator-function
      :function function)))

(defmethod iterator ((iter iterator-function))
  (declare
    (type iterator-function iter)
    (optimize))
  (the iterator-function iter))

(defmethod iterator ((function function))  
  (declare
    (type function function)
    (optimize))
  (the iterator-function 
    (ifunction function)))

;; list 

(defstruct 
  (iterator-list 
    (:include iterator))
  (node nil :type list))

(defun inext-iterator-list (iter)
  (declare
    (type iterator-list iter))
  (if (null (iterator-list-node iter)) (values nil t)
    (prog1 (values (car (iterator-list-node iter)) nil)
      (setf (iterator-list-node iter)
        (cdr (iterator-list-node iter))))))

(defun iskip-iterator-list (iter)
  (declare
    (type iterator-list iter))
  (if (null (iterator-list-node iter)) nil
    (prog1 t 
      (setf (iterator-list-node iter)
        (cdr (iterator-list-node iter))))))

(defun icopy-iterator-list (iter)
  (declare
    (type iterator-list iter)
    (optimize))
  (the iterator-list
    (make-iterator-list 
      :inext-function (iterator-inext-function iter)
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :node (iterator-list-node iter))))

(defun ilist (lis)
  (declare
    (type list lis)
    (optimize))
  (the iterator-list
    (make-iterator-list 
      :inext-function #'inext-iterator-list 
      :iskip-function #'iskip-iterator-list 
      :icopy-function #'icopy-iterator-list
      :node lis)))

(defmethod iterator ((iter iterator-list))
  (declare
    (type iterator-list iter)
    (optimize))
  (the iterator-list iter))

(defmethod iterator ((lis list))
  (declare
    (type list lis)
    (optimize))
  (the iterator-list (ilist lis)))

;; example 

(defstruct 
  (iterator-array
    (:include iterator))
  (array nil :type array)
  (index 0 :type fixnum))

(defun inext-iterator-array (iter)
  (declare
    (type iterator-array iter))
  (if (< (iterator-array-index iter) (length (iterator-array-array iter)))
    (prog1 (values (aref (iterator-array-array iter) (iterator-array-index iter)) nil)
      (incf (iterator-array-index iter)))
    (values nil t)))

(defun iskip-iterator-array (iter)
  (declare
    (type iterator-array iter))
  (if (< (iterator-array-index iter) (length (iterator-array-array iter)))
    (prog1 t (incf (iterator-array-index iter)))
    nil))

(defun icopy-iterator-array (iter)
  (declare
    (type iterator-array iter)
    (optimize))
  (the iterator-array 
    (make-iterator-array 
      :inext-function (iterator-inext-function iter) 
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :array (iterator-array-array iter)
      :index (iterator-array-index iter))))

(defun iarray (arr)
  (declare
    (type array arr)
    (optimize))
  (the iterator-array
    (make-iterator-array 
      :inext-function #'inext-iterator-array 
      :iskip-function #'iskip-iterator-array 
      :icopy-function #'icopy-iterator-array 
      :array arr)))

(defmethod iterator ((iter iterator-array))
  (declare
    (type iterator-array iter)
    (optimize))
  (the iterator-array iter))

(defmethod iterator ((arr array))
  (declare
    (type array arr)
    (optimize))
  (the iterator-array (iarray arr)))

;; range 

(defstruct 
  (iterator-range
    (:include iterator))
  (current 0 :type number)
  (start 0 :type number)
  (end 0 :type number)
  (step 1 :type number))

(defun iterator-range-end? (iter)
  (declare
    (type iterator-range iter))
  (the boolean
    (cond 
      ((= (iterator-range-step iter) 0) nil)
      ((< (iterator-range-step iter) 0) 
        (<= (iterator-range-current iter)
          (iterator-range-end iter)))
      ((> (iterator-range-step iter) 0)
        (>= (iterator-range-current iter) 
          (iterator-range-end iter))))))

(defun inext-iterator-range (iter)
  (declare
    (type iterator-range iter))
  (if (iterator-range-end? iter) (values nil t)
    (prog1 (values (iterator-range-current iter) nil)
      (incf (iterator-range-current iter)
        (iterator-range-step iter)))))

(defun iskip-iterator-range (iter)
  (declare
    (type iterator-range iter))
  (the boolean
    (if (iterator-range-end? iter) nil ;; when reached eoi
      (prog1 t
        (incf (iterator-range-current iter)
          (iterator-range-step iter))))))

(defun icopy-iterator-range (iter)
  (declare
    (type iterator-range iter))
  (the iterator-range
    (make-iterator-range 
      :inext-function (iterator-inext-function iter)
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :current (iterator-range-current iter)
      :start (iterator-range-start iter)
      :end (iterator-range-end iter)
      :step (iterator-range-step iter))))

(defun irange (start &optional end step)
  (declare
    (type number start))
  (the iterator-range
    (cond 
      (step (make-iterator-range :inext-function #'inext-iterator-range :iskip-function #'iskip-iterator-range :icopy-function #'icopy-iterator-range :current start :start start :end end :step step))
      (end (make-iterator-range :inext-function #'inext-iterator-range :iskip-function #'iskip-iterator-range :icopy-function #'icopy-iterator-range :current start :start start :end end :step (if (< start end) 1 -1)))
      (t (make-iterator-range :inext-function #'inext-iterator-range :iskip-function #'iskip-iterator-range :icopy-function #'icopy-iterator-range :current 0 :start 0 :end start :step (if (< 0 start) 1 -1))))))

(defmethod iterator ((iter iterator-range))
  (declare
    (type iterator-range iter)
    (optimize (speed 3)))
  (the iterator-range iter))

;; repeat 

(defstruct 
  (iterator-repeat
    (:include iterator))
  (count 0 :type integer)
  (element))

(defun inext-iterator-repeat (iter)
  (declare
    (type iterator-repeat iter)
    (optimize))
  (if (zerop (iterator-repeat-count iter)) (values nil t)
    (prog1 (values (iterator-repeat-element iter) nil)
      (decf (iterator-repeat-count iter)))))

(defun iskip-iterator-repeat (iter)
  (declare
    (type iterator-repeat iter)
    (optimize))
  (the boolean
    (if (zerop (iterator-repeat-count iter)) nil ;; when reached eoi
      (prog1 t (decf (iterator-repeat-count iter))))))

(defun icopy-iterator-repeat (iter)
  (declare
    (type iterator-repeat iter)
    (optimize))
  (the iterator-repeat
    (make-iterator-repeat
      :inext-function (iterator-inext-function iter)
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :count (iterator-repeat-count iter)
      :element (iterator-repeat-element iter))))

(defun irepeat (count &optional element)
  (declare
    (type integer count)
    (optimize))
  (the iterator-repeat
    (make-iterator-repeat 
      :inext-function #'inext-iterator-repeat
      :iskip-function #'iskip-iterator-repeat
      :icopy-function #'icopy-iterator-repeat
      :count count 
      :element element)))

(defmethod iterator ((iter iterator-repeat))
  (declare
    (type iterator-repeat iter)
    (optimize (speed 3)))
  (the iterator-repeat iter))

;; map 

(defstruct 
  (iterator-map 
    (:include iterator))
  (function nil :type function)
  (iterator))

(defun inext-iterator-map (iter)
  (declare 
    (type iterator-map iter))
  (multiple-value-bind 
    (element eoi)
    (inext (iterator-map-iterator iter))
    (if eoi (values nil t)
      (values (funcall (iterator-map-function iter) element) nil))))

(defun iskip-iterator-map (iter)
  (declare
    (type iterator-map iter))
  (iskip (iterator-map-iterator iter)))

(defun icopy-iterator-map (iter)
  (declare
    (type iterator-map iter)
    (optimize (speed 3)))
  (the iterator-map 
    (make-iterator-map 
      :inext-function (iterator-inext-function iter)
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :function (iterator-map-function iter)
      :iterator (icopy (iterator-map-iterator iter)))))

(defun imap (function iter)
  (declare
    (type function function)
    (optimize (speed 3)))
  (the iterator-map
    (make-iterator-map 
      :inext-function #'inext-iterator-map
      :iskip-function #'iskip-iterator-map
      :icopy-function #'icopy-iterator-map
      :function function
      :iterator (iterator iter))))

(defmethod iterator ((iter iterator-map))
  (declare
    (type iterator-map iter)
    (optimize (speed 3)))
  (the iterator-map iter))

;; filter 

(defstruct
  (iterator-filter
    (:include iterator))
  (function nil :type function)
  (iterator nil :type iterator))

(defun inext-iterator-filter (iter)
  (declare
    (type iterator-filter iter))
  (multiple-value-bind 
    (element eoi)
    (inext (iterator-filter-iterator iter))
    (if eoi (values nil t)
      (if (funcall (iterator-filter-function iter) element) 
        (values element nil)
        (inext-iterator-filter iter)))))

(defun iskip-iterator-filter (iter)
  (declare
    (type iterator-filter iter))
  (multiple-value-bind 
    (element eoi)
    (inext (iterator-filter-iterator iter))
    (if eoi nil
      (if (funcall (iterator-filter-function iter) element) t 
        (iskip-iterator-filter iter)))))

(defun icopy-iterator-filter (iter)
  (declare
    (type iterator-filter iter)
    (optimize (speed 3)))
  (the iterator-filter
    (make-iterator-filter
      :inext-function (iterator-inext-function iter)
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :function (iterator-filter-function iter)
      :iterator (iterator-filter-iterator iter))))

(defun ifilter (function iter)
  (declare
    (type iterator iter)
    (optimize (speed 3)))
  (the iterator-filter
    (make-iterator-filter
      :inext-function #'inext-iterator-filter
      :iskip-function #'iskip-iterator-filter
      :icopy-function #'icopy-iterator-filter
      :function function
      :iterator (iterator iter))))

(defmethod iterator ((iter iterator-filter))
  (declare
    (type iterator-filter iter)
    (optimize (speed 3)))
  (the iterator-filter iter))

;; slice 

(defstruct 
  (iterator-slice
    (:include iterator))
  (current 0 :type fixnum)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (iterator nil :type iterator))

(defun setup-iterator-slice (iter)
  (declare
    (type iterator-slice iter)
    (optimize (speed 3)))
  (loop while (< (iterator-slice-current iter) (iterator-slice-start iter)) do
    (iskip (iterator-slice-iterator iter))
    (incf (iterator-slice-current iter))))

(defun inext-iterator-slice (iter)
  (declare
    (type iterator-slice iter)
    (optimize (speed 3)))
  (setup-iterator-slice iter)
  (cond
    ((< (iterator-slice-current iter) (iterator-slice-end iter))
      (multiple-value-bind
        (element eoi)
        (inext (iterator-slice-iterator iter))
        (if eoi (values nil t)
          (multiple-value-prog1
            (values element nil)
            (incf (iterator-slice-current iter))))))
    (t (values nil t))))

(defun iskip-iterator-slice (iter)
  (declare
    (type iterator-slice iter)
    (optimize (speed 3)))
  (setup-iterator-slice iter)
  (if (< (iterator-slice-current iter) (iterator-slice-end iter))
    (if (iskip (iterator-slice-iterator iter))
      (prog1 t (incf (iterator-slice-current iter)))
      nil)
    nil))

(defun icopy-iterator-slice (iter)
  (declare
    (type iterator-slice iter)
    (optimize (speed 3)))
  (the iterator-slice
    (make-iterator-slice
      :inext-function (iterator-inext-function iter)
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :current (iterator-slice-current iter)
      :start (iterator-slice-start iter)
      :end (iterator-slice-end iter)
      :iterator (icopy (iterator-slice-iterator iter)))))

(defun islice (start end iter)
  (declare
    (type fixnum start end)
    (optimize (speed 3)))
  (the iterator-slice
    (make-iterator-slice
      :inext-function #'inext-iterator-slice
      :iskip-function #'iskip-iterator-slice
      :icopy-function #'icopy-iterator-slice
      :current 0
      :start start
      :end end 
      :iterator (iterator iter))))

(defmethod iterator ((iter iterator-slice))
  (declare
    (type iterator-slice iter)
    (optimize (speed 3)))
  (the iterator-slice iter))

;; step

(defstruct 
  (iterator-step 
    (:include iterator))
  (current 0 :type fixnum)
  (offset 0 :type fixnum)
  (step 1 :type fixnum)
  (iterator nil :type iterator))

(defun setup-iterator-step (iter)
  (declare
    (type iterator-step iter)
    (optimize (speed 3)))
  (loop while (< (iterator-step-current iter) (iterator-step-offset iter)) do 
    (iskip (iterator-step-iterator iter))
    (incf (iterator-step-current iter))))

(defun inext-iterator-step (iter)
  (declare
    (type iterator-step iter)
    (optimize (speed 3)))
  (setup-iterator-step iter)
  (multiple-value-prog1 
    (inext (iterator-step-iterator iter))
    (loop repeat (1- (iterator-step-step iter)) do
      (iskip (iterator-step-iterator iter))
      (incf (iterator-step-current iter)))))

(defun iskip-iterator-step (iter)
  (declare
    (type iterator-step iter)
    (optimize (speed 3)))
  (setup-iterator-step iter)
  (multiple-value-prog1 
    (iskip (iterator-step-iterator iter))
    (loop repeat (1- (iterator-step-step iter)) do
      (iskip (iterator-step-iterator iter))
      (incf (iterator-step-current iter)))))

(defun icopy-iterator-step (iter)
  (declare
    (type iterator-step iter)
    (optimize (speed 3)))
  (the iterator-step
    (make-iterator-step
      :inext-function (iterator-inext-function iter)
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :current (iterator-step-current iter)
      :offset (iterator-step-offset iter)
      :step (iterator-step-step iter)
      :iterator (icopy (iterator-step-iterator iter)))))

(defun istep (offset step iter)
  (declare
    (type fixnum offset)
    (type fixnum step)
    (type iterator iter))
  (the iterator-step
    (make-iterator-step
      :inext-function #'inext-iterator-step
      :iskip-function #'iskip-iterator-step
      :icopy-function #'icopy-iterator-step
      :current 0 
      :offset offset
      :step step
      :iterator (iterator iter))))

(defmethod iterator ((iter iterator-step))
  (declare
    (type iterator-step iter))
  (the iterator-step iter))

;; append 

(defstruct 
  (iterator-append
    (:include iterator))
  (iterators nil :type list))

(defun inext-iterator-append (iter)
  (declare
    (type iterator-append iter))
  (if (null (iterator-append-iterators iter)) (values nil t)
    (multiple-value-bind 
      (element eoi)
      (inext (car (iterator-append-iterators iter)))
      (if eoi 
        (progn 
          (setf (iterator-append-iterators iter)
            (cdr (iterator-append-iterators iter)))
          (inext-iterator-append iter))
        (values element nil)))))

(defun iskip-iterator-append (iter)
  (declare
    (type iterator-append iter)
    (optimize (speed 3)))
  (if (null (iterator-append-iterators iter)) nil ;; when reached eoi
    (if (iskip (car (iterator-append-iterators iter))) t
      (progn
        (setf (iterator-append-iterators iter)
          (cdr (iterator-append-iterators iter)))
        (iskip-iterator-append iter)))))

(defun icopy-iterator-append (iter)
  (declare
    (type iterator-append iter)
    (optimize (speed 3)))
  (the iterator-append 
    (make-iterator-append
      :inext-function (iterator-inext-function iter)
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :iterators (mapcar 'icopy (iterator-append-iterators iter)))))

(defun iappend (&rest iters)
  (declare
    (optimize (speed 3)))
  (the iterator-append 
    (make-iterator-append 
      :inext-function #'inext-iterator-append
      :iskip-function #'iskip-iterator-append
      :icopy-function #'icopy-iterator-append
      :iterators (mapcar 'iterator iters))))

(defmethod iterator ((iter iterator-append))
  (declare
    (type iterator-append iter)
    (optimize (speed 3)))
  (the iterator-append iter))

;; zip 

(defstruct 
  (iterator-zip 
    (:include iterator))
  (iterators nil :type list))

(defun inext-iterator-zip-in (collection iters)
  (declare
    (type list collection iters)
    (optimize (speed 3)))
  (if (null iters) (values (nreverse collection) nil)
    (multiple-value-bind 
      (element eoi)
      (inext (car iters))
      (declare 
        (type boolean eoi)) ;; declaration for multiple value bind 
      (if eoi (values nil t)
        (inext-iterator-zip-in 
          (cons element collection)
          (cdr iters))))))

(defun inext-iterator-zip (iter)
  (declare
    (type iterator-zip iter)
    (optimize (speed 3)))
  (inext-iterator-zip-in nil
    (iterator-zip-iterators iter)))

(defun iskip-iterator-zip-in (iters)
  (declare
    (type list iters)
    (optimize (speed 3)))
  (if (null iters) t
    (if (iskip (car iters))
      (iskip-iterator-zip-in (cdr iters))
      nil))) ;; when reached eoi

(defun iskip-iterator-zip (iter)
  (declare
    (type iterator-zip iter)
    (optimize (speed 3)))
  (iskip-iterator-zip-in 
    (iterator-zip-iterators iter)))

(defun icopy-iterator-zip (iter)
  (declare
    (type iterator-zip iter)
    (optimize (speed 3)))
  (the iterator-zip 
    (make-iterator-zip 
      :inext-function (iterator-inext-function iter)
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :iterators (mapcar 'icopy (iterator-zip-iterators iter)))))

(defun izip (&rest iters)
  (declare
    (optimize (speed  3)))
  (the iterator-zip
    (make-iterator-zip
      :inext-function #'inext-iterator-zip
      :iskip-function #'iskip-iterator-zip
      :icopy-function #'icopy-iterator-zip
      :iterators (mapcar 'iterator iters))))

(defmethod iterator ((iter iterator-zip))
  (declare
    (type iterator-zip iter)
    (optimize (speed 3)))
  (the iterator-zip iter))

;; cache 

(defstruct
  (iterator-cache
    (:include iterator))
  (index 0 :type fixnum)
  (collection nil :type array)
  (iterator nil :type iterator))

(defun inext-iterator-cache (iter)
  (declare
    (type iterator-cache iter))
  (cond 
    ((< (iterator-cache-index iter) (length (iterator-cache-collection iter)))
      (multiple-value-prog1 
        (values (aref (iterator-cache-collection iter) (iterator-cache-index iter)) nil)
        (incf (iterator-cache-index iter))))
    (t
      (multiple-value-bind 
        (element eoi)
        (inext (iterator-cache-iterator iter))
        (declare 
          (type boolean eoi))
        (if eoi (values nil t)
          (multiple-value-prog1 
            (values element nil)
            (vector-push-extend element
              (iterator-cache-collection iter))
            (incf (iterator-cache-index iter))))))))

(defun iskip-iterator-cache (iter)
  (declare
    (type iterator-cache iter)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext-iterator-cache iter)
    (declare 
      (type boolean eoi)
      (ignore element)) ;; declaration for multiple-value-bind 
    (not eoi)))

(defun icopy-iterator-cache (iter)
  (declare
    (type iterator-cache iter)
    (optimize))
  (the iterator-cache 
    (make-iterator-cache 
      :inext-function (iterator-inext-function iter)
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :index (iterator-cache-index iter)
      :collection (iterator-cache-collection iter)
      :iterator (iterator-cache-iterator iter))))

(defun icache (iter)
  (declare
    (optimize))
  (the iterator-cache 
    (make-iterator-cache 
      :inext-function #'inext-iterator-cache
      :iskip-function #'iskip-iterator-cache
      :icopy-function #'icopy-iterator-cache
      :collection (make-array 0 :fill-pointer 0 :adjustable t)
      :iterator (iterator iter))))

(defmethod iterator ((iter iterator-cache))
  (declare
    (type iterator-cache iter)
    (optimize))
  (the iterator-cache iter))

;; reverse 

(defstruct 
  (iterator-reverse 
    (:include iterator))
  (collection nil :type array)
  (index 0 :type fixnum)
  (iterator))

(defun init-iterator-reverse (iter)
  (declare
    (type iterator-reverse iter))
  (multiple-value-bind 
    (element eoi)
    (inext (iterator-reverse-iterator iter))
      (declare 
        (type boolean eoi)) ;; declaration for multiple value bind 
    (unless eoi 
      (vector-push-extend element 
        (iterator-reverse-collection iter))
      (init-iterator-reverse iter))))

(defun inext-iterator-reverse (iter)
  (declare
    (type iterator-reverse iter))
  (if (= 0 (length (iterator-reverse-collection iter)))
    (init-iterator-reverse iter))
  (if (< (iterator-reverse-index iter) (length (iterator-reverse-collection iter)))
    (multiple-value-prog1
      (values 
        (aref (iterator-reverse-collection iter)
          (- (length (iterator-reverse-collection iter)) 1 (iterator-reverse-index iter))) 
        nil)
      (incf (iterator-reverse-index iter)))
    (values nil t)))

(defun iskip-iterator-reverse (iter)
  (declare
    (type iterator-reverse iter))
  (if (= 0 (length (iterator-reverse-collection iter)))
    (init-iterator-reverse iter))
  (the boolean 
    (if (< (iterator-reverse-index iter) (length (iterator-reverse-collection iter)))
      (prog1 t (incf (iterator-reverse-index iter)))
      nil))) ;; when reached eoi 

(defun icopy-iterator-reverse (iter)
  (declare
    (type iterator-reverse iter)
    (optimize (speed 3)))
  (the iterator-reverse 
    (make-iterator-reverse 
      :inext-function (iterator-inext-function iter) 
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :collection (iterator-reverse-collection iter)
      :index (iterator-reverse-index iter)
      :iterator (iterator-reverse-iterator iter))))

(defun ireverse (iter)
  (declare
    (type iterator iter)
    (optimize (speed 3)))
  (the iterator-reverse
    (make-iterator-reverse 
      :inext-function #'inext-iterator-reverse 
      :iskip-function #'iskip-iterator-reverse 
      :icopy-function #'icopy-iterator-reverse 
      :collection (make-array 128 :fill-pointer 0 :adjustable t)
      :iterator (iterator iter))))

(defmethod iterator ((iter iterator-reverse))
  (declare
    (type iterator-reverse iter)
    (optimize (speed 3)))
  (the iterator-reverse iter))

;; sort 

(defstruct 
  (iterator-sort 
    (:include iterator))
  (collection nil :type array)
  (index 0 :type fixnum)
  (function nil :type function)
  (iterator nil :type iterator))

(defun init-iterator-sort-collect (iter)
  (declare
    (type iterator-sort iter))
  (multiple-value-bind 
    (element eoi)
    (inext (iterator-sort-iterator iter))
    (unless eoi
      (vector-push-extend element
        (iterator-sort-collection iter))
      (init-iterator-sort-collect iter))))

(defun init-iterator-sort (iter)
  (declare
    (type iterator-sort iter))
  (init-iterator-sort-collect iter)
  (sort (iterator-sort-collection iter)
    (iterator-sort-function iter)))

(defun inext-iterator-sort (iter)
  (declare
    (type iterator-sort iter))
  (if (= 0 (length (iterator-sort-collection iter)))
    (init-iterator-sort iter))
  (if (< (iterator-sort-index iter) (length (iterator-sort-collection iter)))
    (multiple-value-prog1
      (values (aref (iterator-sort-collection iter) (iterator-sort-index iter)) nil)
      (incf (iterator-sort-index iter)))
    (values nil t)))

(defun iskip-iterator-sort (iter)  
  (declare
    (type iterator-sort iter))
  (if (= 0 (length (iterator-sort-collection iter)))
    (init-iterator-sort iter))
  (the boolean
    (if (< (iterator-sort-index iter) (length (iterator-sort-collection iter)))
      (prog1 t
        (incf (iterator-sort-index iter)))
      nil)))

(defun icopy-iterator-sort (iter)
  (declare
    (type iterator-sort iter))
  (the iterator-sort
    (make-iterator-sort 
      :inext-function (iterator-inext-function iter)
      :iskip-function (iterator-iskip-function iter)
      :icopy-function (iterator-icopy-function iter)
      :collection (iterator-sort-collection iter)
      :function (iterator-sort-function iter)
      :iterator (iterator-sort-iterator iter))))

(defun isort (function iter)
  (declare
    (type function function))
  (the iterator-sort
    (make-iterator-sort 
      :inext-function #'inext-iterator-sort
      :iskip-function #'iskip-iterator-sort
      :icopy-function #'icopy-iterator-sort
      :collection (make-array 128 :fill-pointer 0 :adjustable t :element-type t)
      :function function 
      :iterator (iterator iter))))

(defmethod iterator ((iter iterator-sort))
  (declare
    (type iterator-sort iter))
  (the iterator-sort iter))

;; file 

(defstruct 
  (iterator-file
    (:include iterator))
  (file nil :type stream)
  (read-function nil :type function))

(defvar *iterator-file-eof*
  (make-symbol "*iterator-file-eof*"))

(defun inext-iterator-file (iter)
  (declare
    (type iterator-file iter))
  (let ((char (funcall (iterator-file-read-function iter) nil *iterator-file-eof*)))
    (if (eq char *iterator-file-eof*) 
      (values nil t)
      (values char nil))))

(defun iskip-iterator-file (iter)
  (declare
    (type iterator-file iter))
  (let ((char (funcall (iterator-file-read-function iter) nil *iterator-file-eof*)))
    (if (eq char *iterator-file-eof*) nil t)))

(defun icopy-iterator-file (iter)
  (declare
    (ignore iter)
    (type iterator-file iter))
  (error "cannot icopy an iterator-file, because it cannot icopy a file status."))

(defun ifile (file &optional (function #'read-char))
  (declare
    (type stream file))
  (the iterator-file 
    (make-iterator-file 
      :inext-function #'inext-iterator-file
      :iskip-function #'iskip-iterator-file 
      :icopy-function #'icopy-iterator-file 
      :read-function function
      :file file)))

(defmethod iterator ((iter iterator-file))
  (declare
    (type iterator-file iter))
  (the iterator-file iter))

;; macro 

(defmacro doiterator (argument &body body)
  (let
    ((variable (nth 0 argument))
      (formula (nth 1 argument))
      (result (nth 2 argument))
      (iter (gensym)))
    `(loop 
       with ,iter = ,formula
       with ,variable 
       with eoi do 
       (multiple-value-bind 
         (element-tmp eoi-tmp)
         (inext ,iter)
         (declare
           (type boolean eoi))
         (setq ,variable element-tmp)
         (setq eoi eoi-tmp))
       if eoi 
       return (progn ,@result)
       else do (progn ,@body))))

;; icount 

(defun icount-if-in (function count iter)
  (declare
    (type function function)
    (type fixnum count)
    (type iterator iter)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (declare
      (type boolean eoi))
    (if eoi count 
      (if (funcall function element)
        (icount-if-in function (1+ count) iter)
        (icount-if-in function count iter)))))

(defun icount-if (function iter)
  (declare
    (type function function)
    (type iterator iter)
    (optimize (speed 3)))
  (icount-if-in function 0 iter))

(defun icount (item iter)
  (declare
    (type iterator iter)
    (optimize (speed 3)))
  (icount-if 
    (lambda (element)
      (equal item element))
    (iterator iter)))

;; find 

(defun ifind-if-in (function iter)
  (declare
    (type function function)
    (type iterator iter)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (declare
      (type boolean eoi))
    (if eoi (values nil nil)
      (if (funcall function element)
        (values element t)
        (ifind-if-in function iter)))))

(defun ifind-if (function iter)
  (declare
    (type function function)
    (type iterator iter)
    (optimize (speed 3)))
  (ifind-if-in function (iterator iter)))

(defun ifind (item iter)
  (declare
    (type iterator iter)
    (optimize (speed 3)))
  (ifind-if 
    (lambda (element)
      (equal item element))
    (iterator iter)))

;; position 

(defun iposition-if-in (function count iter)
  (declare
    (type function function)
    (type fixnum count)
    (type iterator iter)
    (optimize (speed 3)))
  (multiple-value-bind
    (element eoi)
    (inext iter)
    (declare
      (type boolean eoi))
    (if eoi nil
      (if (funcall function element) count
        (iposition-if-in function (1+ count) iter)))))

(defun iposition-if (function iter)
  (declare
    (type function function)
    (type iterator iter)
    (optimize (speed 3)))
  (iposition-if-in function 0 (iterator iter)))

(defun iposition (item iter)
  (declare
    (type iterator iter)
    (optimize (speed 3)))
  (iposition-if 
    (lambda (element)
      (equal item element))
    (iterator iter)))

;; reduce 

(defun ireduce-in (function result iter)
  (declare
    (type iterator iter)
    (type function function)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (if eoi result
      (ireduce-in function
        (funcall function result element)
        iter))))

(defun ireduce (function iter)
  (declare 
    (type function function)
    (type iterator iter)
    (optimize (speed 3)))
  (multiple-value-bind
    (element eoi)
    (inext iter)
    (declare
      (type boolean eoi))
    (if eoi nil
      (multiple-value-bind
        (element2 eoi2)
        (inext iter)
        (if eoi2 element
          (ireduce-in function
            (funcall function element element2)
            iter))))))

;; ievery 

(defun ievery-in (function iter)
  (declare
    (type function function)
    (type iterator iter)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (declare
      (type boolean eoi))
    (if eoi t 
      (if (funcall function element)
        (ievery function iter)
        nil))))

(defun ievery (function iter)
  (declare
    (type function function)
    (type iterator iter)
    (optimize (speed 3)))
  (ievery-in function (iterator iter)))

;; isome 

(defun isome-in (function iter)
  (declare
    (type function function)
    (type iterator iter)
    (optimize (speed 3)))
  (multiple-value-bind
    (element eoi)
    (inext iter)
    (declare
      (type boolean eoi))
    (if eoi nil
      (if (funcall function element) t 
        (isome-in function iter)))))

(defun isome (function iter)
  (declare 
    (type function function)
    (type iterator iter)
    (optimize (speed 3)))
  (isome-in function (iterator iter)))
