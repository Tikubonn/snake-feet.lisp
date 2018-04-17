;; Copyright (c) 2018 tikubonn.
;; Released under the MIT license 
;; http://opensource.org/licenses/mitlicense.php 

;; package 

(provide 'snake-feet)

(defpackage snake-feet 
  (:use :common-lisp :snake-feet-builder)
  (:export :inext :iskip :icopy :to-list :to-array
    :iterator :ilist :iarray :ifunction :ilambda :irange :irepeat :ifile :icache
    :imap :ifilter :islice :istep :iappend :izip :ireverse :isort
    :iposition-if :iposition :ifind-if :ifind :icount-if :icount :isome :ievery
    :doiterator))

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

(define-condition iterator-coping-error (iterator-error)
  ()
  (:report
    (lambda (condition stream)
      (format stream "iterator-coping-error: ~a~%"
        (iterator-error-message condition)))))

;; iterator

(defclass iterator (t) nil)
(defmethod inext ((iter iterator)) nil)
(defmethod iskip ((iter iterator)) (inext iter))
(defmethod icopy (something) something)
(defmethod icopy ((iter iterator)))
(defmethod icopy-from ((iter iterator) (iter-from iterator)))

(defmethod iterator ((iter iterator))
  (declare
    (optimize (speed 3)))
  iter)

(defmethod iskip ((iter iterator))
  (declare
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (declare
      (type boolean eoi)
      (ignore element))
    (not eoi)))

;; to list 

(defun to-list-in (builder iter)
  (declare
    (type list-builder builder)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (declare
      (type boolean eoi)) ;; declaration for multiple value bind.
    (unless eoi 
      (push-last-list-builder element builder)
      (to-list-in builder iter))))

(defmethod to-list ((iter iterator))
  (declare
    (optimize (speed 3)))
  (let ((builder (make-list-builder)))
    (to-list-in builder iter)
    (result-list-builder builder)))

(defmethod to-list ((iter t))
  (to-list (iterator iter)))

;; to array

(defun to-array-in (builder iter)
  (declare
    (type list-builder builder)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (declare
      (type boolean eoi)) ;; declaration for multiple value bind.
    (unless eoi 
      (push-last-list-builder element builder)
      (to-array-in builder iter))))

(defmethod to-array ((iter iterator))
  (declare
    (optimize (speed 3)))
  (let ((builder (make-list-builder)))
    (to-array-in builder iter)
    (result-list-builder-as-array builder)))

(defmethod to-array ((iter t))
  (to-array (iterator iter)))

;; functional 

(defclass iterator-function (iterator)
  ((function
     :initarg :function
     :type function
     :accessor iterator-function)))

(defmethod inext ((iter iterator-function))
  (funcall (iterator-function iter)))

(defmethod icopy ((iter iterator-function))
  (error
    (make-instance 'iterator-coping-error 
      :message "iterator-function class cannot coping.")))

(defmethod icopy-from :after ((iter iterator-function) (iter-from iterator-function))
  (error
    (make-instance 'iterator-coping-error 
      :message "iterator-function class cannot coping.")))

(defmethod ifunction ((function function))
  (make-instance 'iterator-function :function function))

(defmethod iterator ((function function))
  (ifunction function))

(defmacro ilambda (arguments &body body)
  `(iterator (lambda ,arguments ,@body)))

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

(defun iterator-range-end? (iter)
  (cond 
    ((= (iterator-step iter) 0) nil)
    ((< (iterator-step iter) 0) (<= (iterator-current iter) (iterator-end iter)))
    ((> (iterator-step iter) 0) (>= (iterator-current iter) (iterator-end iter)))))

(defmethod inext ((iter iterator-range))
  (if (iterator-range-end? iter) (values nil t)
    (multiple-value-prog1 
      (values (iterator-current iter) nil)
      (incf (iterator-current iter)
        (iterator-step iter)))))

(defmethod iskip ((iter iterator-range))
  (the boolean
    (if (iterator-range-end? iter) nil ;; when reached eoi
      (prog1 t 
        (incf (iterator-current iter)
          (iterator-step iter))))))

(defmethod icopy ((iter iterator-range))
  (declare
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-range)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-range) (iter-from iterator-range))
  (declare
    (optimize (speed 3)))
  (setf (iterator-current iter) (iterator-current iter-from))
  (setf (iterator-start iter) (iterator-start iter-from))
  (setf (iterator-end iter) (iterator-end iter-from))
  (setf (iterator-step iter) (iterator-step iter-from)))

(defmethod irange (start &optional end  step)
  (declare
    (type integer start))
  (cond 
    (step (make-instance 'iterator-range :start start :end end :step step))
    (end (make-instance 'iterator-range :start start :end end :step (if (< start end) 1 -1)))
    (t (make-instance 'iterator-range :start 0 :end start :step (if (< 0 start) 1 -1)))))

;; repeat 

(defclass iterator-repeat (iterator)
  ((count
     :initarg :count
     :initform 0
     :type integer
     :accessor iterator-count)
    (element 
      :initarg :element
      :initform nil
      :accessor iterator-element)))

(defmethod inext ((iter iterator-repeat))
  (if (<= (iterator-count iter) 0) (values nil t)
    (multiple-value-prog1 
      (values (iterator-element iter) nil)
      (decf (iterator-count iter)))))

(defmethod iskip ((iter iterator-repeat))
  (the boolean
    (<= (iterator-count iter) 0)))

(defmethod icopy ((iter iterator-repeat))
  (declare
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-repeat)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-repeat) (iter-from iterator-repeat))
  (declare
    (optimize (speed 3)))
  (setf (iterator-count iter) (iterator-count iter-from))
  (setf (iterator-element iter) (iterator-element iter-from)))

(defmethod irepeat ((count integer) &optional element)
  (declare
    (type integer count)
    (optimize (speed 3)))
  (make-instance 'iterator-repeat :count count :element element))

;; list

(defclass iterator-list (iterator)
  ((cons
     :initarg :cons
     :initform nil
     :type list 
     :accessor iterator-cons)))

(defmethod inext ((iter iterator-list))
  (declare
    (optimize (speed 3)))
  (if (null (iterator-cons iter)) 
    (values nil t)
    (values (pop (iterator-cons iter)) nil)))

(defmethod iskip ((iter iterator-list))
  (declare 
    (optimize (speed 3)))
  (the boolean
    (not (null (iterator-cons iter)))))

(defmethod icopy ((iter iterator-list))
  (declare 
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-list)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-list) (iter-from iterator-list))
  (declare 
    (optimize (speed 3)))
  (setf (iterator-cons iter) (iterator-cons iter-from)))

(defun ilist (lis)
  (declare
    (type list lis)
    (optimize (speed 3)))
  (make-instance 'iterator-list :cons lis))

(defmethod iterator ((lis list))
  (declare 
    (type list lis)
    (optimize (speed 3)))
  (ilist lis))

;; array

(defclass iterator-array (iterator)
  ((array
     :initarg :array
     :accessor iterator-array)
    (index
      :initform 0
      :type integer
      :accessor iterator-index)))

(defmethod inext ((iter iterator-array))
  (if (< (iterator-index iter) (length (iterator-array iter)))
    (multiple-value-prog1
      (values (aref (iterator-array iter) (iterator-index iter)) nil)
      (incf (iterator-index iter)))
    (values nil t))) ;; when reached eoi 

(defmethod iskip ((iter iterator-array))
  (the boolean
    (< (iterator-index iter) 
      (length (iterator-array iter)))))

(defmethod icopy ((iter iterator-array))
  (declare
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-array)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-array) (iter-from iterator-array))
  (declare
    (optimize (speed 3)))  
  (setf (iterator-array iter) (iterator-array iter-from))
  (setf (iterator-index iter) (iterator-array iter-from)))

(defun iarray (arr)
  (declare
    (type array arr))
  (make-instance 'iterator-array :array arr))

(defmethod iterator ((arr array))
  (declare
    (type array arr))
  (iarray arr))

;; iterator 

(defclass iterator-iterator (iterator)
  ((iterator
     :initarg :iterator
     :initform nil
     :type iterator
     :accessor iterator-iterator)))

(defmethod inext ((iter iterator-iterator))
  (inext (iterator-iterator iter)))

(defmethod icopy ((iter iterator-iterator))
  (declare
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-iterator)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-iterator) (iter-from iterator-iterator))
  (declare
    (optimize (speed 3)))
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
     :type function 
     :accessor iterator-function)))

(defmethod inext ((iter iterator-map))
  (declare
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext (iterator-iterator iter))
    (declare
      (type boolean eoi)) ;; declaration for multiple value bind.
    (if eoi (values nil t)
      (values (funcall (the function (iterator-function iter)) element) nil))))

(defmethod icopy ((iter iterator-map))
  (declare
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-map)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-map) (iter-from iterator-map))
  (declare
    (optimize (speed 3)))  
  (setf (iterator-function iter) (iterator-function iter-from)))

(defmethod imap ((function function) iter)
  (declare
    (type function function)
    (optimize (speed 3)))
  (imap function (iterator iter)))

(defmethod imap ((function function) (iter iterator))
  (declare
    (type function function)
    (optimize (speed 3)))
  (make-instance 'iterator-map :function function :iterator iter))

;; filter

(defclass iterator-filter (iterator-iterator)
  ((function
     :initarg :function
     :type function
     :accessor iterator-function)))

(defmethod inext ((iter iterator-filter))
  (declare
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext (iterator-iterator iter))
    (declare
      (type boolean eoi)) ;; declaration for multiple value bind.
    (if eoi (values nil t)
      (if (funcall (the function (iterator-function iter)) element)
        (values element nil)
        (inext iter)))))

(defmethod icopy ((iter iterator-filter))
  (declare
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-filter)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-filter) (iter-from iterator-filter))
  (declare
    (optimize (speed 3)))
  (setf (iterator-function iter) (iterator-function iter-from)))

(defmethod ifilter ((function function) iter)
  (declare
    (optimize (speed 3)))
  (ifilter function (iterator iter)))

(defmethod ifilter ((function function) (iter iterator))
  (declare
    (optimize (speed 3)))
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
  (loop while (< (iterator-current iter) (iterator-start iter)) do 
    (iskip (iterator-iterator iter))
    (incf (iterator-current iter))))

(defmethod inext ((iter iterator-slice))
  (setup-iterator-slice iter)
  (if (< (iterator-current iter) (iterator-end iter))
    (multiple-value-bind 
      (element eoi)
      (inext (iterator-iterator iter))
      (declare
        (type boolean eoi)) ;; declaration for multiple value bind.
      (if eoi (values nil t)
        (multiple-value-prog1
          (values element nil)
          (incf (iterator-current iter)))))      
    (values nil t)))

(defmethod iskip ((iter iterator-slice))
  (setup-iterator-slice iter)
  (if (< (iterator-current iter) (iterator-end iter))
    (if (iskip iter)
      (prog1 t
        (incf (iterator-current iter)))
      nil)
    nil))

(defmethod icopy ((iter iterator-slice))
  (declare
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-slice)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-slice) (iter-from iterator-slice))
  (declare
    (optimize (speed 3)))
  (setf (iterator-current iter) (iterator-current iter-from))
  (setf (iterator-start iter) (iterator-start iter-from))
  (setf (iterator-end iter) (iterator-end iter-from)))

(defmethod islice (start end iter)
  (declare
    (type integer start end)
    (optimize (speed 3)))
  (islice start end (iterator iter)))

(defmethod islice (start end (iter iterator))
  (declare
    (type integer start end)
    (optimize (speed 3)))
  (make-instance 'iterator-slice :start start :end end :iterator iter))

;; append 

(defclass iterator-append (iterator)
  ((iterators 
     :initarg :iterators
     :initform nil
     :type list 
     :accessor iterator-iterators)))

(defmethod inext ((iter iterator-append))
  (declare
    (optimize (speed 3)))
  (if (null (iterator-iterators iter)) (values nil t)
    (multiple-value-bind 
      (element eoi)
      (inext (car (iterator-iterators iter)))
      (declare
        (type boolean eoi)) ;; declaration for multiple value bind.
      (if eoi 
        (progn 
          (pop (iterator-iterators iter))
          (inext iter))
        (values element nil)))))

(defmethod iskip ((iter iterator-append))
  (declare
    (optimize (speed 3)))
  (if (null (iterator-iterators iter)) nil
    (if (iskip (car (iterator-iterators iter))) t
      (progn 
        (setf (iterator-iterators iter)
          (cdr (iterator-iterators iter)))
        (iskip iter)))))

(defmethod icopy ((iter iterator-append))
  (declare
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-append)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-append) (iter-from iterator-append))
  (declare
    (optimize (speed 3)))
  (setf (iterator-iterators iter) (mapcar 'icopy (iterator-iterators iter-from))))

(defmethod iappend (&rest iters)
  (declare
    (type list iters)
    (optimize (speed 3)))
  (make-instance 'iterator-append :iterators (mapcar 'iterator iters)))

;; zip 

(defclass iterator-zip (iterator)
  ((iterators
     :initarg :iterators
     :initform nil
     :type list 
     :accessor iterator-iterators)))

(defun inext-iterator-zip-in (sequence iters)
  (declare
    (type list sequence)
    (type list iters)
    (optimize (speed 3)))
  (if (null iters) (values sequence t)
    (multiple-value-bind 
      (element eoi)
      (inext (car iters))
      (if eoi (values nil nil)
        (inext-iterator-zip-in
          (cons element sequence)
          (cdr iters))))))

(defmethod inext ((iter iterator-zip))
  (declare
    (optimize (speed 3)))
  (multiple-value-bind
    (sequence success)
    (inext-iterator-zip-in nil (iterator-iterators iter))
    (if success 
      (values sequence nil)
      (values nil t))))

(defun iskip-iterator-zip-in (iters)
  (declare
    (optimize (speed 3)))
  (if (null iters) t
    (if (iskip iters)
      (iskip-iterator-zip-in (cdr iters))
      nil)))

(defmethod iskip ((iter iterator-zip))
  (declare
    (optimize (speed 3)))
  (iskip-iterator-zip-in 
    (iterator-iterators iter)))

(defmethod icopy ((iter iterator-zip))
  (declare
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-zip)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-zip) (iter-from iterator-zip))
  (declare
    (optimize (speed 3)))
  (setf (iterator-iterators iter) (mapcar 'icopy (iterator-iterators iter-from))))

(defmethod izip (&rest iterators)
  (declare
    (type list iterators)
    (optimize (speed 3)))
  (make-instance 'iterator-zip :iterators (mapcar 'iterator iterators)))

;; step 

(defclass iterator-step (iterator-iterator)
  ((current 
     :initform 0
     :type integer
     :accessor iterator-current)
    (offset 
      :initarg :offset
      :initform 0
      :type integer
      :accessor iterator-offset)
    (step 
      :initarg :step
      :initform 1
      :type integer
      :accessor iterator-step)))

(defun init-iterator-step (iter)
  (loop while (< (iterator-current iter) (iterator-offset iter)) do
    (iskip (iterator-iterator iter))
    (incf (iterator-current iter))))

(defmethod inext ((iter iterator-step))
  (init-iterator-step iter)
  (multiple-value-prog1 
    (inext (iterator-iterator iter))
    (loop repeat (1- (iterator-step iter)) do
      (iskip (iterator-iterator iter))
      (incf (iterator-current iter)))))

(defmethod iskip ((iter iterator-step))
  (init-iterator-step iter)
  (prog1 (iskip (iterator-iterator iter))
    (loop repeat (1- (iterator-step iter)) do
      (iskip (iterator-iterator iter))
      (incf (iterator-current iter)))))
 
(defmethod icopy ((iter iterator-step))
  (declare
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-iskip)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-step) (iter-from iterator-step))
  (declare
    (optimize (speed 3)))
  (setf (iterator-current iter) (iterator-current iter-from))
  (setf (iterator-offset iter) (iterator-offset iter-from))
  (setf (iterator-step iter) (iterator-step iter-from)))

(defmethod istep ((offset integer) (step integer) iter)
  (declare
    (type integer offset step)
    (optimize (speed  3)))
  (istep (iterator iter)))

(defmethod istep ((offset integer) (step integer) (iter iterator))
  (declare
    (type integer offset step)
    (optimize (speed 3)))
  (make-instance 'iterator-step :offset offset :step step :iterator iter))

;; pool 

(defclass iterator-pool (iterator-iterator)
  ((collection
     :initarg :collection
     :accessor iterator-collection)))

(defun load-from-iterator-in (iter)
  (multiple-value-bind 
    (element eoi)
    (inext (iterator-iterator iter))
    (declare
      (type boolean eoi)) ;; declaration for multiple value bind.
    (unless eoi 
      (vector-push-extend element 
        (iterator-collection iter))
      (load-from-iterator-in iter))))

(defmethod load-from-iterator ((iter iterator-pool))
  (load-from-iterator-in iter))

(defmethod did-load-from-iterator? ((iter iterator-pool))
  (the boolean 
    (< 0 (length (iterator-collection iter)))))

;; reverse (slow)

(defclass iterator-reverse (iterator-pool)
  ((index
     :initform 0
     :type integer
     :accessor iterator-index)))

(defmethod inext ((iter iterator-reverse))
  (unless (did-load-from-iterator? iter)
    (load-from-iterator iter))
  (if (< (iterator-index iter) (length (iterator-collection iter)))
    (multiple-value-prog1 
      (values 
        (aref (iterator-collection iter) 
          (- (length (iterator-collection iter)) 1 
            (iterator-index iter))) nil)
      (incf (iterator-index iter)))
    (values nil t)))

(defmethod icopy ((iter iterator-reverse))
  (declare 
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-reverse)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-reverse) (iter-from iterator-reverse))
  (declare
    (optimize (speed 3)))
  (setf (iterator-index iter) (iterator-index iter-from)))

(defmethod ireverse (iter)
  (declare
    (optimize (speed 3)))
  (ireverse (iterator iter)))

(defmethod ireverse ((iter iterator))
  (declare
    (optimize (speed 3)))
  (make-instance 'iterator-reverse 
    :collection (make-array 0 :fill-pointer 0 :adjustable t)
    :iterator iter))

;; sort (slow)

(defclass iterator-sort (iterator-pool)
  ((function 
     :initarg :function
     :type function
     :accessor iterator-function)
    (index
      :initform 0
      :type integer
      :accessor iterator-index)))

(defmethod load-from-iterator :after ((iter iterator-sort))
  (setf (iterator-collection iter)
    (sort (iterator-collection iter) 
      (iterator-function iter))))

(defmethod inext ((iter iterator-sort))
  (unless (did-load-from-iterator? iter)
    (load-from-iterator iter))
  (if (< (iterator-index iter) (length (iterator-collection iter)))
    (multiple-value-prog1
      (values (aref (iterator-collection iter) (iterator-index iter)) nil)
      (incf (iterator-index iter)))
    (values nil t)))

(defmethod icopy ((iter iterator-sort))
  (declare
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-sort)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-sort) (iter-from iterator-sort))
  (declare
    (optimize (speed 3)))
  (setf (iterator-index iter) (iterator-index iter-from)))

(defmethod isort (function iter)
  (declare
    (optimize (speed 3)))
  (isort function (iterator iter)))

(defmethod isort (function (iter iterator))
  (declare
    (optimize (speed 3)))
  (make-instance 'iterator-sort 
    :collection (make-array 0 :fill-pointer 0 :adjustable t)
    :function function
    :iterator iter))

;; cache 

(defclass iterator-cache (iterator) ;; iterator-iterator will copy a source iterator when coping
  ((iterator 
     :initarg :iterator
     :type iterator
     :accessor iterator-iterator)
    (collection
      :initarg :collection
      :type array
      :accessor iterator-collection)
    (index
      :initform 0
      :type integer
      :accessor iterator-index)))

(defmethod inext ((iter iterator-cache))
  (cond 
    ((< (iterator-index iter) (length (iterator-collection iter)))
      (multiple-value-prog1
        (values (aref (iterator-collection iter) (iterator-index iter)) nil)
        (incf (iterator-index iter))))
    (t 
      (multiple-value-bind 
        (element eoi)
        (inext (iterator-iterator iter))
        (if eoi (values nil t)
          (multiple-value-prog1
            (values element nil)
            (vector-push-extend element
              (iterator-collection iter))
            (incf (iterator-index iter))))))))

(defmethod icopy ((iter iterator-cache))
  (declare 
    (optimize (speed 3)))
  (let ((niter (make-instance 'iterator-cache)))
    (prog1 niter (icopy-from niter iter))))

(defmethod icopy-from :after ((iter iterator-cache) (iter-from iterator-cache))
  (declare
    (optimize (speed 3)))
  (setf (iterator-iterator iter) (iterator-iterator iter-from))
  (setf (iterator-collection iter) (iterator-collection iter-from))
  (setf (iterator-index iter) (iterator-index iter-from)))

(defmethod icache (iter)
  (declare
    (optimize (speed 3)))
  (icache (iterator iter)))

(defmethod icache ((iter iterator))
  (declare
    (optimize (speed 3)))
  (make-instance 'iterator-cache 
    :collection (make-array 0 :fill-pointer 0 :adjustable t)
    :iterator iter))

;; file 

(defvar *iterator-file-eof* (make-symbol "iterator-file-eof"))

(defclass iterator-file (iterator)
  ((file 
     :initarg :file
     :accessor iterator-file)
    (read-function
      :initarg :read-function
      :initform 'read-char
      :type function 
      :accessor iterator-read-function)))

(defmethod inext ((iter iterator-file))
  (declare
    (optimize (speed 3)))
  (let ((element (funcall (the function (iterator-read-function iter)) (iterator-file iter) nil *iterator-file-eof*)))
    (if (eq element *iterator-file-eof*)
      (values nil t)
      (values element nil))))

(defmethod icopy ((iter iterator-file))
  (declare
    (ignore iter)
    (optimize (speed 3)))
  (error 
    (make-instance 'iterator-coping-error 
      :message "iterator-file class cannot coping.")))

(defmethod icopy-from :after ((iter iterator-file) (iter-from iterator-file))
  (declare
    (ignore iter iter-from)
    (optimize (speed 3)))
  (error
    (make-instance 'iterator-coping-error
      :message "iterator-file class cannot coping.")))

(defmethod ifile (file &key (read-function 'read-char))
  (declare
    (optimize (speed 3)))
  (make-instance 'iterator-file :file file :read-function read-function))

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

;; every 

(defmethod ievery ((function function) iter)
  (declare
    (type function function)
    (optimize (speed 3)))
  (ievery function (iterator iter)))

(defmethod ievery ((function function) (iter iterator))
  (declare
    (type function function)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (if eoi t
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
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (if eoi nil 
      (if (funcall function element) t
        (isome function iter)))))

;; reduce

(defun ireduce-in (function result iter)
  (declare 
    (type function function)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (declare
      (type boolean eoi)) ;; declaration for multiple value bind.
    (if eoi result 
      (ireduce-in function
        (funcall function result element) iter))))

(defmethod ireduce ((function function) iter)
  (declare 
    (type function function)
    (optimize (speed 3)))
  (ireduce function (iterator iter)))

(defmethod ireduce ((function function) (iter iterator))
  (declare
    (type function function)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (declare
      (type boolean eoi)) ;; declaration for multiple value bind.
    (if eoi nil 
      (multiple-value-bind
        (element2 eoi2)
        (inext iter)
        (declare
          (type boolean eoi)) ;; declaration for multiple value bind.
        (if eoi2 element
          (ireduce-in function 
            (funcall function element element2) iter))))))

;; findif 

(defmethod ifind-if ((function function) iter)
  (declare
    (type function function)
    (optimize (speed 3)))
  (ifind-if function (iterator iter)))

(defmethod ifind-if ((function function) (iter iterator))
  (declare
    (type function function)
    (optimize (speed 3)))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (if eoi (values nil nil)
      (if (funcall function element)
        (values element t)
        (ifind-if function iter)))))

(defmethod ifind (item iter)
  (declare
    (optimize (speed 3)))
  (ifind item (iterator iter)))

(defmethod ifind (item (iter iterator))
  (declare
    (optimize (speed 3)))
  (ifind-if 
    (lambda (element)
      (equal element  item))
    iter))

;; position if 

(defun iposition-if-in (function count iter)
  (declare
    (type function function)
    (type integer count))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (if eoi nil
      (if (funcall function element) count 
        (iposition-if-in function (1+ count) iter)))))

(defmethod iposition-if ((function function) iter)
  (declare
    (type function function)
    (optimize (speed 3)))
  (iposition-if function (iterator iter)))

(defmethod iposition-if (function (iter iterator))
  (declare
    (type function function)
    (optimize (speed 3)))
  (iposition-if-in function 0 iter))

(defmethod iposition (item iter)
  (declare
    (optimize (speed 3)))
  (iposition-if item iter))

(defmethod iposition (item (iter iterator))
  (declare
    (optimize (speed 3)))
  (iposition-if 
    (lambda (element)
      (equal element iter))
    iter))

;; icount 

(defun icount-if-in (function count iter)
  (declare
    (type function function)
    (type integer count))
  (multiple-value-bind 
    (element eoi)
    (inext iter)
    (if eoi count 
      (if (funcall function element)
        (icount-if-in function (1+ count) iter)
        (icount-if-in function count iter)))))

(defmethod icount-if ((function function) iter)
  (declare
    (type function function)
    (optimize (speed 3)))
  (icount-if function (iterator iter)))

(defmethod icount-if ((function function) (iter iterator))
  (declare
    (type function function)
    (optimize (speed 3)))
  (icount-if-in function 0 iter))

(defmethod icount (item iter)
  (declare
    (optimize (speed 3)))
  (icount item (iterator iter)))

(defmethod icount (item (iter iterator))
  (declare
    (optimize (speed 3)))
  (icount-if 
    (lambda (element)
      (equal item element))
    iter))

