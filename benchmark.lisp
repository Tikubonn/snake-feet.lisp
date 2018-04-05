
(load "snake-feet.lisp")
(load "snake-feet-mamba.lisp")

(defvar lis 
  (loop for n from 0 below 1000 collect n))

(defun test ()
  (snake-feet:doiterator 
    (element 
      (snake-feet:islice 100 900 
        (snake-feet:imap #'1+
          (snake-feet:imap #'1+
            (snake-feet:imap #'1+
              (snake-feet:iterator lis))))))))

(defun test2 ()
  (snake-feet-mamba:doiterator 
    (element 
      (snake-feet-mamba:islice 100 900
        (snake-feet-mamba:imap #'1+
          (snake-feet-mamba:imap #'1+
            (snake-feet-mamba:imap #'1+
              (snake-feet-mamba:iterator lis))))))))

(defun test3 ()
  (subseq
    (mapcar #'1+
      (mapcar #'1+
        (mapcar #'1+
          lis)))
    100 900))

(defun test4 ()
  (subseq
    (mapcar 
      (lambda (num)
        (1+ 
          (1+ 
            (1+ num))))
      lis)
    100 900))

(defun benchmark ()
  ;; (sb-profile:profile
  ;;   snake-feet:next snake-feet:skip snake-feet:copy
  ;;   snake-feet:ireduce snake-feet:icount-if snake-feet:icount
  ;;   snake-feet:iposition-if snake-feet:iposition snake-feet:ifind-if
  ;;   snake-feet:ifind snake-feet:ievery snake-feet:isome)
  (time
    (loop repeat 1000 do (test)))
  (time
    (loop repeat 1000 do (test2)))
  (time 
    (loop repeat 1000 do (test3)))
  (time 
    (loop repeat 1000 do (test4)))
  ;; (sb-profile:report)
  )

(benchmark)
