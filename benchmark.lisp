
(require :asdf)
(asdf:load-system :snake-feet)
(asdf:load-system :mamba-feet)

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
  (mamba-feet:doiterator 
    (element 
      (mamba-feet:islice 100 900
        (mamba-feet:imap #'1+
          (mamba-feet:imap #'1+
            (mamba-feet:imap #'1+
              (mamba-feet:iterator lis))))))))

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
  ;;   snake-feet:inext snake-feet:iskip snake-feet:icopy snake-feet:to-list snake-feet:to-array
  ;;   snake-feet:iterator snake-feet:ilist snake-feet:iarray snake-feet:ifunction snake-feet:ilambda snake-feet:irange snake-feet:irepeat snake-feet:ifile snake-feet:icache
  ;;   snake-feet:imap snake-feet:ifilter snake-feet:islice snake-feet:istep snake-feet:iappend snake-feet:izip snake-feet:ireverse snake-feet:isort
  ;;   snake-feet:iposition-if snake-feet:iposition snake-feet:ifind-if snake-feet:ifind snake-feet:icount-if snake-feet:icount snake-feet:isome snake-feet:ievery
  ;;   snake-feet:doiterator
  ;;   mamba-feet:inext mamba-feet:iskip mamba-feet:icopy mamba-feet:to-list mamba-feet:to-array
  ;;   mamba-feet:iterator mamba-feet:ilist mamba-feet:iarray mamba-feet:ifunction mamba-feet:ilambda mamba-feet:irange mamba-feet:irepeat mamba-feet:ifile mamba-feet:icache
  ;;   mamba-feet:imap mamba-feet:ifilter mamba-feet:islice mamba-feet:istep mamba-feet:iappend mamba-feet:izip mamba-feet:ireverse mamba-feet:isort
  ;;   mamba-feet:iposition-if mamba-feet:iposition mamba-feet:ifind-if mamba-feet:ifind mamba-feet:icount-if mamba-feet:icount mamba-feet:isome mamba-feet:ievery
  ;;   mamba-feet:doiterator)
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
