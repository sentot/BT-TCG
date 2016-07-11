
(in-package "bt")

(process-bt-file "examples/SSM160224b.btc")

(reachable-states)

(defvar *blocks* (indices-of-update-blocks "SSM"))

(defvar *stopped* (indices-of-update-blocks "SSM" "Stopped"))
(defvar *unknown* (indices-of-update-blocks "SSM" "Unknown"))
(defvar *ok* (indices-of-update-blocks "SSM" "Ok"))
(defvar *failedst* (indices-of-update-blocks "SSM" "FailedST"))
(defvar *failedsensors* (indices-of-update-blocks "SSM" "FailedSensors"))

(defvar *all-noi*
        (remove-duplicates
          (append (indices-of-event-blocks "TimerST" "timeout")
                  (indices-of-event-blocks "Timer" "timeout(s1)")
                  (indices-of-event-blocks "Timer" "timeout(s2)")
	   )))

(defun generate-test-paths-new (sources targets intermediates blocks)
  (shortest-paths-for-each-combination intermediates
  (loop for source in sources
        append
        (loop for target in targets
              append
	       (find-test-paths source intermediates target blocks)))))

(defun generate-and-print-test-paths-new (intermediates)
  (cond ((null intermediates)
         (format t "~%Generating test paths"))
        (t
         (format t "~%Generating test paths with NOIs:~%")))
  (loop for i in intermediates
        do (print-block-summary i))
  (format t "~%========== Paths from SSM stopped to SSM stopped:~%")
  (let ((result (generate-test-paths-new
                  *stopped* *stopped* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM stopped to SSM unknown:~%")
  (let ((result (generate-test-paths-new
                  *stopped* *unknown* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM stopped to SSM ok:~%")
  (let ((result (generate-test-paths-new
                  *stopped* *ok* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM stopped to SSM failedst:~%")
  (let ((result (generate-test-paths-new
                  *stopped* *failedst* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM stopped to SSM failedsensors:~%")
  (let ((result (generate-test-paths-new
                  *stopped* *failedsensors* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM unknown to SSM stopped:~%")
  (let ((result (generate-test-paths-new
                  *unknown* *stopped* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM unknown to SSM unknown:~%")
  (let ((result (generate-test-paths-new
                  *unknown* *unknown* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM unknown to SSM ok:~%")
  (let ((result (generate-test-paths-new
                  *unknown* *ok* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM unknown to SSM failedst:~%")
  (let ((result (generate-test-paths-new
                  *unknown* *failedst* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM unknown to SSM failedsensors:~%")
  (let ((result (generate-test-paths-new
                  *unknown* *failedsensors* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM ok to SSM stopped:~%")
  (let ((result (generate-test-paths-new
                  *ok* *stopped* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM ok to SSM unknown:~%")
  (let ((result (generate-test-paths-new
                  *ok* *unknown* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM ok to SSM ok:~%")
  (let ((result (generate-test-paths-new
                  *ok* *ok* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM ok to SSM failedst:~%")
  (let ((result (generate-test-paths-new
                  *ok* *failedst* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM ok to SSM failedsensors:~%")
  (let ((result (generate-test-paths-new
                  *ok* *failedsensors* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM failedst to SSM stopped:~%")
  (let ((result (generate-test-paths-new
                  *failedst* *stopped* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM failedst to SSM unknown:~%")
  (let ((result (generate-test-paths-new
                  *failedst* *unknown* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM failedst to SSM ok:~%")
  (let ((result (generate-test-paths-new
                  *failedst* *ok* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM failedst to SSM failedst:~%")
  (let ((result (generate-test-paths-new
                  *failedst* *failedst* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM failedst to SSM failedsensors:~%")
  (let ((result (generate-test-paths-new
                  *failedst* *failedsensors* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM failedsensors to SSM stopped:~%")
  (let ((result (generate-test-paths-new
                  *failedsensors* *stopped* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM failedsensors to SSM unknown:~%")
  (let ((result (generate-test-paths-new
                  *failedsensors* *unknown* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM failedsensors to SSM ok:~%")
  (let ((result (generate-test-paths-new
                  *failedsensors* *ok* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM failedsensors to SSM failedst:~%")
  (let ((result (generate-test-paths-new
                  *failedsensors* *failedst* intermediates *blocks*)))
    (print-test-paths result))
  (format t "~%========== Paths from SSM failedsensors to SSM failedsensors:~%")
  (let ((result (generate-test-paths-new
                  *failedsensors* *failedsensors* intermediates *blocks*)))
    (print-test-paths result)))

(defun run-test (intermediates)
  (let ((noi (loop for i in intermediates
                   when (not (member-equal i *blocks*))
                   collect i)))
    (time (generate-and-print-test-paths-new noi))))

(defun generate-and-print-cyclic-test-paths (intermediates)
  (cond ((null intermediates)
         (format t "~%Generating cyclic test paths"))
        (t
         (format t "~%Generating cyclic test paths with NOIs:~%")))
  (let ((result (find-test-paths-with-cycles intermediates *blocks*)))
    (print-test-paths
     (append (shortest-paths-for-each-combination
	      intermediates
	      (loop for p in result
		    when (member-equal (first p) *failedst*)
		    collect p))
	     (shortest-paths-for-each-combination
	      intermediates
	      (loop for p in result
		    when (member-equal (first p) *failedsensors*)
		    collect p))))))

(defun permutations (sequence)
  (cond ((null sequence) (list nil))
	(t
	 (loop for el in sequence
	       append
	       (loop for seq in (permutations (remove-equal el sequence))
		     collect (cons el seq))))))
		      
(defun noi-combinations (intermediates)
  (unless (null intermediates)
    (append (permutations intermediates)
	    (loop for node in intermediates
		  append (noi-combinations
			  (remove-equal node intermediates))))))

(defun generate-and-print-noi-test-paths (intermediates)
  (format t "~%Generating NOI test paths~%")
  (let ((result (loop for source in *blocks*
		      append
		      (loop for noi in (noi-combinations intermediates)
			    append
			    (find-test-prefixes source noi *blocks*)))))
    (print-test-paths
     (shortest-paths-for-each-combination
      intermediates
      (remove-duplicate-reversion-paths result)))))

(defun run-cycle-test (intermediates)
  (let ((noi (loop for i in intermediates
                   when (not (member-equal i *blocks*))
                   collect i)))
    (time (generate-and-print-cyclic-test-paths noi))))

(defun run-noi-test (intermediates)
  (time (generate-and-print-noi-test-paths intermediates)))

  

(run-test *all-noi*)

(run-cycle-test *all-noi*)

(run-noi-test *all-noi*)
