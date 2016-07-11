
(in-package "bt")

(process-bt-file "examples/AGerman160308.btc")

(reachable-states)

(defvar *blocks* (indices-of-update-blocks "Cache"))

(defun action-events (action-item)
  (remove-duplicates (indices-of-event-blocks "Action" action-item)))

(defun all-action-events ()
  (remove-duplicates (indices-of-event-blocks "Action")))

(defvar *all-actions* (remove-duplicates (all-action-events)))

(defvar *all-noi*
  (remove-duplicates
   (append (action-events "SendReqS(1)")
	   (action-events "SendReqS(2)")
	   (action-events "SendReqE(1)")
	   (action-events "SendReqE(2)")
	   (action-events "SendInvAck(1)")
	   (action-events "SendInvAck(2)")
	   (action-events "RecvGntS(1)")
	   (action-events "RecvGntS(2)")
	   (action-events "RecvGntE(1)")
	   (action-events "RecvGntE(2)"))))

(defvar *latest-test-noi*
  (remove-duplicates
   (append (action-events "SendReqS(1)")
	   (action-events "SendReqE(1)")
	   (action-events "RecvReqS(1)")
	   (action-events "RecvReqE(1)")
	   (action-events "SendInv(1)")
	   (action-events "SendInvAck(1)")
	   (action-events "RecvInvAck(1)")
	   (action-events "SendGntS(1)")
	   (action-events "SendGntE(1)")
	   (action-events "RecvGntS(1)")
	   (action-events "RecvGntE(1)"))))

(defvar *have-paths* nil)

(defun print-test-paths-summaries (paths)
  (format t "~%(~S paths)~%" (length paths))
  (loop for path in paths
	unless (check-test-path path)
	do (format t "~%Invalid path:~%~S~%" path))
  ;;(loop for path in paths
        ;;do
        ;;(cond ((check-test-path path)
               ;;(format t "~%Valid path:~%")
	       ;;(loop for i in path
		;;     do (print-block-label i))
	       ;;(format t "~%")
               ;;(print-test-path-summary path)
	       ;;)
              ;;(t
              ;; (format t "~%Invalid path:~%")
	      ;; (loop for i in path
		     ;;do (print-block-label i))
	       ;;(format t "~%")
               ;;(print-test-path-summary path)
  ;;)))
  )

(defun print-block-summary-update-node (index component)
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (cond ((and (>= index 0) (<= index (length bt-block-array)))
	     (loop for node in (bt-block-bt-nodes (aref bt-block-array index))
		   when (and (eq (bt-node-type node) 'update)
			     (equal (bt-node-component node) component))
		   do (print-bt-node-summary node)))
            ((bt-block-p index)
	     (loop for node in (bt-block-bt-nodes index)
		   when (and (eq (bt-node-type node) 'update)
			     (equal (bt-node-component node) component))
		   do (print-bt-node-summary node)))))))

(defun generate-simple-test-paths (sources targets blocks)
  (loop for source in sources
        append
        (loop for target in targets
              append
	      (let ((result (find-test-paths source nil target blocks)))
		(unless (null result)
		  (push (list source target) *have-paths*))
		result))))

(defun generate-and-print-simple-test-paths ()
  (format t "~%Generating simple test paths:~%")
  (loop for source in *blocks*
	do
	(loop for target in *blocks*
	      do
	      (let ((paths (generate-simple-test-paths
			    (list source) (list target) *blocks*)))
		(unless (null paths)
		(format t "~%========== There is a path from~%")
		(print-block-summary-update-node source "Cache")
		(format t " to~%")
		(print-block-summary-update-node target "Cache")
		(format t "~%")
		(print-test-paths-summaries paths))))))

(defun generate-test-paths-new (sources targets intermediates blocks)
  (loop for source in sources
        append
        (loop for target in targets
	      when (member-equal (list source target) *have-paths*)
              append
              (find-test-paths source intermediates target blocks))))

(defun generate-and-print-test-paths-new (intermediates)
  (cond ((null intermediates)
         (format t "~%Generating test paths"))
        (t
         (format t "~%Generating test paths with NOIs:~%")))
  (loop for i in intermediates
        do (print-block-summary i))
  (loop for source in *blocks*
	do
	(loop for target in *blocks*
	      do
	      (let ((paths (generate-test-paths-new
			    (list source) (list target) intermediates
			    *blocks*)))
		(unless (null paths)
		(format t "~%========== Paths from~%")
		(print-block-summary-update-node source "Cache")
		(format t " to~%")
		(print-block-summary-update-node target "Cache")
		(format t "~%")
		(print-test-paths-summaries
		 (shortest-paths-for-each-combination
		  intermediates paths)))))))

(defun run-simple-test ()
  (time (generate-and-print-simple-test-paths)))

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
    (print-test-paths result)))

(defun generate-and-print-cyclic-test-paths (intermediates)
  (cond ((null intermediates)
         (format t "~%Generating cyclic test paths"))
        (t
         (format t "~%Generating test paths with NOIs:~%")))
  (let ((result (find-test-paths-with-cycles intermediates *blocks*)))
    (print-test-paths-summaries result)))
  
(defun run-cycle-test (intermediates)
  (let ((noi (loop for i in intermediates
                   when (not (member-equal i *blocks*))
                   collect i)))
    (time (generate-and-print-cyclic-test-paths noi))))

(defun generate-and-print-test-latest (intermediates)
  (loop for i in intermediates
	do
	(progn
	  (format t "~%==================== NOI:~%")
	  (print-block-summary i)
	  (format t "~%")
	  (loop for source in *blocks*
		do
		(loop for target in *blocks*
		      do
		      (let ((paths (generate-test-paths-new
				    (list source) (list target) (list i)
				    *blocks*)))
			(unless (or (null paths)
				    (and (= (length paths) 1)
					 (not (member-equal i (car paths)))))
			  (format t "~%========== Paths from~%")
			  (print-block-summary-update-node source "Cache")
			  (format t " to~%")
			  (print-block-summary-update-node target "Cache")
			  (print-test-paths-summaries
			   (shortest-paths-for-each-combination
			    (list i) paths)))))))))

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


(defun run-noi-test (intermediates)
  (time
   (loop for i in intermediates
	 do
	 (generate-and-print-noi-test-paths (list i)))))

(defun run-rule-coverage-test (intermediates)
  (time (generate-and-print-test-latest intermediates)))

(run-simple-test)

(run-rule-coverage-test *latest-test-noi*)
