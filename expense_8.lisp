#!/usr/bin/env -S sbcl --control-stack-size 100MB --script

;(require :sb-sprof)

(declaim (optimize speed))

(load "queue.lisp")
(load "priority-queue.lisp")

;; STATISTICS
(defvar *max-fringe-size* 0)
(defvar *nodes-expanded* 0)
(defvar *nodes-generated* 0)
(defvar *nodes-popped* 0)

;; Value used for infinity
(defconstant *INF* 1000000000)

; Define the grid size
(defconstant ROWS 3)
(defconstant COLS 3)

;;===========GLOBAL VARIABLES===========
(defvar *start-state*)
(defvar *goal-state*)

;;=============STRUCTURES===============
(defstruct (state (:copier nil))
    (grid (make-array (list ROWS COLS) :initial-element 0))
    (zero-pos nil))


;;=============FUNCTIONS===============

(defun copy-state (state &key (zero-pos (state-zero-pos state)))
    (let ((copy (make-state 
        :grid (make-array (list ROWS COLS))
        :zero-pos :zero-pos)))

    (dotimes (i (* ROWS COLS))
        (setf (row-major-aref (state-grid copy) i)
            (row-major-aref (state-grid state) i))) 
    copy))

; A node is just a plist. who needs structures huh ;)
(defmacro new-node (&rest body)
    `(list ,@body))

(defun state-from-file (filename &aux start line num
    (state (make-state)))
    (with-open-file (stream filename :if-does-not-exist :error)        
        (dotimes (row ROWS) 
            (setq start 0) 
            (setq line (read-line stream))
            (dotimes (col COLS) 
                (setf (values num start) (parse-integer line :start start :junk-allowed t))
                (setf (aref (state-grid state) row col) num) 

                (when (= num 0) (setf (state-zero-pos state) `(,row ,col)))
            ))
    ) state)

(defun move-zero (state dx dy)
  (let* ((grid (state-grid state))
         (zero-pos (state-zero-pos state))
         (zero-row (first zero-pos))
         (zero-col (second zero-pos))
         (new-row (+ zero-row dy)) 
         (new-col (+ zero-col dx)))
    
    (when (and (>= new-row 0) (< new-row ROWS) (>= new-col 0) (< new-col COLS))
        (let ((culprit (aref grid new-row new-col)))
            (let ((copy (copy-state state)))

                (setf (aref (state-grid copy) zero-row zero-col) culprit)
                (setf (aref (state-grid copy) new-row new-col) '0)
                (setf (state-zero-pos copy) (list new-row new-col))

                (list :action (format nil "Move ~d ~a" culprit 
                    (if (= dx 0) 
                        (if (> dy 0) "up" "down") 
                        (if (> dx 0) "left" "right")))
                :state copy
                :step-cost culprit))))))

;; Returns a list of 3-element tuples (action state step-cost). 
;; A more traditional function accepting initial-state, end-state and the action
;; and returns the step-cost is provided below this
(defun successors (state &aux (ans ()))
    (dolist (dir '((0 -1) (0 1) (-1 0) (1 0))) ; LEFT, RIGHT, UP, DOWN
        (let ((successor (move-zero state (first dir) (second dir))))
            (when successor (push successor ans))))
    ans)

;; NOT USED
(defun step-cost (start end action)
    ;; For this problem, I only need the action to know the cost
    (parse-integer action :junk-allowed t))

(defvar *count* 0)
(defun goalp (state) (equalp (state-grid state) (state-grid *goal-state*)))

(defun print-state (state &optional stream )
    (when stream (format stream "~a" (state-grid state))))

(defun print-node1 (node &optional stream)
    (when stream 
        (princ "(:STATE " stream)
    (print-state (getf node :state) stream))
    
    (format stream " :ACTION ~s " (getf node :action))
    (format stream ":PATH-COST ~a " (getf node :path-cost))
    (format stream ":HEURISTIC ~a " (getf node :heuristic))
    (format stream ":A*-COST ~a " (getf node :a*-cost))
    (format stream ":DEPTH ~a " (getf node :depth))
    (format stream ":PARENT-ACTION ~a)~%" (if (getf node :parent) (getf (getf node :parent) :action) 'NIL))
)

(defun print-trace (node &optional (stream nil) &aux (actions ()))
    (dolist (out (list stream *standard-output*))
        (when out 
            (format out "Nodes Popped: ~d~%" *nodes-popped*)
            (format out "Nodes Expanded: ~d~%" *nodes-expanded*)
            (format out "Nodes Generated: ~d~%" *nodes-generated*)
            (format out "Max Fringe Size: ~a~%~%" *max-fringe-size*)
    ))
    (if node 
        (progn 
            (when stream (format stream "Solution found at depth ~d with cost of ~d.~%Steps:~%"
                (getf node :depth) (getf node :path-cost)))
            (format t "Solution found at depth ~d with cost of ~d.~%Steps:~%"
                (getf node :depth) (getf node :path-cost))

            (loop while (getf node :parent) do 
                (push (format nil "    ~a~%" (getf node :action)) actions)
                (setq node (getf node :parent)))
            (format t "~{~a~}" actions)
            (when stream (format stream "~{~a~}" actions)))

        (progn (write-line "No solution found!") 
            (when stream (write-line "No solution found!" stream)))))

(defun print-fringe (fringe &optional stream (space ""))
    (when stream 
        (format stream ":FRINGE (~%")
        (dolist (node fringe) (print-node1 node stream))
        (format stream ")~%")))

(define-modify-macro appendf (&rest args) append)

;; THE SEARCH FUNCTIONS
;; They will mostly have the same argument list

(defun ucs-compare (a b) (< (getf a :path-cost) (getf b :path-cost)))

(defun ucs (&optional stream (start *start-state*) (goal *goal-state*)
    &aux (closed (make-hash-table :size 1000 :test 'equalp)) (fringe (make-queue :priority-queue :compare #'ucs-compare)))
    (qpush fringe (new-node :state start :action "Start" :path-cost 0 :depth 0 :parent nil))
    
    (loop
        (setq *max-fringe-size* (max *max-fringe-size* (qsize fringe)))
        ;(print-fringe fringe)
        (when (<= (qsize fringe) 0) (return (values nil :failure)))
        (let* ((node (qpop fringe)) (state (getf node :state)))
            (incf *nodes-popped*)
            (when (goalp state) (return (values node :success)))
            (if (gethash state closed)
                () ;(format t "~% Duplicate state ~a found" state)
                (progn
                    (incf *nodes-expanded*)

                    (setf (gethash state closed) t)
                    (let* ((sccrs (successors state))
                        (len (length sccrs)))                    
                        (setf *nodes-generated* (+ len *nodes-generated*))

                        (when stream 
                            (format stream "Generating successors to " )
                            (print-node1 node stream)
                            (format stream "~d sucessors generated~%" len)

                            (princ "    CLOSED: (" stream)
                            (maphash #'(lambda (k v)
                                    (format stream "(")
                                (dotimes (i (array-dimension (state-grid k) 0))
                                    (format stream "(")
                                    (dotimes (j (array-dimension (state-grid k) 1))
                                        (format stream "~a " (aref (state-grid k) i j))
                                        )(princ ") " stream))
                                (princ ") " stream)) closed)
                            (write-line ")" stream))

                        (mapcar 
                            #'(lambda (successor)
                                (let ((s-state (getf successor :state)) 
                                        (s-action (getf successor :action))
                                        (s-scost (getf successor :step-cost)))
                                    
                                    (qpush fringe (new-node 
                                        :state s-state 
                                        :action s-action
                                        :path-cost (+ s-scost (getf node :path-cost))
                                        :depth (+ 1 (getf node :depth))
                                        :parent node)))) sccrs)))))
        (when stream 
        ;(print-fringe fringe stream)
        (write-line "    FRINGE: " stream)
        (print-queue fringe stream))))

(defun bfs (&optional stream (start *start-state*) (goal *goal-state*) 
    &aux (closed (make-hash-table :size 1000 :test 'equalp)) (fringe (make-queue :simple-queue)))
    (when (goalp start) 
        (return-from bfs (values (new-node :state start :action "Do nothing" :path-cost 0 :depth 0 :parent nil) :success)))
    (qpush fringe (new-node :state start :action "Start" :path-cost 0 :depth 0 :parent nil)) 
         
    (loop
        (setq *max-fringe-size* (max *max-fringe-size* (qsize fringe)))
        ;(print-fringe fringe)
        (when (<= (qsize fringe) 0) (return (values nil :failure)))
        (let* ((node (qpop fringe)) (state (getf node :state)))
            (incf *nodes-popped*)
            (if (gethash state closed)
                () ;(format t "~% Duplicate state ~a found" state)
                (progn
                    (incf *nodes-expanded*)

                    (setf (gethash state closed) t)
                    (let* ((sccrs (successors state))
                        (len (length sccrs)))                    
                        (setf *nodes-generated* (+ len *nodes-generated*))

                        (when stream 
                                                        (format stream "Generating successors to " )
                            (print-node1 node stream)
                            (format stream "~d sucessors generated~%" len)

                            (princ "    CLOSED: (" stream)
                            (maphash #'(lambda (k v)
                                    (format stream "(")
                                (dotimes (i (array-dimension (state-grid k) 0))
                                    (format stream "(")
                                    (dotimes (j (array-dimension (state-grid k) 1))
                                        (format stream "~a " (aref (state-grid k) i j))
                                        )(princ ") " stream))
                                (princ ") " stream)) closed)
                            (write-line ")" stream))

                        (mapcar 
                            #'(lambda (successor)
                                (when (not successor) (error node))
                                (let* ((s-state (getf successor :state)) 
                                        (s-action (getf successor :action))
                                        (s-scost (getf successor :step-cost))
                                        (s-node (new-node 
                                            :state s-state 
                                            :action s-action
                                            :path-cost (+ s-scost (getf node :path-cost))
                                            :depth (+ 1 (getf node :depth))
                                            :parent node)))

                                    (when (goalp s-state) (return (values s-node :success))) ;; make bfs faster
                                    (qpush fringe s-node))) sccrs)))))
        (when stream 
        ;(print-fringe fringe stream)
        (write-line "FRINGE: " stream)
        (print-queue fringe stream))))


;; =========

(defun dls (&optional stream (start *start-state*) (goal *goal-state*))
    (setq *max-fringe-size* 'N\/A)
    (dls-rec (progn (princ "Enter the depth limit: ") (force-output) (parse-integer (read-line))) stream start goal))

;(defparameter dls-closed (make-hash-table :size 1000 :test 'equalp))

(defun dls-rec (limit &optional stream (start *start-state*) (goal *goal-state*)
    &key (node 
        (new-node :state start 
            :action "Start" 
            :path-cost 0 
            :depth 0 :parent nil))
    (closed (make-hash-table :size 1000 :test 'equalp))

    &aux cur-state cutoff?)
    (setf cur-state (getf node :state))


    (incf *nodes-popped*)
    ;; Check if in closed before checking depth limit
    (when (gethash cur-state closed) (return-from dls-rec (values nil 'failure)))
    (setf (gethash cur-state closed) t) ;; Add it to the closed set

    (when stream
        (write-string "CURRENT NODE:  " stream)
        (print-node1 node stream)
        (terpri stream))
    
    (incf *nodes-expanded*)

    (when (goalp cur-state) (return-from dls-rec (values node 'success)))
    (when (= limit (getf node :depth)) (return-from dls-rec (values nil 'cutoff)))
    (dolist (successor (successors cur-state)) 
        (incf *nodes-generated*)

        (multiple-value-bind (result status) (dls-rec limit stream start goal :closed closed :node (new-node :state (getf successor :state)
            :action (getf successor :action)
            :path-cost (+ (getf successor :step-cost) (getf node :path-cost))
            :depth (1+ (getf node :depth))
            :parent node))
        (if (eq status 'cutoff) 
            (setf cutoff? t)
            (when (eq status 'success)
                (return-from dls-rec (values result 'success))))
        )) (values nil (if cutoff? 'cutoff 'failure)))

(defun ids (&optional stream (start *start-state*) (goal *goal-state*)
    &aux (ng 0) (np 0) (ne 0))    
        (setq *max-fringe-size* 'N\/A)

        (loop for depth from 1 to *INF*
        as xx = (progn 
            (when stream (format stream "==================================IDS with depth ~d======================================~%~%" depth))
            (format t "==================================IDS with depth ~d======================================~%~%" depth))
        as result = (dls-rec depth stream start goal)
        do 
            (setq ng (+ ng *nodes-generated*))
            (setq ne (+ ng *nodes-expanded*))
            (setq np (+ ng *nodes-popped*))
            (when result 
                (when stream (format stream "~%Total Results: ~%"))
                (format t "~%Total Results: ~%")
                (setq *nodes-expanded* ne)
                (setq *nodes-generated* ng)
                (setq *nodes-popped* np)
                (return result))

            (if (= depth *INF*)
                (progn (when stream (format stream "~%Total Results: ~%"))
                    (format t "~%Total Results: ~%")
                    (setq *nodes-expanded* ne)
                    (setq *nodes-generated* ng)
                    (setq *nodes-popped* np))

                (progn 
                    (terpri)
                    (when stream (terpri)) 
                    (print-trace result stream)
                    (setq *nodes-expanded* 0)
                    (setq *nodes-generated* 0)
                    (setq *nodes-popped* 0)))

            (when stream (write-line "=================================================================================================" stream) (terpri stream) (terpri stream))
            (write-line "=================================================================================================") (terpri) (terpri)))
             

;;=============
;; Heuristic function used by A* and Greedy
(defun h2(state &aux (grid1 (state-grid state)) (grid2 (state-grid *goal-state*)) (sum 0))
    (let ((map1 (make-hash-table :size (* ROWS COLS) :test 'eq))
        (map2 (make-hash-table :size (* ROWS COLS) :test 'eq)))
            (dotimes (i ROWS)
                (dotimes (j COLS)
                    (setf (gethash (aref grid1 i j) map1) (list i j))
                    (setf (gethash (aref grid2 i j) map2) (list i j))))
            (maphash
                #'(lambda (k v1) 
                    (let ((v2 (gethash k map2)))
                        ;; v1 and v2 are coordinate pairs
                        (setq sum (+ sum (* k ( +
                            (abs (- (first v1) (first v2)))
                            (abs (- (second v1) (second v2)))))))
                )) map1) sum))

(defun greedy-compare (a b) (< (getf a :heuristic) (getf b :heuristic)))

;; Informed Search Strategies
(defun greedy (&optional stream (start *start-state*) (goal *goal-state*)
    &aux (closed (make-hash-table :size 1000 :test 'equalp)) (fringe (make-queue :priority-queue :compare #'greedy-compare)))
    
    (qpush fringe (new-node :state start :action "Start" :path-cost 0 :heuristic (h2 start) :depth 0 :parent nil))
    (loop
        ;(print-fringe fringe)
        (setq *max-fringe-size* (max *max-fringe-size* (qsize fringe)))

        (when (<= (qsize fringe) 0) (return (values nil :failure)))
        (let* ((node (qpop fringe)) (state (getf node :state)))
            (incf *nodes-popped*)
            (when (goalp state) (return (values node :success)))
            (if (gethash state closed)
                () ;(format t "~% Duplicate state ~a found" state)
                (progn
                    (incf *nodes-expanded*)

                    (setf (gethash state closed) t)
                    (let* ((sccrs (successors state))
                        (len (length sccrs)))                    
                        (setf *nodes-generated* (+ len *nodes-generated*))

                        (when stream 
                                                        (format stream "Generating successors to " )
                            (print-node1 node stream)
                            (format stream "~d sucessors generated~%" len)

                            (princ "    CLOSED: (" stream)
                            (maphash #'(lambda (k v)
                                    (format stream "(")
                                (dotimes (i (array-dimension (state-grid k) 0))
                                    (format stream "(")
                                    (dotimes (j (array-dimension (state-grid k) 1))
                                        (format stream "~a " (aref (state-grid k) i j))
                                        )(princ ") " stream))
                                (princ ") " stream)) closed)
                            (write-line ")" stream))

                        (mapcar 
                            #'(lambda (successor)                          

                                (let ((s-state (getf successor :state)) 
                                        (s-action (getf successor :action))
                                        (s-scost (getf successor :step-cost)))

                                    (qpush fringe (new-node 
                                        :state s-state 
                                        :action s-action
                                        :path-cost (+ s-scost (getf node :path-cost))
                                        :heuristic (h2 s-state)
                                        :depth (+ 1 (getf node :depth))
                                        :parent node)))) sccrs)))))
        (when stream 
            ;(print-fringe fringe stream)
            (write-line "    FRINGE: " stream)
            (print-queue fringe stream))))

(defun a*-compare (a b) (< (getf a :a*-cost) (getf b :a*-cost)))

(defun a* (&optional stream (start *start-state*) (goal *goal-state*)
    &aux (closed (make-hash-table :size 1000 :test 'equalp)) (fringe (make-queue :priority-queue :compare #'a*-compare)))
    
    (let ((h-cost (h2 start))) 
        (qpush fringe (new-node :state start :action "Start" :path-cost 0 :heuristic h-cost :a*-cost (+ 0 h-cost) :depth 0 :parent nil)))
    (loop
        (setq *max-fringe-size* (max *max-fringe-size* (qsize fringe)))
        ;(print-fringe fringe)
        (when (<= (qsize fringe) 0) (return (values nil :failure)))

        (let* ((node (qpop fringe)) (state (getf node :state)))
            (incf *nodes-popped*)
            (when (goalp state) (return (values node :success)))
            (if (gethash state closed)
                () ;(format t "~% Duplicate state ~a found" state)
                (progn
                    (incf *nodes-expanded*)

                    (setf (gethash state closed) t)
                    (let* ((sccrs (successors state))
                        (len (length sccrs)))                    
                        (setf *nodes-generated* (+ len *nodes-generated*))

                        (when stream 
                                                        (format stream "Generating successors to " )
                            (print-node1 node stream)
                            (format stream "~d sucessors generated~%" len)

                            (princ "    CLOSED: (" stream)
                            (maphash #'(lambda (k v)
                                    (format stream "(")
                                (dotimes (i (array-dimension (state-grid k) 0))
                                    (format stream "(")
                                    (dotimes (j (array-dimension (state-grid k) 1))
                                        (format stream "~a " (aref (state-grid k) i j))
                                        )(princ ") " stream))
                                (princ ") " stream)) closed)
                            (write-line ")" stream))

                        (mapcar 
                            #'(lambda (successor)
                                (let* ((s-state (getf successor :state)) 
                                        (s-action (getf successor :action))
                                        (s-scost (getf successor :step-cost))
                                        (path-cost (+ s-scost (getf node :path-cost)))
                                        (h-cost (h2 s-state))) 
                                
                                (qpush fringe (new-node 
                                    :state s-state 
                                    :action s-action
                                    :path-cost path-cost
                                    :heuristic h-cost
                                    :a*-cost (+ path-cost h-cost)
                                    :depth (+ 1 (getf node :depth))
                                    :parent node)))) sccrs)))))
        (when stream 
            ;(print-fringe fringe stream)
            (write-line "    FRINGE: " stream)
            (print-queue fringe stream))))

;;=============

(defun dfs (&optional stream (start *start-state*) (goal *goal-state*) &aux (closed (make-hash-table :size 10000 :test 'equalp)) (fringe ()) (size 1))
    (push (new-node :state start :action "Start" :path-cost 0 :depth 0 :parent nil) fringe) 
    
    ;(setf *max-fringe-size* 1)
    (loop
        (setq *max-fringe-size* (max *max-fringe-size* size))

        ;(print-fringe fringe)
        (when (null fringe) (return (values nil :failure)))
        (let* ((node (pop fringe)) (state (getf node :state)))
            (decf size)
            (incf *nodes-popped*)
            (when (goalp state) (return (values node :success)))
            (if (gethash state closed)
                () ;(format t "~% Duplicate state ~a found" state)
                (progn
                    (incf *nodes-expanded*)

                    (setf (gethash state closed) t)
                    (let* ((sccrs (successors state))
                        (len (length sccrs)))                
                        (setf *nodes-generated* (+ len *nodes-generated*))

                        (when stream 
                                                        (format stream "Generating successors to " )
                            (print-node1 node stream)
                            (format stream "~d sucessors generated~%" len)

                            (princ "    CLOSED: (" stream)
                            (maphash #'(lambda (k v)
                                    (format stream "(")
                                (dotimes (i (array-dimension (state-grid k) 0))
                                    (format stream "(")
                                    (dotimes (j (array-dimension (state-grid k) 1))
                                        (format stream "~a " (aref (state-grid k) i j))
                                        )(princ ") " stream))
                                (princ ") " stream)) closed)
                            (write-line ")" stream))

                        (mapcar 
                            #'(lambda (successor)
                            (let ((s-state (getf successor :state)) 
                                    (s-action (getf successor :action))
                                    (s-scost (getf successor :step-cost)))
                                (incf size)
                                (push (new-node 
                                    :state s-state 
                                    :action s-action
                                    :path-cost (+ s-scost (getf node :path-cost))
                                    :depth (+ 1 (getf node :depth))
                                    :parent node) fringe))) sccrs)))))
        (when stream 
            (write-line "    FRINGE: " stream)
            (print-fringe fringe stream))))

(defun shift() 
    (let ((arg (car *posix-argv*)))
        (setq *posix-argv* (cdr *posix-argv*))
    arg))

(defun log-file-name ()
    (multiple-value-bind
           (second minute hour day month year)
           (get-decoded-time)
           (format nil "trace-~2,'0d-~2,'0d-~d_~2,'0d-~2,'0d-~2,'0d.txt" month day year hour minute second)))

;; Entry point (lisp doesn't even need this but my dumbass had to put it in)
(defun main()
    (shift)
    (let (
        (all-args (format nil "~s" *posix-argv*))
        (startf (shift)) 
        (goalf  (shift)) 
        (method (shift))
        (dump   (shift))) ; we leave dump as is since if a value is set, it is evaluated as true 

        (if (or (not startf) (not goalf))
            (progn 
                (write-line "Start file and Goal file must be given" *error-output*) 
                (return-from main -1)) ; non-zero exit code
            ())
        (if method (setq method (read-from-string method)) (setq method 'a*))

        (setq *start-state* (state-from-file startf))
        (setq *goal-state*  (state-from-file goalf))

        (format t "Command-line arguments: ~a~%" all-args)
        (format t "Method selected: ~a~%Running \"~a\"~%~%" method method)

        (if dump (with-open-file (stream (log-file-name) 
            :direction :output
            :if-exists :overwrite 
            :if-does-not-exist :create)

            (print-trace (funcall method stream) stream))
            (print-trace (funcall method))))
)

(main)