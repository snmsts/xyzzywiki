(defvar *calc-history* nil)
(defvar *current-completion-list* nil)
(defvar *current-completion-number* -1)

(defun calc-make-completion (preedit)
  (setq *current-completion-list*
	(if preedit
	    (let ((reg (compile-regexp (concat "^" (regexp-quote preedit)))))
	      (remove-if #'(lambda (x)
			     (not (string-match reg x))) *calc-history*))
	  *calc-history*)))

(defun calc-get-line()
  (let (line)
    (when (save-excursion
	    (goto-bol)
	    (looking-at "^ *\\$ *\\(.*\\)$"))
      (setq line (match-string 1))
      (when (not (string-match "^[ 	]*$" line))
	line))))

(defun calc-eval-line-add-history()
  (interactive)
  (let ((expr-line (calc-get-line)))
    (when expr-line
      (setq *calc-history* (cons expr-line *calc-history*))))
  (ed::calc-eval-line))

(defun calc-complete-from-history (direction)
  (let (expr-begin expr-end)
    (when (save-excursion
	    (goto-bol)
	    (looking-at "^ *\\$ *\\(.*\\)$"))
      (setq expr-begin (match-beginning 1)
	    expr-end   (match-end 1))
      (unless (or (eq *last-command* 'calc-complete-next)
		  (eq *last-command* 'calc-complete-previous))
	(setq *current-completion-list* (calc-make-completion (calc-get-line))
	      *current-completion-number* -1))
      (let (number str)
	(if (eq direction 'up)
	    (if (< (1+ *current-completion-number*) (length *current-completion-list*))
		(setq number (1+ *current-completion-number*)))
	  (if (> *current-completion-number* 0)
	      (setq number (1- *current-completion-number*))))
	(when number
	  (setq str (nth number *current-completion-list*))
	  (delete-region expr-begin expr-end)
	  (insert str)
	  (setq *current-completion-number* number))))))


(defun calc-complete-previous()
  (interactive)
  (calc-complete-from-history 'up))

(defun calc-complete-next()
  (interactive)
  (calc-complete-from-history 'down))

(defun calc-beginng-of-line()
  (interactive)
  (if (save-excursion
	(goto-bol)
	(looking-at "^ *\\$ ?"))
      (goto-char (match-end 0))
    (goto-bol)))
    
(define-key *calc-mode-map* #\C-a 'calc-beginng-of-line)
(define-key *calc-mode-map* #\M-n 'calc-complete-next)
(define-key *calc-mode-map* #\M-p 'calc-complete-previous)
(define-key *calc-mode-map* #\RET 'calc-eval-line-add-history)
