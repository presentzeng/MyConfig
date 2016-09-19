(setq scheme-program-name "mit-scheme")


(define-key global-map (kbd "C-q") 'scheme-send-last-sexp-split-window)

;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.4 (window-height))))
    )))


(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (run-scheme-in-emacs )
  (other-window 1)
  )



(defun run-scheme-in-emacs ()
(interactive)
(let ((file-name (buffer-file-name)))
(progn
(run-scheme scheme-program-name)
(scheme-load-file file-name))))








;
;(ren match
(show-paren-mode 1)
;;;recenter
;;;set nu
(global-linum-mode 1)
;
;highlight current line
;;;(global-hl-line-mode 1)
;
;;;hjkl
(define-key global-map (kbd "C-k ") 'previous-line)
(define-key global-map (kbd "C-j ") 'next-line)
(define-key global-map (kbd "C-l ") 'forward-char)
(define-key global-map (kbd "C-h ") 'backward-char)

;;page
(define-key global-map (kbd "M-b") 'scroll-down)
(define-key global-map (kbd "M-f ") 'scroll-up)

;;word
(define-key global-map (kbd "C-b ") 'backward-word)
(define-key global-map (kbd "C-e ") 'forward-word)

;;sentence
(global-set-key (kbd "M-h")  'beginning-of-line)
;(define-key global-map (kbd "C-i ") 'beginning-of-line)
(define-key global-map (kbd "M-l ") 'end-of-line)

;;undo redo
;;(define-key global-map (kbd "C-u ") 'undo)

;;block
(define-key global-map (kbd "C-v ") 'set-mark-command)

;;delete
;;C-x del delete former from tag
(define-key global-map (kbd "C-d ") 'kill-line)
(define-key global-map (kbd "C-n ") 'delete-char)
(define-key global-map (kbd "C-x w") 'kill-word)

;;(define-key global-map (kbd "C-q ") 'recent-jump)

;;paste
(define-key global-map (kbd "C-p ") 'yank)

(define-key global-map (kbd "C-t ") 'universal-argument)

;;ups and downs
(define-key global-map (kbd "M-{ ") 'beginning-of-defun)
(define-key global-map (kbd "M-} ") 'end-of-defun)
(define-key global-map (kbd "M-[ ") 'down-list)
(define-key global-map (kbd "M-] ") 'up-list)

(define-key global-map (kbd "C-m ") 'recenter)

;;gg G
(define-key global-map (kbd "M-k ") 'beginning-of-buffer)
(define-key global-map (kbd "M-j ") 'end-of-buffer)


;;copy region or whole line(global-set-key "\C-y
(global-set-key "\C-y"
(lambda ()
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning)
      (region-end))
    (progn
     (kill-ring-save (line-beginning-position)
     (line-end-position))
     (message "copied line")))))


;;kill region or whole line
(global-set-key "\M-d"
(lambda ()
  (interactive)
  (if mark-active
      (kill-region (region-beginning)
   (region-end))
    (progn
     (kill-region (line-beginning-position)
  (line-end-position))
     (message "killed line")))))



;;tab
;;(define-key global-map (kbd "TAB ") 'indent-tabs-mode)

;;insert next line
(global-set-key (kbd "C-o") '(lambda ()
(interactive)
(move-end-of-line 1)
(newline)))


(global-set-key (kbd "C-u") '(lambda ()
(interactive)
(move-beginning-of-line 1)
(newline)
(previous-line)))

;;tab
;;(global-set-key (kbd "TAB") '(lambda () (interactive) (insert-char 9 1)))
;;macro
;;C-X-( record
;;C-X-) stop
;;C-X-e do
;;C-t n times


;;file
;;C-x-f open file
;;C-x-s save
;;C-x-c exit

;;M-r delete the outside paren
(define-key global-map (kbd "M-r ") 'paredit-raise-sexp)

(defun paredit-raise-sexp (&optional argument)
    "Raise the following S-expression in a tree, deleting its siblings.
With a prefix argument N, raise the following N S-expressions.  If N
  is negative, raise the preceding N S-expressions.
If the point is on an S-expression, such as a string or a symbol, not
  between them, that S-expression is considered to follow the point."
    (interactive "P")
    (save-excursion
      (cond ((paredit-in-string-p)
             (goto-char (car (paredit-string-start+end-points))))
            ((paredit-in-char-p)
             (backward-sexp))
            ((paredit-in-comment-p)
             (error "No S-expression to raise in comment.")))
      ;; Select the S-expressions we want to raise in a buffer substring.
      (let* ((n (prefix-numeric-value argument))
             (bound (scan-sexps (point) n))
             (sexps
              (if (< n 0)
                  (buffer-substring bound (paredit-point-at-sexp-end))
                (buffer-substring (paredit-point-at-sexp-start) bound))))
        ;; Move up to the list we're raising those S-expressions out of and
        ;; delete it.
        (backward-up-list)
        (delete-region (point) (scan-sexps (point) 1))
        (let* ((indent-start (point))
               (indent-end (save-excursion (insert sexps) (point))))
          (indent-region indent-start indent-end nil)))))



(defun paredit-in-string-p (&optional state)
  "True if the parse state is within a double-quote-delimited string.
If no parse state is supplied, compute one from the beginning of the
  defun to the point."
  ;; 3. non-nil if inside a string (the terminator character, really)
  (and (nth 3 (or state (paredit-current-parse-state)))
       t))


       (defun paredit-in-char-p (&optional position)
  "True if point is on a character escape outside a string."
  (save-excursion
    (goto-char (or position (point)))
    (paredit-in-string-escape-p)))


(defun paredit-in-string-escape-p ()
  "True if the point is on a character escape of a string.
This is true only if the character is preceded by an odd number of
  backslashes.
This assumes that `paredit-in-string-p' has already returned true."
  (let ((oddp nil))
    (save-excursion
      (while (eq (char-before) ?\\ )
        (setq oddp (not oddp))
        (backward-char)))
    oddp))


(defun paredit-in-comment-p (&optional state)
  "True if parse state STATE is within a comment.
If no parse state is supplied, compute one from the beginning of the
  defun to the point."
  ;; 4. nil if outside a comment, t if inside a non-nestable comment,
  ;;    else an integer (the current comment nesting)
  (and (nth 4 (or state (paredit-current-parse-state)))
       t))


(defun paredit-point-at-sexp-start ()
  (save-excursion
    (forward-sexp)
    (backward-sexp)
    (point)))

(defun paredit-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    ;; Calling PARSE-PARTIAL-SEXP will advance the point to its second
    ;; argument (unless parsing stops due to an error, but we assume it
    ;; won't in paredit-mode).
    (parse-partial-sexp (point) point)))















;;C-x l
(define-key global-map (kbd "C-]") 'paredit-forward-slurp-sexp )

(defun paredit-backward-barf-sexp (&optional argument)
  "Remove the first S-expression in the current list from that list
  by moving the closing delimiter.
Automatically reindent the barfed S-expression and the form from which
  it was barfed."
  (interactive "P")
  (paredit-lose-if-not-in-sexp 'paredit-backward-barf-sexp)
  (if (and (numberp argument) (< argument 0))
      (paredit-backward-slurp-sexp (- 0 argument))
    (let ((end (make-marker)))
      (set-marker end (point))
      (save-excursion
        (backward-up-list)
        (let ((open (char-after)))
          (delete-char +1)
          (paredit-ignore-sexp-errors
            (paredit-forward-and-indent
             (if (or (not argument) (numberp argument))
                 argument
                 (let ((n 0))
                   (save-excursion
                     (while (paredit-handle-sexp-errors
                                (save-excursion
                                  (forward-sexp)
                                  (<= (point) end))
                              nil)
                       (forward-sexp)
                       (setq n (+ n 1))))
                   n))))
          (while (progn (paredit-skip-whitespace t) (eq (char-after) ?\; ))
            (forward-line 1))
          (if (eobp)
              ;++ We'll have deleted the close, but there's no open.
              ;++ Is that OK?
              (error "Barfing all subexpressions with no close-paren?"))
          ;** Don't use `insert' here.  Consider, e.g., barfing from
          ;**   (foo|)
          ;** and how `save-excursion' works.
          (insert-before-markers open))
        (backward-up-list)
        (lisp-indent-line)
        (indent-sexp)))))


(defun paredit-forward-slurp-sexp (&optional argument)
  "Add the S-expression following the current list into that list
  by moving the closing delimiter.
Automatically reindent the newly slurped S-expression with respect to
  its new enclosing form.
If in a string, move the opening double-quote forward by one
  S-expression and escape any intervening characters as necessary,
  without altering any indentation or formatting."
  (interactive "P")
  (save-excursion
    (cond ((paredit-in-comment-p)
           (error "Invalid context for slurping S-expressions."))
          ((numberp argument)
           (if (< argument 0)
               (paredit-forward-barf-sexp (- 0 argument))
               (while (< 0 argument)
                 (paredit-forward-slurp-sexp)
                 (setq argument (- argument 1)))))
          ((paredit-in-string-p)
           ;; If there is anything to slurp into the string, take that.
           ;; Otherwise, try to slurp into the enclosing list.
           (if (save-excursion
                 (goto-char (paredit-enclosing-string-end))
                 (paredit-handle-sexp-errors (progn (forward-sexp) nil)
                   t))
               (progn
                 (goto-char (paredit-enclosing-string-end))
                 (paredit-forward-slurp-into-list argument))
               (paredit-forward-slurp-into-string argument)))
          (t
           (paredit-forward-slurp-into-list argument)))))


(defun paredit-forward-slurp-into-list (&optional argument)
  (let ((nestedp nil))
    (save-excursion
      (up-list)                            ; Up to the end of the list to
      (let ((close (char-before)))         ;   save and delete the closing
        (delete-char -1)                   ;   delimiter.
        (let ((start (point)))
          (catch 'return                   ; Go to the end of the desired
            (while t                       ;   S-expression, going up a
              (paredit-handle-sexp-errors  ;   list if it's not in this,
                  (progn (forward-sexp)
                         (if argument
                             (paredit-ignore-sexp-errors
                               (while (not (eobp))
                                 (forward-sexp))))
                         (throw 'return nil))
                (setq nestedp t)
                (up-list)
                (setq close                ; adjusting for mixed
                      (prog1 (char-before) ;   delimiters as necessary,
                        (delete-char -1)
                        (insert close))))))
          (insert close)                   ;  to insert that delimiter.
          (indent-region start (point) nil))))
    (if (and (not nestedp)
             (eq (save-excursion (paredit-skip-whitespace nil) (point))
                 (save-excursion (backward-up-list) (forward-char) (point)))
             (eq (save-excursion (forward-sexp) (backward-sexp) (point))
                 (save-excursion (paredit-skip-whitespace t) (point))))
        (delete-region (save-excursion (paredit-skip-whitespace nil) (point))
                       (save-excursion (paredit-skip-whitespace t) (point))))))



  (defmacro paredit-handle-sexp-errors (body &rest handler)
    `(condition-case ()
         ,body
       (,paredit-sexp-error-type ,@handler)))

  (put 'paredit-handle-sexp-errors 'lisp-indent-function 1)


(defvar paredit-sexp-error-type
    (with-temp-buffer
      (insert "(")
      (condition-case condition
          (backward-sexp)
        (error (if (eq (car condition) 'error)
                   (paredit-warn "%s%s%s%s%s"
                                 "Paredit is unable to discriminate"
                                 " S-expression parse errors from"
                                 " other errors. "
                                 " This may cause obscure problems. "
                                 " Please upgrade Emacs."))
               (car condition)))))

(defun paredit-skip-whitespace (trailing-p &optional limit)
  "Skip past any whitespace, or until the point LIMIT is reached.
If TRAILING-P is nil, skip leading whitespace; otherwise, skip trailing
  whitespace."
  (funcall (if trailing-p 'skip-chars-forward 'skip-chars-backward)
           " \t\n
"  ; This should skip using the syntax table, but LF
           limit))    ; is a comment end, not newline, in Lisp mode.
















(define-key global-map (kbd "C-]") 'paredit-forward-barf-sexp)



(defun paredit-forward-barf-sexp (&optional argument)
  "Remove the last S-expression in the current list from that list
  by moving the closing delimiter.
Automatically reindent the newly barfed S-expression with respect to
  its new enclosing form."
  (interactive "P")
  (paredit-lose-if-not-in-sexp 'paredit-forward-barf-sexp)
  (if (and (numberp argument) (< argument 0))
      (paredit-forward-slurp-sexp (- 0 argument))
    (let ((start (point)) (end nil))
      (save-excursion
        (up-list)                       ; Up to the end of the list to
        (let ((close (char-before)))    ;   save and delete the closing
          (delete-char -1)              ;   delimiter.
          (setq end (point))
          (paredit-ignore-sexp-errors   ; Go back to where we want to
            (if (or (not argument)      ;   insert the delimiter.
                    (numberp argument))
                (backward-sexp argument)
                (while (paredit-handle-sexp-errors
                           (save-excursion (backward-sexp) (<= start (point)))
                         nil)
                  (backward-sexp))))
          (paredit-skip-whitespace nil) ; Skip leading whitespace.
          (cond ((bobp)
                 ;++ We'll have deleted the close, but there's no open.
                 ;++ Is that OK?
                 (error "Barfing all subexpressions with no open-paren?"))
                ((paredit-in-comment-p) ; Don't put the close-paren in
                 (newline)))            ;   a comment.
          (insert close))
        ;; Reindent all of the newly barfed S-expressions.  Start at the
        ;; start of the first barfed S-expression, not at the close we
        ;; just inserted.
        (forward-sexp)
        (backward-sexp)
        (if (or (not argument) (numberp argument))
            (paredit-forward-and-indent argument)
            (indent-region (point) end))))))


(defun paredit-lose-if-not-in-sexp (command)
  (if (or (paredit-in-string-p)
          (paredit-in-comment-p)
          (paredit-in-char-p))
      (error "Invalid context for command `%s'." command)))


(defmacro paredit-ignore-sexp-errors (&rest body)
    `(paredit-handle-sexp-errors (progn ,@body)
       nil))

  (put 'paredit-ignore-sexp-errors 'lisp-indent-function 0)


(defun paredit-forward-and-indent (&optional n)
  "Move forward by N S-expressions, indenting them with `indent-region'."
  (let ((start (point)))
    (forward-sexp n)
    (indent-region start (point) nil)))
