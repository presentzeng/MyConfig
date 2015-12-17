(provide 'vim)
;;paren match
(show-paren-mode 1)
;;recenter	
;;set nu
(global-linum-mode 1)

;;highlight current line
;;(global-hl-line-mode 1)

;;hjkl
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
