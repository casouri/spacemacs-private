
;;================================================================================
;; custom functions

;; open log.org
(defun open-log () (interactive) (find-file "~/log.org"))

;; insert date for blog composing
(defun insert-current-date () (interactive)
        (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

;; insert ; at end of line
(defun insert-semi-at-eol ()
  "Insert semicolon at end of line."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")
    ))

;; create new line above/below without breaking current line
(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun beginning-of-line-and-indented-new-line-above ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (previous-line))


;;================================================================================
;; line numbers have different background color than buffer, this snippet set it to same
(defun match-number-line-backgroud-color ()
  (interactive)
  (set-face-background 'linum (face-attribute 'default :background) nil)
  )

;;================================================================================
;; page scrolling
(defun scroll-down-reserve-point ()
  (interactive)
  (scroll-up 2)
  (next-line)
  (next-line)
  )
(defun scroll-up-reserve-point ()
  (interactive)
  (scroll-down 2)
  (previous-line)
  (previous-line)
  )

(defun sync-hlinum-face ()
  (set-face-attribute
   'linum-highlight-face nil
   :background (face-attribute 'hl-line :background)
   :foreground (face-attribute 'font-lock-keyword-face :foreground)
   :weight 'bold
   ))

