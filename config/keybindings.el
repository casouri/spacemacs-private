;; kill buffer
(spacemacs/set-leader-keys (kbd "ok") 'kill-buffer-and-window)
;; save buffer
(spacemacs/set-leader-keys (kbd "os") 'save-buffer)
;; insert shortcuts
(spacemacs/declare-prefix "oi" "insert")
(spacemacs/set-leader-keys (kbd "oid") 'insert-current-date)

;; save-buffer shortcut
(global-set-key (kbd "C-s") 'save-buffer)

;; insert ;
(define-key evil-insert-state-map (kbd "C-;") 'insert-semi-at-eol)

;; simple hybrid mode

(define-key evil-insert-state-map (kbd "C-n") #'next-line)
(define-key evil-insert-state-map (kbd "C-p") #'previous-line)
(define-key evil-insert-state-map (kbd "C-f") #'forward-char)
(define-key evil-insert-state-map (kbd "C-b") #'backward-char)
(define-key evil-insert-state-map (kbd "C-a") #'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") #'end-of-line)
