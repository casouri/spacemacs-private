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
(define-key evil-hybrid-state-map (kbd "C-;") 'insert-semi-at-eol)

