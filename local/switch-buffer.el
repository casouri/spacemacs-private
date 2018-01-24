;;================================================================================
;; switch-buffer
(define-key evil-normal-state-map (kbd "<S-return>") 'buffer-switch)
;; keymap used in the popup menu
(setq switch-keymap (make-sparse-keymap))
(define-key switch-keymap (kbd "<tab>") 'popup-next)
(define-key switch-keymap (kbd "j") 'popup-next)
(define-key switch-keymap (kbd "k") 'popup-previous)
(define-key switch-keymap (kbd "<return>") 'popup-select)
(setq buffer-switch-max 5)

(defun buffer-switch ()
  (interactive)
  ;; all the buffers
  (setq full-buffer-list (mapcar (function buffer-name) (buffer-list)))
  (if buffer-switch-max
      (progn
        ;; if max specified, only take n buffers
        (setq buffer-select-list (subseq full-buffer-list 0 buffer-switch-max))
        )
    ;; if not specified, take all
    (setq buffer-select-list full-buffer-list)
    )
  (setq dest-buffer (popup-menu* buffer-select-list :keymap switch-keymap))
  (switch-to-buffer dest-buffer)
  )

(provide 'switch-buffer)
;; switch-buffer ends here
