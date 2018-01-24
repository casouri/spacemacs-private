
(define-minor-mode switch-input-mode
  "switch to different input source when enter insert mode,
switch back when back to normal mode."
  :lighter " Ⓢ"
  )


(defun switch-input-use-source (source)
  (when switch-input-mode
    (shell-command (replace-regexp-in-string "source" source "issw source")))

  ;; use the second input source when inserting
  (add-hook 'evil-insert-state-entry-hook
            (lambda () (switch-input-use-source switch-input-source1)))
  ;; use English input when back to normal mode
  (add-hook 'evil-insert-state-exit-hook
            (lambda () (switch-input-use-source switch-input-source0)))
  )

