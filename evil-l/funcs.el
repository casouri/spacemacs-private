
;; evil-search-highlight-persist
(defun spacemacs/evil-search-clear-highlight ()
  "Clear evil-search or evil-ex-search persistent highlights."
  (interactive)
  (case evil-search-module
    ('isearch (evil-search-highlight-persist-remove-all))
    ('evil-search (evil-ex-nohighlight))))

(defun spacemacs//adaptive-evil-highlight-persist-face ()
  (set-face-attribute 'evil-search-highlight-persist-highlight-face nil
                      :inherit 'lazy-highlight
                      :background nil
                      :foreground nil))


;; vi-tilde-fringe
(defun spacemacs/disable-vi-tilde-fringe ()
  "Disable `vi-tilde-fringe' in the current buffer."
  (vi-tilde-fringe-mode -1))

(defun spacemacs/disable-vi-tilde-fringe-read-only ()
  "Disable `vi-tilde-fringe' in the current buffer if it is read only."
  (when buffer-read-only
    (spacemacs/disable-vi-tilde-fringe)))
