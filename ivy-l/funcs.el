;; Evil

(defun spacemacs/ivy-evil-registers ()
  "Show evil registers"
  (interactive)
  (let ((ivy-height 24))
    (ivy-read "Evil Registers:"
              (cl-loop for (key . val) in (evil-register-list)
                       collect (eval `(format "%s : %s" (propertize ,(char-to-string key) 'face 'font-lock-builtin-face)
                                              ,(or (and val
                                                        (stringp val)
                                                        (replace-regexp-in-string "\n" "^J" val))
                                                   ""))))
              :action #'spacemacs/ivy-insert-evil-register)))

(defun spacemacs/ivy-insert-evil-register (candidate)
  (insert (replace-regexp-in-string "\\^J" "\n"
                                    (substring-no-properties candidate 4))))
