;;; packages.el --- config layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Yuan Fu <yuan@missSilver>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst config-packages
  '(
    nyan-mode
    (switch-input-mode :location local)
    json-mode
    evil
    org-mode
    python
    google-translate
    dired
    hlinum
    ))

(defun config/init-json-mode ()
  (use-package json-mode
    :mode ("\\.json\\'" . json-mode)))

(defun config/init-switch-input-mode ()
  (use-package switch-input-mode
    :config
    (setq switch-input-source0 "com.apple.keylayout.ABC")
    (setq switch-input-source1 "com.apple.inputmethod.SCIM.ITABC")
    :commands (switch-input-mode)
    ))

(defun config/init-nyan-mode ()
  (use-package nyan-mode
    :config
    (nyan-mode 1)
    (setq nyan-wavy-trail t)
    (setq nyan-animate-nyancat t)
    ))

(defun config/post-init-evil ()
  ;; uer evil-search instead of isearch
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; page scrolling
  (define-key evil-normal-state-map (kbd "J") 'scroll-down-reserve-point)
  (define-key evil-normal-state-map (kbd "K") 'scroll-up-reserve-point)

  ;; use capslock led to indicate insert & nomal mode
  (add-hook 'evil-insert-state-entry-hook (lambda () (shell-command-to-string "setleds +caps")))
  (add-hook 'evil-normal-state-entry-hook (lambda () (shell-command-to-string "setleds -caps")))
  ;; custom functions
  (define-key evil-insert-state-map (kbd "<C-return>") 'end-of-line-and-indented-new-line)

  (define-key evil-insert-state-map (kbd "<M-return>") 'beginning-of-line-and-indented-new-line-above)
)


(defun config/post-init-org-mode ()
  ;; soft wrap for org-mode
  (setq org-startup-truncated nil)
  )

(defun config/post-init-python ()
  (setq python-indent-offset 4)
  )

(defun config/post-init-google-translate ()
  (setq google-translate-default-target-language "zh")
  (spacemacs/set-leader-keys (kbd "ot") 'google-translate-at-point)
  )

(defun config/post-init-dired ()
  ;; jump to functions
  (defun jump-to-note-org () (interactive) (find-file "~/note/note.org"))
  (defun jump-to-note () (interactive) (dired "~/note"))
  (defun jump-to-project () (interactive) (dired "~/p"))
  (defun jump-to-Desktop () (interactive) (dired "~/Desktop"))
  (defun jump-to-attic () (interactive) (dired "~/attic") )
  (defun jump-to-bin () (interactive) (dired "~/bin"))

  ;; directory shortcuts
  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/set-leader-keys (kbd "on") 'jump-to-note-org)

  (spacemacs/declare-prefix "oo" "dir-shortcuts")
  (spacemacs/set-leader-keys (kbd "oon") 'jump-to-note)
  (spacemacs/set-leader-keys (kbd "oop") 'jump-to-project)
  (spacemacs/set-leader-keys (kbd "ooD") 'jump-to-Desktop)
  (spacemacs/set-leader-keys (kbd "ooa") 'jump-to-attic)
  (spacemacs/set-leader-keys (kbd "oob") 'jump-to-bin))

(defun config/init-hlinum ()
  (use-package hlinum
    :config
    (progn
      (hlinum-activate)
      (set-face-attribute 'linum-highlight-face nil
                          :background (face-attribute 'hl-line :background)
                          :foreground (face-attribute 'font-lock-keyword-face :foreground)
                          :weight 'bold
                          )
    )))

;;; packages.el ends here
