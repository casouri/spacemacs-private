;;; packages.el --- evil-l layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Yuan Fu <yuan@missSilver>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `evil-l-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `evil-l/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `evil-l/pre-init-PACKAGE' and/or
;;   `evil-l/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst evil-l-packages
  '(evil-surround
    evil-anzu
    evil-nerd-commenter
    evil-mc
    evil-search-highlight-persist
    evil-terminal-cursor-changer
    evil-matchit
    vi-tilde-fringe
    ))

(defun evil-l/init-vi-tilde-fringe ()
  (spacemacs|do-after-display-system-init
   (use-package vi-tilde-fringe
     :init
     (progn
       (global-vi-tilde-fringe-mode)
       (spacemacs|add-toggle vi-tilde-fringe
         :mode global-vi-tilde-fringe-mode
         :documentation
         "Globally display a ~ on empty lines in the fringe."
         :evil-leader "T~")
       ;; don't enable it on some special buffers
       (with-current-buffer spacemacs-buffer-name (spacemacs/disable-vi-tilde-fringe))
       (add-hook 'which-key-init-buffer-hook 'spacemacs/disable-vi-tilde-fringe)
       ;; after a major mode is loaded, check if the buffer is read only
       ;; if so, disable vi-tilde-fringe-mode
       (add-hook 'after-change-major-mode-hook
                 'spacemacs/disable-vi-tilde-fringe-read-only)
       ;; TODO move this hook if/when we have a layer for eww
       (spacemacs/add-to-hooks 'spacemacs/disable-vi-tilde-fringe
                               '(eww-mode-hook)))
     :config
     (spacemacs|hide-lighter vi-tilde-fringe-mode))))

(defun evil-l/init-evil-matchit ()
  (use-package evil-matchit
    :defer t
    :init (global-evil-matchit-mode 1)))

(defun evil-l/init-evil-terminal-cursor-changer ()
  (use-package evil-terminal-cursor-changer
    :if (not (display-graphic-p))
    :init (setq evil-visual-state-cursor 'box
                evil-insert-state-cursor 'bar
                evil-emacs-state-cursor 'hbar)))

  (defun evil-l/init-evil-search-highlight-persist ()
    (use-package evil-search-highlight-persist
      :init
      (progn
        (global-evil-search-highlight-persist)
        ;; (set-face-attribute )
        (spacemacs/set-leader-keys "sc" 'spacemacs/evil-search-clear-highlight)
        (define-key evil-search-highlight-persist-map (kbd "C-x SPC") 'rectangle-mark-mode)
        (evil-ex-define-cmd "nohlsearch"
                            'evil-search-highlight-persist-remove-all)
        (spacemacs//adaptive-evil-highlight-persist-face)
        (add-hook 'spacemacs-post-theme-change-hook 'spacemacs//adaptive-evil-highlight-persist-face))))

(defun evil-l/init-evil-anzu ()
  (use-package evil-anzu
    :init
    (global-anzu-mode t)
    :config
    (progn
      (spacemacs|hide-lighter anzu-mode)
      (setq anzu-search-threshold 1000
            anzu-cons-mode-line-p nil)
      ;; powerline integration
      (when (configuration-layer/package-usedp 'spaceline)
        (defun spacemacs/anzu-update-mode-line (here total)
          "Custom update function which does not propertize the status."
          (when anzu--state
            (let ((status (cl-case anzu--state
                            (search (format "(%s/%d%s)"
                                            (anzu--format-here-position here total)
                                            total (if anzu--overflow-p "+" "")))
                            (replace-query (format "(%d replace)" total))
                            (replace (format "(%d/%d)" here total)))))
              status)))
        (setq anzu-mode-line-update-function 'spacemacs/anzu-update-mode-line)))))

(defun evil-l/init-evil-surround ()
  (use-package evil-surround
    :init
    (progn
      (global-evil-surround-mode 1)
      ;; `s' for surround instead of `substitute'
      ;; see motivation for this change in the documentation
      (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
      (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))))

;; other commenting functions in funcs.el with keybinds in keybindings.el
(defun evil-l/init-evil-nerd-commenter ()
  (use-package evil-nerd-commenter
    :commands evilnc-comment-operator
    :init
    (progn
      ;; double all the commenting functions so that the inverse operations
      ;; can be called without setting a flag
      (defun spacemacs/comment-or-uncomment-lines-inverse (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line t))
          (evilnc-comment-or-uncomment-lines arg)))

      (defun spacemacs/comment-or-uncomment-lines (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line nil))
          (evilnc-comment-or-uncomment-lines arg)))

      (defun spacemacs/copy-and-comment-lines-inverse (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line t))
          (evilnc-copy-and-comment-lines arg)))

      (defun spacemacs/copy-and-comment-lines (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line nil))
          (evilnc-copy-and-comment-lines arg)))

      (defun spacemacs/quick-comment-or-uncomment-to-the-line-inverse
          (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line t))
          (evilnc-comment-or-uncomment-to-the-line arg)))

      (defun spacemacs/quick-comment-or-uncomment-to-the-line (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line nil))
          (evilnc-comment-or-uncomment-to-the-line arg)))

      (defun spacemacs/comment-or-uncomment-paragraphs-inverse (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line t))
          (evilnc-comment-or-uncomment-paragraphs arg)))

      (defun spacemacs/comment-or-uncomment-paragraphs (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line nil))
          (evilnc-comment-or-uncomment-paragraphs arg)))

      (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
      (define-key evil-normal-state-map "gy" 'spacemacs/copy-and-comment-lines)

      (spacemacs/set-leader-keys
        ";"  'evilnc-comment-operator
        "cl" 'spacemacs/comment-or-uncomment-lines
        "cL" 'spacemacs/comment-or-uncomment-lines-inverse
        "cp" 'spacemacs/comment-or-uncomment-paragraphs
        "cP" 'spacemacs/comment-or-uncomment-paragraphs-inverse
        "ct" 'spacemacs/quick-comment-or-uncomment-to-the-line
        "cT" 'spacemacs/quick-comment-or-uncomment-to-the-line-inverse
        "cy" 'spacemacs/copy-and-comment-lines
        "cY" 'spacemacs/copy-and-comment-lines-inverse))))

(defun evil-l/init-evil-mc ()
  (use-package evil-mc
    :defer t
    :init
    ;; remove emc prefix when there is not multiple cursors
    (setq evil-mc-mode-line
          `(:eval (when (> (evil-mc-get-cursor-count) 1)
                    (format ,(propertize " %s:%d" 'face 'cursor)
                            evil-mc-mode-line-prefix
                            (evil-mc-get-cursor-count)))))
    (global-evil-mc-mode 1)
    ))

;;; packages.el ends here
