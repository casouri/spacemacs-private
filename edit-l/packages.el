;;; packages.el --- edit-l layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Yuan Fu <yuan@missSilver>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defconst edit-l-packages
  '(
    avy
    expand-region
    origami
    winum
    (smooth-scrolling :location built-in)
    (spacemacs-whitespace-cleanup :location local)
    undo-tree
    ))

(defun edit-l/init-avy ()
  (use-package avy
    :defer t
    :commands (spacemacs/avy-open-url spacemacs/avy-goto-url avy-pop-mark)
    :init
    (progn
      (setq avy-all-windows 'all-frames)
      (setq avy-background t)
      (spacemacs/set-leader-keys
        "jb" 'avy-pop-mark
        "jj" 'evil-avy-goto-char
        "jJ" 'evil-avy-goto-char-2
        "jl" 'evil-avy-goto-line
        "ju" 'spacemacs/avy-goto-url
        "jw" 'evil-avy-goto-word-or-subword-1
        "xo" 'spacemacs/avy-open-url))
    :config
    (progn
      (defun spacemacs/avy-goto-url()
        "Use avy to go to an URL in the buffer."
        (interactive)
        (avy--generic-jump "https?://" nil 'pre))
      (defun spacemacs/avy-open-url ()
        "Use avy to select an URL in the buffer and open it."
        (interactive)
        (save-excursion
          (spacemacs/avy-goto-url)
          (browse-url-at-point))))))

(defun edit-l/init-expand-region ()
  (use-package expand-region
    :defer t
    :init (spacemacs/set-leader-keys "v" 'er/expand-region)
    :config
    (progn
      ;; add search capability to expand-region
      (when (configuration-layer/package-used-p 'helm-ag)
        (defadvice er/prepare-for-more-expansions-internal
            (around helm-ag/prepare-for-more-expansions-internal activate)
          ad-do-it
          (let ((new-msg (concat (car ad-return-value)
                                 ", / to search in project, "
                                 "f to search in files, "
                                 "b to search in opened buffers"))
                (new-bindings (cdr ad-return-value)))
            (cl-pushnew
             '("/" (lambda ()
                     (call-interactively
                      'spacemacs/helm-project-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("f" (lambda ()
                     (call-interactively
                      'spacemacs/helm-files-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("b" (lambda ()
                     (call-interactively
                      'spacemacs/helm-buffers-smart-do-search-region-or-symbol)))
             new-bindings)
            (setq ad-return-value (cons new-msg new-bindings)))))
      (setq expand-region-contract-fast-key "V"
            expand-region-reset-fast-key "r"))))

(defun edit-l/init-origami ()
  (use-package origami
    :defer t
    :init
    (progn
      (global-origami-mode)
      (define-key evil-normal-state-map "za" 'origami-forward-toggle-node)
      (define-key evil-normal-state-map "zc" 'origami-close-node)
      (define-key evil-normal-state-map "zC" 'origami-close-node-recursively)
      (define-key evil-normal-state-map "zO" 'origami-open-node-recursively)
      (define-key evil-normal-state-map "zo" 'origami-open-node)
      (define-key evil-normal-state-map "zr" 'origami-open-all-nodes)
      (define-key evil-normal-state-map "zm" 'origami-close-all-nodes)
      (define-key evil-normal-state-map "zs" 'origami-show-only-node)
      (define-key evil-normal-state-map "zn" 'origami-next-fold)
      (define-key evil-normal-state-map "zp" 'origami-previous-fold)
      (define-key evil-normal-state-map "zR" 'origami-reset)
      (define-key evil-normal-state-map (kbd "z <tab>") 'origami-recursively-toggle-node)
      (define-key evil-normal-state-map (kbd "z TAB") 'origami-recursively-toggle-node)

      (spacemacs|define-transient-state fold
        :title "Code Fold Transient State"
        :doc "
 Close^^            Open^^             Toggle^^         Goto^^         Other^^
 ───────^^───────── ─────^^─────────── ─────^^───────── ──────^^────── ─────^^─────────
 [_c_] at point     [_o_] at point     [_a_] at point   [_n_] next     [_s_] single out
 [_C_] recursively  [_O_] recursively  [_A_] all        [_p_] previous [_R_] reset
 [_m_] all          [_r_] all          [_TAB_] like org ^^             [_q_] quit"
        :foreign-keys run
        :on-enter (unless (bound-and-true-p origami-mode) (origami-mode 1))
        :bindings
        ("a" origami-forward-toggle-node)
        ("A" origami-toggle-all-nodes)
        ("c" origami-close-node)
        ("C" origami-close-node-recursively)
        ("o" origami-open-node)
        ("O" origami-open-node-recursively)
        ("r" origami-open-all-nodes)
        ("m" origami-close-all-nodes)
        ("n" origami-next-fold)
        ("p" origami-previous-fold)
        ("s" origami-show-only-node)
        ("R" origami-reset)
        ("TAB" origami-recursively-toggle-node)
        ("<tab>" origami-recursively-toggle-node)
        ("q" nil :exit t)
        ("C-g" nil :exit t)
        ("<SPC>" nil :exit t))
      ;; Note: The key binding for the fold transient state is defined in
      ;; evil config
      )))

(defun edit-l/init-winum ()
  (use-package winum
    :config
    (progn
      (setq winum-auto-assign-0-to-minibuffer nil
            winum-auto-setup-mode-line nil
            winum-ignored-buffers '(" *which-key*"))
      (spacemacs/set-leader-keys
        "`" 'winum-select-window-by-number
        "²" 'winum-select-window-by-number
        "0" 'winum-select-window-0-or-10
        "1" 'winum-select-window-1
        "2" 'winum-select-window-2
        "3" 'winum-select-window-3
        "4" 'winum-select-window-4
        "5" 'winum-select-window-5
        "6" 'winum-select-window-6
        "7" 'winum-select-window-7
        "8" 'winum-select-window-8
        "9" 'winum-select-window-9)
      (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
      (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
      (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
      (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
      (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
      (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
      (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
      (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
      (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
      (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)
      (winum-mode))))

(defun edit-l/init-smooth-scrolling ()
  (setq scroll-preserve-screen-position t
        scroll-margin 0
        scroll-conservatively (if dotspacemacs-smooth-scrolling 101 0))
  (spacemacs|add-toggle smooth-scrolling
    :status (= 101 scroll-conservatively)
    :on (spacemacs/enable-smooth-scrolling)
    :off (spacemacs/disable-smooth-scrolling)
    :documentation "Smooth scrolling."
    :evil-leader "tv"))

(defun edit-l/init-spacemacs-whitespace-cleanup ()
  (use-package spacemacs-whitespace-cleanup
    :commands (spacemacs-whitespace-cleanup-mode
               global-spacemacs-whitespace-cleanup-mode)
    :init
    (progn
      (spacemacs|add-toggle whitespace-cleanup
        :mode spacemacs-whitespace-cleanup-mode
        :documentation "Automatic whitespace clean up."
        :on-message (spacemacs-whitespace-cleanup/on-message)
        :evil-leader "tW")
      (spacemacs|add-toggle global-whitespace-cleanup
        :mode global-spacemacs-whitespace-cleanup-mode
        :status spacemacs-whitespace-cleanup-mode
        :on (let ((spacemacs-whitespace-cleanup-globally t))
              (spacemacs-whitespace-cleanup-mode))
        :off (let ((spacemacs-whitespace-cleanup-globally t))
               (spacemacs-whitespace-cleanup-mode -1))
        :on-message (spacemacs-whitespace-cleanup/on-message t)
        :documentation "Global automatic whitespace clean up."
        :evil-leader "t C-S-w")
      (with-eval-after-load 'ws-butler
        (when dotspacemacs-whitespace-cleanup
          (spacemacs/toggle-global-whitespace-cleanup-on))))
    :config
    (progn
      (spacemacs|diminish spacemacs-whitespace-cleanup-mode " Ⓦ" " W")
      (spacemacs|diminish global-spacemacs-whitespace-cleanup-mode
                          " Ⓦ" " W"))))

(defun edit-l/init-undo-tree ()
  (use-package undo-tree
    :init
    (progn
      (global-undo-tree-mode)
      (setq undo-tree-visualizer-timestamps t
            undo-tree-visualizer-diff t))
    :config
    (progn
      ;; restore diff window after quit.  TODO fix upstream
      (defun spacemacs/undo-tree-restore-default ()
        (setq undo-tree-visualizer-diff t))
      (advice-add 'undo-tree-visualizer-quit :after #'spacemacs/undo-tree-restore-default)
      (spacemacs|hide-lighter undo-tree-mode)
      (evilified-state-evilify-map undo-tree-visualizer-mode-map
        :mode undo-tree-visualizer-mode
        :bindings
        (kbd "j") 'undo-tree-visualize-redo
        (kbd "k") 'undo-tree-visualize-undo
        (kbd "h") 'undo-tree-visualize-switch-branch-left
        (kbd "l") 'undo-tree-visualize-switch-branch-right))
    (spacemacs|hide-lighter undo-tree-mode)
    ))


;;; packages.el ends here
