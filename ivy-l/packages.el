;;; packages.el --- ivy-l layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Yuan Fu <yuan@missSilver>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ivy-l-packages
  '(
    evil
    ivy
    ivy-hydra
    counsel
    (counsel-projectile :requires projectile)
    (ivy-spacemacs-help :location local)
    projectile
    recentf
    smex
    swiper
    )
  )


(defun ivy-l/init-counsel ()
  (use-package counsel
    :init
    (progn
      (spacemacs/set-leader-keys
        dotspacemacs-emacs-command-key 'counsel-M-x
        ;; files
        "ff"  'counsel-find-file
        "fel" 'counsel-find-library
        "fL"  'counsel-locate
        ;; help
        "?"   'counsel-descbinds
        "hdf" 'counsel-describe-function
        "hdF" 'counsel-describe-face
        "hdm" 'spacemacs/describe-mode
        "hdv" 'counsel-describe-variable
        "hi"  'counsel-info-lookup-symbol
        "hR"  'spacemacs/counsel-search-docs
        ;; insert
        "iu"  'counsel-unicode-char
        ;; jump
        ;; register/ring
        "ry"  'counsel-yank-pop
        "rm"  'counsel-mark-ring
        ;; jumping
        "sj"  'counsel-imenu
        ;; themes
        "Ts"  'counsel-load-theme
        ))
    :config
    (progn
      ;; set additional ivy actions
      (ivy-set-actions
       'counsel-find-file
       spacemacs--ivy-file-actions)

      (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
      ;; remaps built-in commands that have a counsel replacement
      (counsel-mode 1)
      (spacemacs|hide-lighter counsel-mode)
      ;; Set syntax highlighting for counsel search results
      (ivy-set-display-transformer 'spacemacs/counsel-search 'counsel-git-grep-transformer))))

(defun ivy-l/post-init-evil ()
  (spacemacs/set-leader-keys
    "re" 'spacemacs/ivy-evil-registers))

(defun ivy-l/pre-init-counsel-projectile ()
  ;; overwrite projectile settings
  (spacemacs|use-package-add-hook projectile
    :post-init
    (progn
      (setq projectile-switch-project-action 'counsel-projectile-find-file)

      (ivy-set-actions
       'counsel-projectile-find-file
       (append spacemacs--ivy-file-actions
               '(("R" (lambda (arg)
                        (interactive)
                        (call-interactively
                         #'projectile-invalidate-cache)
                        (ivy-resume)) "refresh list")
                 )))

      (spacemacs/set-leader-keys
        "p SPC" 'counsel-projectile
        ;; search
        "pb"    'counsel-projectile-switch-to-buffer
        "pd"    'counsel-projectile-find-dir
        "pp"    'counsel-projectile-switch-project
        "pf"    'counsel-projectile-find-file))))

(defun ivy-l/init-counsel-projectile ()
  (use-package counsel-projectile :defer t))

(defun ivy-l/post-init-imenu ()
  (spacemacs/set-leader-keys "ji" 'counsel-imenu))

(defun ivy-l/init-ivy ()
  (use-package ivy
    :init
    (progn
      ;; Key bindings
      (spacemacs/set-leader-keys
        "a'" 'spacemacs/ivy-available-repls
        "fr" 'counsel-recentf
        "rl" 'ivy-resume
        "bb" 'ivy-switch-buffer))

    :config
    (progn
      ;; custom actions for recentf
      (ivy-set-actions
       'counsel-recentf
       spacemacs--ivy-file-actions)

      ;; mappings to quit minibuffer or enter transient state
      (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
      (define-key ivy-minibuffer-map (kbd "M-SPC") 'hydra-ivy/body)

      (ivy-mode 1)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "<f6>") 'ivy-resume)
      ;; Occur
      (evil-make-overriding-map ivy-occur-mode-map 'normal)
      (ivy-set-occur 'spacemacs/counsel-search
                     'spacemacs//counsel-occur)
      (spacemacs/set-leader-keys-for-major-mode 'ivy-occur-grep-mode
        "w" 'ivy-wgrep-change-to-wgrep-mode)
      ;; Why do we do this ?
      (ido-mode -1)

      ;; allow to select prompt in some ivy functions
      (setq ivy-use-selectable-prompt t))))

(defun ivy-l/init-ivy-hydra ()
  (use-package ivy-hydra)
  (define-key hydra-ivy/keymap [escape] 'hydra-ivy/keyboard-escape-quit-and-exit))

(defun ivy/init-ivy-rich ()
  (use-package ivy-rich
    :defer t
    :init
    (progn
      (setq ivy-rich-abbreviate-paths t
            ivy-virtual-abbreviate 'full
            ivy-rich-switch-buffer-align-virtual-buffer t)
      (ivy-set-display-transformer 'ivy-switch-buffer
                                   'ivy-rich-switch-buffer-transformer))))

(defun ivy-l/init-ivy-spacemacs-help ()
  (use-package ivy-spacemacs-help
    :commands (ivy-spacemacs-help-dotspacemacs
               ivy-spacemacs-help
               ivy-spacemacs-help-faq
               ivy-spacemacs-help-layers
               ivy-spacemacs-help-packages
               ivy-spacemacs-help-docs
               ivy-spacemacs-help-toggles)
    :init (spacemacs/set-leader-keys
            "h ."   'ivy-spacemacs-help-dotspacemacs
            "h SPC" 'ivy-spacemacs-help
            "h f"   'ivy-spacemacs-help-faq
            "h l"   'ivy-spacemacs-help-layers
            "h p"   'ivy-spacemacs-help-packages
            "h r"   'ivy-spacemacs-help-docs
            "h t"   'ivy-spacemacs-help-toggles)))

(defun ivy-l/post-init-projectile ()
  (setq projectile-completion-system 'ivy)
  (spacemacs/set-leader-keys
    "pv"  'projectile-vc))

(defun ivy-l/post-init-recentf ()
  ;; custom actions for recentf
  (ivy-set-actions
   'counsel-recentf
   (append spacemacs--ivy-file-actions
           '(("R" (lambda (arg)
                    (interactive)
                    (recentf-cleanup)
                    (ivy-recentf)) "refresh list")
             ("D" (lambda (arg)
                    (interactive)
                    (setq recentf-list (delete arg recentf-list))
                    (ivy-recentf)) "delete from list"))))
  ;; merge recentf and bookmarks into buffer switching. If we set this
  (setq ivy-use-virtual-buffers t))


(defun ivy-l/init-smex ()
  (use-package smex
    :defer t
    :init (setq-default smex-history-length 32
                        smex-save-file (concat spacemacs-cache-directory
                                               ".smex-items"))))

(defun ivy-l/init-swiper ()
  (use-package swiper
    :config
    (progn
      (spacemacs/set-leader-keys
        "ss" 'swiper
        )
      (global-set-key "\C-s" 'swiper))))


;;; packages.el ends here
