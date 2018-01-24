;;; packages.el --- complete-l layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Yuan Fu <yuan@missSilver>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst complete-l-packages
  '(company
    yasnippet
    (company-quickhelp :toggle complete-l-enable-help-tooltip)
    auto-yasnippet
    ))

(defun complete-l/init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq company-idle-delay 0.2
            company-minimum-prefix-length 2
            company-require-match nil
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil)
      (global-company-mode 1)
    :config
    (spacemacs|diminish company-mode " ⓐ" " a")
    (setup-company-map)
    )))

(defun complete-l/init-company-quickhelp ()
  (use-package company-quickhelp
    :commands company-quickhelp-manual-begin
    :init
    (spacemacs|do-after-display-system-init
     (with-eval-after-load 'company
       (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))
       (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
       (unless (eq complete-l-enable-help-tooltip 'manual)
         (company-quickhelp-mode))))))

(defun complete-l/init-yasnippet ()
  (use-package yasnippet
    :commands (yas-global-mode yas-minor-mode)
    :init
    (progn
      ;; We don't want undefined variable errors
      (defvar yas-global-mode nil)
      (setq yas-triggers-in-field t
            yas-wrap-around-region t
            helm-yas-display-key-on-candidate t)
      ;; on multiple keys, fall back to completing read
      ;; typically this means helm
      (setq yas-prompt-functions '(yas-completing-prompt))
      ;; disable yas minor mode map
      ;; use hippie-expand instead
      (setq yas-minor-mode-map (make-sparse-keymap))
      ;; this makes it easy to get out of a nested expansion
      (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
      ;; configure snippet directories
      (let* ((spacemacs--complete-l-dir
              (configuration-layer/get-layer-local-dir 'complete-l))
             (private-yas-dir (if complete-l-private-snippets-directory
                                  complete-l-private-snippets-directory
                                (concat
                                 configuration-layer-private-directory
                                 "snippets/")))
             (spacemacs-layer-snippets-dir (expand-file-name
                                      "snippets"
                                      spacemacs--complete-l-dir))
             (dotspacemacs-directory-snippets-dir (when dotspacemacs-directory
                                                    (expand-file-name
                                                     "snippets"
                                                     dotspacemacs-directory))))
        (setq yas-snippet-dirs nil)
        ;; ~/.emacs.d/layers/complete-l/snippets
        (push spacemacs-layer-snippets-dir yas-snippet-dirs)
        ;; ~/.emacs.d/elpa/yasnippet-xxxxx/snippets
        (push 'yas-installed-snippets-dir yas-snippet-dirs)
        ;; ~/.spacemacs.d/snippets
        (when dotspacemacs-directory-snippets-dir
          (push dotspacemacs-directory-snippets-dir yas-snippet-dirs))
        ;; arbitrary directories in `complete-l-private-snippets-directory'
        (when private-yas-dir
          (if (listp private-yas-dir)
              (setq yas-snippet-dirs (append yas-snippet-dirs private-yas-dir))
            (push private-yas-dir yas-snippet-dirs))))

      (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(prog-mode-hook
                                                          markdown-mode-hook
                                                          org-mode-hook))
      (spacemacs|add-toggle yasnippet
        :mode yas-minor-mode
        :documentation "Enable snippets."
        :evil-leader "ty")

      (spacemacs/add-to-hooks
       'spacemacs/force-yasnippet-off '(term-mode-hook
                                        shell-mode-hook
                                        eshell-mode-hook)))
    :config (spacemacs|diminish yas-minor-mode " ⓨ" " y")))

(defun complete-l/init-auto-yasnippet ()
  (use-package auto-yasnippet
    :defer t
    :init
    (progn
      (setq aya-persist-snippets-dir
            (or complete-l-private-snippets-directory
                (concat configuration-layer-private-directory "snippets/")))
      (spacemacs/declare-prefix "iS" "auto-yasnippet")
      (spacemacs/set-leader-keys
        "iSc" 'aya-create
        "iSe" 'spacemacs/auto-yasnippet-expand
        "iSw" 'aya-persist-snippet))))
;;; packages.el ends here
