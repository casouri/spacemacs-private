(defconst lsp-packages
  '(
    (company-lsp :requires company)
    (ivy-xref :requires ivy)
    lsp-mode
    lsp-ui
    ))

(defun lsp/init-company-lsp ()
  (use-package company-lsp
    :after company
    :init
    ;; Language servers have better idea filtering and sorting,
    ;; don't filter results on the client side.
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil)
    (spacemacs|add-company-backends :backends company-lsp)
    ))

(defun lsp/init-ivy-xref ()
  (use-package ivy-xref
    :config
    (setq xref-prompt-for-identifier
          '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references spacemacs/jump-to-definition))

    ;; Use ivy-xref to display xref.el results.
    (setq xref-show-xrefs-function 'ivy-xref-show-xrefs)
    ))

(defun lsp/init-lsp-mode ()
  (use-package lsp-mode
    :config
    (require 'lsp-imenu)
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
    ;; Disable lsp-flycheck.el in favor of lsp-ui-flycheck.el
    (setq lsp-enable-flycheck nil)
    ))

(defun lsp/init-lsp-ui ()
  (use-package lsp-ui
    :after lsp-mode
    :after markdown-mode
    :config
    (add-hook 'lsp-mode-hook #'lsp-ui-mode)
    ;; imenu
    (spacemacs/set-leader-keys-for-minor-mode 'lsp-ui-mode (kbd "i") #'lsp-ui-imenu)
    ;; bind peek key
    (define-key lsp-ui-mode-map (kbd "M-.") #'xref-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
    ;; set peek color face
    (sync-peek-face)
    (add-hook 'spacemacs-post-theme-change-hook #'sync-peek-face)
    (spacemacs|diminish lsp-mode " Ⓛ" " L")
    ))

