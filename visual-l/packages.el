;;; packages.el --- visual-l layer packages file for Spacemacs.
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
;; added to `visual-l-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `visual-l/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `visual-l/pre-init-PACKAGE' and/or
;;   `visual-l/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst visual-l-packages
  '(rainbow-delimiters
    highlight-parentheses
    ))


(defun visual-l/init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "tCd" 'rainbow-delimiters-mode)
      (when (member dotspacemacs-highlight-delimiters '(any all))
        (spacemacs/add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook))))))

(defun visual-l/init-highlight-parentheses ()
  (use-package highlight-parentheses
    :defer t
    :init
    (progn
      (when (member dotspacemacs-highlight-delimiters '(all current))
        (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
      (setq hl-paren-delay 0.2)
      (spacemacs/set-leader-keys "tCp" 'highlight-parentheses-mode)
      (setq hl-paren-colors '("Springgreen3"
                              "IndianRed1"
                              "IndianRed3"
                              "IndianRed4")))
    :config
    (spacemacs|hide-lighter highlight-parentheses-mode)
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)))

;;; packages.el ends here
