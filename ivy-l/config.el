
;; Private Variables

(defvar spacemacs--counsel-commands
  '(;; --line-number forces line numbers (disabled by default on windows)
    ;; no --vimgrep because it adds column numbers that wgrep can't handle
    ;; see https://github.com/syl20bnr/spacemacs/pull/8065
    ("rg" . "rg --smart-case --no-heading --color never --line-number --max-columns 150 %s %S .")
    ("grep" . "grep -nrP %s %S ."))
  "An alist of search commands and their corresponding commands
with options to run in the shell.")

(defvar spacemacs--counsel-search-max-path-length 30
  "Truncate the current path in counsel search if it is longer
than this amount.")

(defvar spacemacs--counsel-initial-number-cand 100)

(defvar spacemacs--ivy-file-actions
  '(("f" find-file-other-frame "other frame")
    ("j" find-file-other-window "other window")
    ("v" spacemacs/find-file-vsplit "in vertical split")
    ("s" spacemacs/find-file-split "in horizontal split")
    ("l" find-file-literally "literally")
    ("d" spacemacs/delete-file-confirm "delete file")
    ("r" spacemacs/rename-file "rename file"))
  "Default ivy actions for files.")

