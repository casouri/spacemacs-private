(defvar spacemacs-default-company-backends
  '((company-dabbrev-code company-gtags company-etags company-keywords)
    company-files company-dabbrev)
  "The list of default company backends used by spacemacs.
This variable is used to configure mode-specific company backends in spacemacs.
Backends in this list will always be active in these modes, as well as any
backends added by individual spacemacs layers.")

(defvar complete-l-private-snippets-directory nil
  "Configurable private snippets directory.")

(defvar complete-l-enable-sort-by-usage nil
  "If non nil suggestions are sorted by how often they are used.")

(defvar complete-l-enable-snippets-in-popup nil
  "If non nil show snippets in the complete-l popup.")

(defvar complete-l-enable-help-tooltip nil
  "If non nil the docstring appears in a tooltip.
If set to `manual', help tooltip appears only when invoked
manually.")
(defvar complete-l-tab-key-behavior 'cycle
  "What the TAB key should do when complete-l menu is active.
Possible values are `complete', `cycle' or `nil'.")

(defvar complete-l-return-key-behavior 'complete
  "What the RET key should do when complete-l menu is active.
Possible values are `complete' or `nil'.")
