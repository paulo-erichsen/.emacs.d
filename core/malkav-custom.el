(defcustom malkav-flyspell t
  "Non-nil values enable Malkav's flyspell support."
  :type 'boolean
  :group 'malkav)

(defcustom malkav-guru t
  "Non-nil values enable `guru-mode'."
  :type 'boolean
  :group 'malkav)

(defcustom malkav-whitespace t
  "Non-nil values enable Malkav's whitespace visualization."
  :type 'boolean
  :group 'malkav)

(defcustom malkav-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `malkav-whitespace' is also enabled."
  :type 'boolean
  :group 'malkav)

(provide 'malkav-custom)
