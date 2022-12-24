;;; slam-mode-el -- Major mode for editing WPDL files

;; Author: prestosilver
;; Created: 25 Sep 2000
;; Keywords: SLAM major-mode

;;; Commentary:
;;
;; This mode is an example used in a tutorial about Emacs
;; mode creation. The tutorial can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html

;;; Code:
(defvar slam-mode-hook nil)
(defvar slam-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Slam-mode map.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slm\\'" . slam-mode))

(defconst slam-font-lock-keywords-1
  (list
   '("\\<\\(co\\(?:py\\|vr\\)\\|dump\\|lambda\\|nop\\|\\|s\\(?:im\\|wap\\)\\)\\>" . font-lock-builtin-face)
   '("" . font-lock-variable-name-face))
  "Minimal highlighting expressions for SLAM mode.")

(defvar slam-font-lock-keywords slam-font-lock-keywords-1
  "Default highlighting expressions for SLAM mode.")

(defvar slam-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for slam-mode.")

(defun slam-mode ()
  "Major mode for editing slam files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table slam-mode-syntax-table)
  (use-local-map slam-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(slam-font-lock-keywords))
  (setq major-mode 'slam-mode)
  (setq mode-name "SLAM")
  (run-hooks 'slam-mode-hook))

(provide 'slam-mode)
;;; slam-mode.el ends here
