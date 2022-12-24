;;; slam-mode-el -- Major mode for editing WPDL files

;; Author: prestosilver
;; Created: 25 Sep 2000

;;; Commentary:
;;
;; This mode is an example used in a tutorial about Emacs
;; mode creation. The tutorial can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html

;;; Code:
(defvar slam-mode-hook nil)
(defvar slam-mode-map
  (let ((slam-mode-map (make-keymap)))
    (define-key slam-mode-map "\C-j" 'newline-and-indent)
    slam-mode-map)
  "Keymap for SLAM major mode.")

;;;###autoload
;(add-to-list 'auto-mode-alist '("\\.slm\\'" . slam-mode))

(defconst slam-font-lock-keywords-1
  (list
   '("\\<\\(sim\\|lambda\\|nop\\|swap\\|dump\\|copy\\|covr\\|readc\\|read\\|putc\\|put\\|disc\\|argv\\|argc\\|envp\\|const\\|quit\\|if\\|proc\\|do\\|end\\|inc\\|enum\\|var\\|gvar\\|prop\\|class\\|cproc\\|lnk\\)\\>" . font-lock-builtin-face)
  )
  "Minimal highlighting expressions for SLAM mode.")

(defconst slam-font-lock-keywords-2
  (append wpdl-font-lock-keywords-1
    (list
     '("\\<\\(brk\\|macro\\|ret\\|asm\\|of\\|temp\\|push\\|oper\\|-\\|*\\|,\\|/%\\|+\\|^\\|!\\|!=\\|==\\|<\\|>\\|&&\\|||\\|()\\|sys0\\|sys1\\|sys2\\|sys3\\|sys4\\|sys5\\|sys6\\|[\\|]\\|\\)\\>" . font-lock-builtin-face)
    )
  )
  "Minimal highlighting expressions for SLAM mode.")


(defvar slam-font-lock-keywords slam-font-lock-keywords-2
  "Default highlighting expressions for SLAM mode.")

(defvar slam-mode-syntax-table
  (let ((slam-mode-syntax-table (make-syntax-table)))
    ; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" slam-mode-syntax-table)

    ; Comment styles are same as C++
    (modify-syntax-entry ?/ ". 124b" slam-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" slam-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" slam-mode-syntax-table)
    slam-mode-syntax-table)
  "Syntax table for slam-mode.")

(defun slam-mode ()
  "Major mode for editing slam files."
  (interactive)
  (use-local-map slam-mode-map)
  (set-syntax-table slam-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(slam-font-lock-keywords))
  (setq major-mode 'slam-mode)
  (setq mode-name "SLAM")
  (run-hooks 'slam-mode-hook))

(provide 'slam-mode)

;;; slam-mode.el ends here

