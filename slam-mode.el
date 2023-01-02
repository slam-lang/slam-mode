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
  (let ((smap (make-keymap)))
    (define-key smap "\C-j" 'newline-and-indent)
    smap)
  "Keymap for SLAM major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slm\\'" . slam-mode))

(defconst slam-font-lock-keywords-1
  (list
   '("\\<\\(sim\\|lambda\\|nop\\|swap\\|dump\\|copy\\|covr\\|readc\\|read\\|putc\\|put\\|disc\\|argv\\|argc\\|envp\\|const\\|quit\\|if\\|proc\\|do\\|end\\|inc\\|enum\\|var\\|gvar\\|prop\\|class\\|cproc\\|lnk\\)\\>" . font-lock-builtin-face)
  )
  "Minimal highlighting expressions for SLAM mode.")
(defconst slam-font-lock-keywords-2
  (append slam-font-lock-keywords-1
    (list
     '("\\<\\(brk\\|macro\\|ret\\|asm\\|of\\|temp\\|push\\|oper\\|-\\|*\\|,\\|/%\\|+\\|^\\|!\\|!=\\|==\\|<\\|>\\|&&\\|||\\|()\\|sys0\\|sys1\\|sys2\\|sys3\\|sys4\\|sys5\\|sys6\\|[\\|]\\|\\)\\>" . font-lock-builtin-face)
    )
  )
  "Minimal highlighting expressions for SLAM mode.")
(defvar slam-font-lock-keywords slam-font-lock-keywords-2
  "Default highlighting expressions for SLAM mode.")


;(defconst slam-font-lock-keywords-1
;  (list
;   '("\\<\\(argv?\\|co\\(?:py\\|vr\\)\\|d\\(?:isc\\|ump\\)\\|lambda\\|nop\\|putc?\\|readc?\\|s\\(?:im\\|wap\\)\\)\\>" . font-lock-builtin-face)
;   '("\\<\\(proc\\)\\>" . font-lock-constant-face)
;   '("\\('\\w*'\\)" . font-lock-variable-name-face)
;  )
;  "Minimal highlighting expressions for SLAM mode.")

; (regexp-opt '("sim" "lambda" "nop" "swap" "dump" "copy"
; "covr" "readc" "read" "putc" "put" "disc" "argv" "argc"
; "envp" "sys0" "sys1" "sys2" "sys3") t)
; ".\\(argv?\\|co\\(?:py\\|vr\\)\\|d\\(?:isc\\|ump\\)\\|lambda\\|nop\\|putc?\\|readc?\\|s\\(?:im\\|wap\\)\\)"

(defvar slam-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" syntax-table)

    ; Comment styles are same as C++
    (modify-syntax-entry ?{ "<" syntax-table)
    (modify-syntax-entry ?} ">" syntax-table)
    syntax-table)
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

(defun org-babel-execute:slam (body params)
  "Execute a block of Slam code with org-babel."
  (let ((in-file (org-babel-temp-file "s" ".slm"))
       (verbosity (or (cdr (assq :verbosity params)) 0)))
    (with-temp-file in-file
      (insert body))
    (let ((out-file (org-babel-temp-file "s" ""))
         (verbosity (or (cdr (assq :verbosity params)) 0)))
    (org-babel-eval
      (format "slam -o %s %s > /dev/null; chmod +X %s; %s"
              (org-babel-process-file-name out-file)
              (org-babel-process-file-name in-file)
              (org-babel-process-file-name out-file)
              (org-babel-process-file-name out-file))
      ""))))


(provide 'org-babel-execute:slam)
(provide 'slam-mode)
;;; slam-mode.el ends here
