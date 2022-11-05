;;; ruri.el --- A thin layer over mode-line -*- lexical-binding: t; -*-
;; Author: vekatze <vekatze@icloud.com>
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: ISC
;; URL: https://github.com/vekatze/ruri.el
;; Version: 1.0.0
;;; Commentary:
;; A thin layer over mode-line.
;;; Code:

(require 'vc-git)

;;
;; global variables
;;

(defgroup ruri nil
  "The customization group for ruri."
  :group 'mode-line)

(defcustom ruri-separator "  "
  "The separator that is inserted between segments."
  :group 'ruri
  :type 'string)

(defcustom ruri-summary-separator " › "
  "The separator that is inserted between buffer summary entities."
  :group 'ruri
  :type 'string)

(defcustom ruri-buffer-format "%b"
  "The format string for the last part of a buffer summary."
  :group 'ruri
  :type 'string)

(defcustom ruri-checker-separator " "
  "The separator that is inserted between flycheck/flymake entities."
  :group 'ruri
  :type 'string)

(defcustom ruri-checker-format "•%d"
  "The format string that is used when displaying a flycheck/flymake entity.
`%d' stands for the number of errors."
  :group 'ruri
  :type 'string)

(defcustom ruri-lsp-separator " "
  "The separator that is inserted between LSP servers."
  :group 'ruri
  :type 'string)

(defcustom ruri-mode-line-height-zoom 1.4
  "The height zoom of ruri-mode-lines."
  :group 'ruri
  :type 'integer)

(defvar ruri--segment--format "ruri--segment:%s"
  "The format string that is used when defining/using an ruri segment.")

(defface ruri-summary-prefix
  '((t (:inherit font-lock-type-face)))
  "Face for the prefix part of a buffer summary string."
  :group 'ruri)

(defface ruri-summary-secondary
  '((t (:inherit shadow)))
  "Face for the secondary part of a buffer summary string."
  :group 'ruri)

(defface ruri-summary-body
  '((t (:weight bold)))
  "Face for the body part of a buffer summary string."
  :group 'ruri)

(defface ruri-major-mode
  '((t (:inherit font-lock-string-face)))
  "Face for the major mode string in the ruri mode-line."
  :group 'ruri)

(defface ruri-lsp
  '((t (:inherit font-lock-constant-face)))
  "Face for the LSP server string in the ruri mode-line."
  :group 'ruri)

(defface ruri-encoding
  '((t (:inherit font-lock-keyword-face)))
  "Face for the buffer encoding string in the ruri mode-line."
  :group 'ruri)

(defface ruri-tramp-method
  '((t (:inherit ruri-summary-prefix)))
  "Face for the TRAMP method name in the ruri mode-line."
  :group 'ruri)

(defface ruri-keyboard-macro
  '((t (:inherit font-lock-builtin-face)))
  "Face for keyboard macro information."
  :group 'ruri)

(defface ruri-checker-info
  '((t ()))
  "Face for flycheck/flymake info feedback in the ruri mode-line."
  :group 'ruri)

(defface ruri-checker-warning
  '((t ()))
  "Face for flycheck/flymake warning feedback in the ruri mode-line."
  :group 'ruri)

(defface ruri-checker-error
  '((t ()))
  "Face for flycheck/flymake error feedback in the ruri mode-line."
  :group 'ruri)

(defface ruri-eglot
  '((t (:inherit font-lock-builtin-face)))
  "Face for eglot info in the ruri mode-line."
  :group 'ruri)

;;
;; main
;;

(defun ruri--get-segment-name (key)
  "Construct the name of the segment using KEY."
  (intern (format ruri--segment--format key)))

(defmacro define-ruri-segment (name &rest body)
  "Define a ruri segment by specifing its NAME and BODY."
  (declare (indent defun))
  `(defun ,(ruri--get-segment-name name) () ,@body))

(defun ruri--render-segment (segment)
  "Render given SEGMENT."
  (funcall segment))

(defun ruri--render (segment-list)
  "Render the list of segments SEGMENT-LIST."
  (string-join (remove nil (mapcar #'ruri--render-segment segment-list)) ruri-separator))

(defun ruri--calculate-raise ()
  "Calculate the `raise' value of the mode-line."
  (* (/ (* ruri-mode-line-height-zoom 2.0) 15) -1))

(define-ruri-segment empty
  (propertize "\u200b" 'display `((height ,ruri-mode-line-height-zoom) (raise ,(ruri--calculate-raise)))))

(define-ruri-segment modified
  "%*")

(define-ruri-segment size
  "%i")

(defun ruri--get-vc-root-dir ()
  "Get project root directory."
  (when-let ((root-dir (vc-root-dir)))
    (expand-file-name root-dir)))

(defun ruri--tramp-method ()
  "Get the method name of TRAMP."
  (when-let ((method-name (file-remote-p default-directory 'method)))
    (propertize
     (concat
      method-name
      ruri-summary-separator)
     'face
     'ruri-tramp-method)))

(defun ruri--get-vc-currect-git-branch ()
  "Get the name of current branch."
  (when (vc-root-dir)
    (car (vc-git-branches))))

(defun ruri--summary-project-name ()
  "Get the propertized name of current project."
  (when-let ((current-project-root (ruri--get-vc-root-dir)))
    (propertize
     (concat
      (file-name-base (directory-file-name current-project-root))
      ruri-summary-separator)
     'face
     'ruri-summary-prefix)))

(defun ruri--summary-git-branch-name ()
  "Get the propertized name of current branch."
  (when-let ((current-git-branch (ruri--get-vc-currect-git-branch)))
    (propertize
     (concat
      current-git-branch
      ruri-summary-separator)
     'face
     'ruri-summary-prefix)))

(defun ruri--summary-relative-path-from-project-root ()
  "Get the project-relative path of current file."
  (when-let ((project-root-dir (ruri--get-vc-root-dir))
             (file-name (buffer-file-name))
             (relative-path (file-name-directory (file-relative-name file-name project-root-dir))))
    (propertize relative-path 'face 'ruri-summary-secondary)))

(defun ruri--summary-primary-name ()
  "Get the file name or the buffer name."
  (propertize
   (if-let ((file-name (buffer-file-name)))
       (file-name-nondirectory file-name)
     (buffer-name))
   'face
   'ruri-summary-body))

(define-ruri-segment summary
  (concat
   (ruri--tramp-method)
   (ruri--summary-project-name)
   (ruri--summary-git-branch-name)
   (ruri--summary-relative-path-from-project-root)
   (ruri--summary-primary-name)))

(define-ruri-segment major-mode
  (propertize (format-mode-line mode-name) 'face 'ruri-major-mode))

(define-ruri-segment lsp
  (when (and (featurep 'lsp-mode) lsp--buffer-workspaces)
    (let ((text (mapconcat #'lsp--workspace-print lsp--buffer-workspaces ruri-lsp-separator)))
      (propertize text 'face 'ruri-lsp))))

(define-ruri-segment encoding
  (propertize (symbol-name buffer-file-coding-system) 'face 'ruri-encoding))

(define-ruri-segment keyboard-macro ()
  (when defining-kbd-macro
    (propertize "recording" 'face 'ruri-keyboard-macro)))

(defun ruri--render-checker-count (count face)
  "Render checker COUNT using FACE."
  (propertize (format ruri-checker-format count) 'face face))

(defun ruri--flycheck-get-face (predicate)
  "Get flycheck face using PREDICATE."
  (cl-case predicate
    (ruri--flycheck-is-info 'ruri-checker-info)
    (ruri--flycheck-is-warning 'ruri-checker-warning)
    (ruri--flycheck-is-error 'ruri-checker-error)))

(defun ruri--flycheck-get-severity (report)
  "Get severity from REPORT."
  (flycheck-error-level-severity (car report)))

(defun ruri--flycheck-is-info (report)
  "Judge if given flycheck REPORT is info."
  (< (ruri--flycheck-get-severity report) 10))

(defun ruri--flycheck-is-warning (report)
  "Judge if given flycheck REPORT is warning."
  (let ((level (ruri--flycheck-get-severity report)))
    (and (<= 10 level) (< level 100))))

(defun ruri--flycheck-is-error (report)
  "Judge if given flycheck REPORT is error."
  (<= 100 (ruri--flycheck-get-severity report)))

(defun ruri--flycheck-render-report-list (predicate report-list)
  "Extract and render certain kind of reports (e.g. warning) from REPORT-LIST using PREDICATE."
  (let ((face (ruri--flycheck-get-face predicate))
        (count (apply #'+ (mapcar #'cdr (seq-filter predicate report-list)))))
    (when count
      (ruri--render-checker-count count face))))

(define-ruri-segment flycheck
  (when (featurep 'flycheck)
    (when-let ((report-list (flycheck-count-errors flycheck-current-errors)))
      (let ((info-text (ruri--flycheck-render-report-list #'ruri--flycheck-is-info report-list))
            (warning-text (ruri--flycheck-render-report-list #'ruri--flycheck-is-warning report-list))
            (error-text (ruri--flycheck-render-report-list #'ruri--flycheck-is-error report-list)))
        (string-join (remove nil (list error-text warning-text info-text)) ruri-checker-separator)))))

(defun ruri--flymake-get-face (predicate)
  "Get flymake face using PREDICATE."
  (cl-case predicate
    (ruri--flymake-is-note 'ruri-checker-info)
    (ruri--flymake-is-warning 'ruri-checker-warning)
    (ruri--flymake-is-error 'ruri-checker-error)))

(defun ruri--get-flymake-category (report)
  "Get flymake category of a REPORT."
  (get (flymake-diagnostic-type report) 'flymake-category))

(defun ruri--flymake-is-note (report)
  "Check if a REPORT is a flymake note."
  (eq (ruri--get-flymake-category report) 'flymake-note))

(defun ruri--flymake-is-warning (report)
  "Check if a REPORT is a flymake warning."
  (eq (ruri--get-flymake-category report) 'flymake-warning))

(defun ruri--flymake-is-error (report)
  "Check if a REPORT is a flymake error."
  (eq (ruri--get-flymake-category report) 'flymake-error))

(defun ruri--flymake-render-report-list (predicate report-list)
  "Extract and render certain kind of reports (e.g. warning) from REPORT-LIST using PREDICATE."
  (let ((face (ruri--flymake-get-face predicate))
        (count (length (seq-filter predicate report-list))))
    (ruri--render-checker-count count face)))

(define-ruri-segment flymake
  (when (featurep 'flymake)
    (when-let ((report-list (flymake-diagnostics)))
      (let ((note-text (ruri--flymake-render-report-list #'ruri--flymake-is-note report-list))
            (warning-text (ruri--flymake-render-report-list #'ruri--flymake-is-warning report-list))
            (error-text (ruri--flymake-render-report-list #'ruri--flymake-is-error report-list)))
        (string-join (remove nil (list error-text warning-text note-text)) ruri-checker-separator)))))

(defvar ruri--eglot-connecting nil)

(defun ruri--eglot-set-connecting-true (&rest _)
  (setq ruri--eglot-connecting t))

(defun ruri--eglot-set-connecting-false (&rest _)
  (setq ruri--eglot-connecting nil))

(define-ruri-segment eglot
  (when (featurep 'eglot)
    (pcase-let* ((server (eglot-current-server))
                 (nick (and server (eglot-project-nickname server))))
      (when (or nick ruri--eglot-connecting)
        (propertize
         (if ruri--eglot-connecting
             (when ruri--eglot-connecting
               "activating lsp...")
           (concat "lsp:" nick))
         'face
         'ruri-eglot)))))

;;;###autoload
(defun ruri-install (segment-list)
  "Update `mode-line-format' using SEGMENT-LIST."
  (let ((prefixed-segment-list (mapcar #'ruri--get-segment-name (cons 'empty segment-list))))
    (add-hook 'post-self-insert-hook #'force-mode-line-update)
    (add-hook 'eglot-server-initialized-hook 'ruri--eglot-set-connecting-true)
    (add-hook 'eglot-connect-hook 'ruri--eglot-set-connecting-false)
    (setq-default mode-line-format `("%e" (:eval (ruri--render ',prefixed-segment-list))))))

;;;###autoload
(defun ruri-cleanup ()
  (remove-hook 'post-self-insert-hook #'force-mode-line-update)
  (remove-hook 'eglot-server-initialized-hook #'ruri--eglot-set-connecting-true)
  (remove-hook 'eglot-connect-hook #'ruri--eglot-set-connecting-false))

(provide 'ruri)

;;; ruri.el ends here
