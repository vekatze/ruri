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

(defcustom ruri-flycheck-separator " "
  "The separator that is inserted between flycheck entities."
  :group 'ruri
  :type 'string)

(defcustom ruri-flycheck-format "•%d"
  "The format string that is used when displaying a flycheck entity.
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

(defvar ruri--segment--format "ruri-segment:%s"
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
  "Face for the major mode string in an ruri modeline."
  :group 'ruri)

(defface ruri-lsp
  '((t (:inherit font-lock-constant-face)))
  "Face for the LSP server string in an ruri modeline."
  :group 'ruri)

(defface ruri-encoding
  '((t (:inherit font-lock-keyword-face)))
  "Face for the buffer encoding string in an ruri modeline."
  :group 'ruri)

(defface ruri-flycheck-error
  '((t (:foreground "#ff8059")))
  "Face for flycheck error feedback in the modeline."
  :group 'ruri)

(defface ruri-flycheck-warning
  '((t (:foreground "#d0bc00")))
  "Face for flycheck warning feedback in the modeline."
  :group 'ruri)

(defface ruri-flycheck-info
  '((t (:foreground "#44bc44")))
  "Face for flycheck info feedback in the modeline."
  :group 'ruri)

;;
;; main
;;

(defun ruri--get-segment-name (key)
  (intern (format ruri--segment--format key)))

(defmacro define-ruri-segment (key &rest value)
  "Define a ruri segment by specifing its name and body."
  (declare (indent defun))
  `(defun ,(ruri--get-segment-name key) () ,@value))

(defun ruri--render-segment (segment)
  (funcall segment))

(defun ruri--render (segment-list)
  (string-join (remove nil (mapcar 'ruri--render-segment segment-list)) ruri-separator))

;;;###autoload
(defun ruri-install (segment-list)
  (let ((prefixed-segment-list (mapcar 'ruri--get-segment-name (cons 'empty segment-list))))
    (add-hook 'post-self-insert-hook 'force-mode-line-update)
    (setq-default mode-line-format `("%e" (:eval (ruri--render ',prefixed-segment-list))))))

(defun ruri--calculate-raise ()
  (* (/ (* ruri-mode-line-height-zoom 2.0) 15) -1))

(define-ruri-segment empty
  (propertize "\u200b" 'display `((height ,ruri-mode-line-height-zoom) (raise ,(ruri--calculate-raise)))))

(define-ruri-segment modified
  "%*")

(define-ruri-segment size
  "%i")

(defun ruri--get-vc-root-dir ()
  (when-let ((root-dir (vc-root-dir)))
    (expand-file-name root-dir)))

(defun ruri--get-vc-currect-git-branch ()
  (when (vc-root-dir)
    (car (vc-git-branches))))

(defun ruri--summary-project-name ()
  (when-let ((current-project-root (ruri--get-vc-root-dir)))
    (propertize
     (concat
      (file-name-base (directory-file-name current-project-root))
      ruri-summary-separator)
     'face
     'ruri-summary-prefix)))

(defun ruri--summary-git-branch-name ()
  (when-let ((current-git-branch (ruri--get-vc-currect-git-branch)))
    (propertize
     (concat
      current-git-branch
      ruri-summary-separator)
     'face
     'ruri-summary-prefix)))

(defun ruri--summary-relative-path-from-project-root ()
  (when-let ((project-root-dir (ruri--get-vc-root-dir))
             (relative-path (file-name-directory (file-relative-name (buffer-file-name) project-root-dir))))
    (propertize relative-path 'face 'ruri-summary-secondary)))

(defun ruri--summary-primary-name ()
  (propertize
   (if-let ((file-name (buffer-file-name)))
       (file-name-nondirectory file-name)
     (buffer-name))
   'face
   'ruri-summary-body))

(define-ruri-segment summary
  (concat
   (ruri--summary-project-name)
   (ruri--summary-git-branch-name)
   (ruri--summary-relative-path-from-project-root)
   (ruri--summary-primary-name)))

(define-ruri-segment major-mode
  (propertize (format-mode-line mode-name) 'face 'ruri-major-mode))

(define-ruri-segment lsp
  (when (and (featurep 'lsp-mode) lsp--buffer-workspaces)
    (let ((text (mapconcat 'lsp--workspace-print lsp--buffer-workspaces ruri-lsp-separator)))
      (propertize text 'face 'ruri-lsp))))

(define-ruri-segment encoding
  (propertize (symbol-name buffer-file-coding-system)
              'face 'ruri-encoding))

(defun ruri--flycheck-get-face (predicate)
  (cl-case predicate
    (ruri--flycheck-is-info 'ruri-flycheck-info)
    (ruri--flycheck-is-warning 'ruri-flycheck-warning)
    (ruri--flycheck-is-error 'ruri-flycheck-error)))

(defun ruri--flycheck-get-severity (level-count-pair)
  (flycheck-error-level-severity (car level-count-pair)))

(defun ruri--flycheck-is-info (level-count-pair)
  (< (ruri--flycheck-get-severity level-count-pair) 10))

(defun ruri--flycheck-is-warning (level-count-pair)
  (let ((level (ruri--flycheck-get-severity level-count-pair)))
    (and (<= 10 level) (< level 100))))

(defun ruri--flycheck-is-error (level-count-pair)
  (<= 100 (ruri--flycheck-get-severity level-count-pair)))

(defun ruri--flycheck-render-info (predicate check-info-list)
  (let ((face (ruri--flycheck-get-face predicate))
        (count (apply '+ (mapcar 'cdr (seq-filter predicate check-info-list)))))
    (when count
      (propertize (format ruri-flycheck-format count) 'face face))))

(define-ruri-segment flycheck
  (when (featurep 'flycheck)
    (when-let ((check-info-list (flycheck-count-errors flycheck-current-errors)))
      (let ((info-text (ruri--flycheck-render-info 'ruri--flycheck-is-info check-info-list))
            (warning-text (ruri--flycheck-render-info 'ruri--flycheck-is-warning check-info-list))
            (error-text (ruri--flycheck-render-info 'ruri--flycheck-is-error check-info-list)))
        (string-join (remove nil (list error-text warning-text info-text)) ruri-flycheck-separator)))))

(provide 'ruri)

;;; ruri.el ends here
