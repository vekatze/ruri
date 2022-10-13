;;; ruri.el --- A thin-layer over modeline -*- lexical-binding: t; -*-

;; Author: vekatze <vekatze@icloud.com>
;; Created: 2021-12-10
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (dash "2.14.1") (f "0.20.0") (lsp-mode "6.2.1") (yaml "0.2.0"))
;; URL: https://github.com/vekatze/ruri.el

;;; Commentary:

;; lorem

;;; Code:

(require 'flycheck)
(require 'lsp)
(require 'magit)
(require 'projectile)

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
  "Define an ruri segment by specifing its name and body."
  (declare (indent defun))
  `(defun ,(ruri--get-segment-name key) () ,@value))

(defun ruri--render-segment (segment)
  (funcall segment))

(defun ruri--render (segment-list)
  (string-join (remove nil (mapcar 'ruri--render-segment segment-list)) ruri-separator))

(defun ruri-install (segment-list)
  (let ((prefixed-segment-list (mapcar 'ruri--get-segment-name (cons 'empty segment-list))))
    (add-hook 'post-self-insert-hook 'force-mode-line-update)
    (setq-default mode-line-format `("%e" (:eval (ruri--render ',prefixed-segment-list))))))

(defun ruri-calculate-raise ()
  (* (/ (* ruri-mode-line-height-zoom 2.0) 15) -1))

(define-ruri-segment empty
  (propertize "\u200b" 'display `((height ,ruri-mode-line-height-zoom) (raise ,(ruri-calculate-raise)))))

(define-ruri-segment modified
  "%*")

(define-ruri-segment size
  "%i")

(defun ruri--project-info ()
  (when-let ((branch (magit-get-current-branch))
             (project-root-dir (directory-file-name (projectile-project-root)))
             (project-root (file-name-base project-root-dir))
             (file-path (buffer-file-name)))
    (concat
     (propertize
      (concat
       project-root
       ruri-summary-separator
       branch
       ruri-summary-separator)
      'face 'ruri-summary-prefix)
     (propertize
      (or (file-name-directory (file-relative-name file-path project-root-dir)) "")
      'face 'ruri-summary-secondary))))

(defun ruri--get-summary-primary-name ()
  (if-let ((file-path (buffer-file-name)))
      (file-name-nondirectory file-path)
    ruri-buffer-format))

(define-ruri-segment summary
  (let ((primary (propertize (ruri--get-summary-primary-name) 'face 'ruri-summary-body)))
    (concat (ruri--project-info) primary)))

(define-ruri-segment major-mode
  (propertize (format-mode-line mode-name) 'face 'ruri-major-mode))

(define-ruri-segment lsp
  (when lsp--buffer-workspaces
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
  (when-let ((check-info-list (flycheck-count-errors flycheck-current-errors)))
    (let ((info-text (ruri--flycheck-render-info 'ruri--flycheck-is-info check-info-list))
          (warning-text (ruri--flycheck-render-info 'ruri--flycheck-is-warning check-info-list))
          (error-text (ruri--flycheck-render-info 'ruri--flycheck-is-error check-info-list)))
      (string-join (remove nil (list error-text warning-text info-text)) ruri-flycheck-separator))))

(provide 'ruri)

;;; ruri.el ends here
