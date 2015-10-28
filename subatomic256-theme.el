;;; subatomic256-theme.el --- Fork of subatomic-theme for terminals.

;; Copyright 2012 John Olsson,
;;           2015 Duncan Burken

;; Author: John Olsson <john@cryon.se>
;; Modified by: Chris Corbyn <chris@w3style.co.uk>,
;;              Duncan Burke <duncankburke@gmail.com>
;; URL: https://github.com/cryon/subatomic256
;; Version: 1.3

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

(deftheme subatomic256
  "subatomic256 emacs theme")

(require 'dash)

(eval-when-compile
  (require 'cl-lib))

(defvar subatomic256-palette-file "~/subatomic256.gpl")
(defvar subatomic256-faces nil)

(defmacro let-bindings (bindings &rest body)
  `(let ,(eval bindings) ,@body))

(defmacro subatomic256-face (name &rest alist)
  `(,name ((t alist))))

(defun subatomic256--get-colour (symbol)
  (cadr (assq symbol subatomic256-palette)))

(defun subatomic256--resolve-vars (list)
  (-map
   (lambda (elt)
          (cond
           ((listp elt) (subatomic256--resolve-vars elt))
           ((symbolp elt)
            (let ((colour (subatomic256--get-colour elt)))
              (if colour colour elt)))
           (t elt)))
   list))

(defun subatomic256--resolve-faces ()
  (-map
   (lambda (cons)
     (let ((face (car cons))
           (spec (cdr cons)))
       `(quote (,face
                ((t ,(subatomic256--resolve-vars spec)))
                t))
       ))
   subatomic256-faces))

(defun subatomic256-set-faces ()
  (eval `(custom-theme-set-faces
          'subatomic256
          ,@(subatomic256--resolve-faces))))

(defun subatomic256-colour-string (r g b)
  (format "#%02x%02x%02x" r g b))

(defun subatomic256-string-colour (str)
  (cl-flet ((parse-match (n) (string-to-number (match-string n str) 16)))
    (unless (string-match "^#\\(..\\)\\(..\\)\\(..\\)$" str)
      (error "Parsing failure"))
    (list
     (parse-match 1)
     (parse-match 2)
     (parse-match 3))))

(defun subatomic256-load-palette ()
  (interactive)
  (let* ((palette nil)
         (digits "\\([0-9]*\\)")
         (search-string (format "^%s\\s-+%s\\s-+%s\\s-+\\(.*\\)" digits digits digits)))
    (cl-flet ((parse-match (n) (string-to-number (match-string n))))
      (with-temp-buffer
        (insert-file-contents subatomic256-palette-file)
        (goto-char (point-min))
        (re-search-forward "#\n")
        (while (not (eobp))
          (let ((eol nil))
            (save-excursion
              (end-of-line)
              (setq eol (point)))
            (re-search-forward search-string eol))
          (let* ((r (parse-match 1))
                 (g (parse-match 2))
                 (b (parse-match 3))
                 (name (match-string 4))
                 (colour (subatomic256-colour-string r g b)))
            (push (list (intern name) colour) palette))
          (forward-line)))
      (setq subatomic256-palette (reverse palette))
      (subatomic256-set-faces))))

(defun subatomic256-save-palette ()
  (interactive)
  (with-temp-buffer
    (insert "GIMP Palette\n")
    (insert "Name: subatomic256-256\n")
    (insert "Columns: 4\n")
    (insert "#\n")
    (cl-mapc
     (lambda (l)
       (let ((s (symbol-name (car l)))
             (c (subatomic256-string-colour (cadr l))))
         (insert (eval `(format "%03d %03d %03d %s\n" ,@c ,s)))))
     subatomic256-palette)
    (write-region
     (point-min) (point-max) subatomic256-palette-file)))

(defun subatomic256-watch-palette ()
  (require 'filenotify)
  (file-notify-add-watch
   subatomic256-palette-file
   '(change)
   #'subatomic256--watch-callback))

(defun subatomic256--watch-callback (event)
  (when (or (eq 'changed (cadr event))
            (eq 'created (cadr event)))
    (subatomic256-load-palette)))



(defvar subatomic256-palette
  '((background-1        "#1c1c1c")
    (background-2        "#262626")
    (background-3        "#444444")
    (background-4        "#6868a8")
    (foreground-0        "#d7d7d7")
    (foreground-1        "#d7afaf")
    (emph-1              "#9292d6")
    (emph-2              "#87afaf")
    (emph-3              "#87afd7")
    (emph-4              "#afd75f")
    (emph-5              "#87af5f")
    (emph-6              "#ffaf00")
    (emph-7              "#af5faf")
    (emph-8              "#d7875f")

    (full-white        "#ffffff")
    (full-black        "#000000")
    (full-red          "#ff0000")
    (full-green        "#00ff00")
    (full-blue         "#0000ff")
    (full-yellow       "#ffff00")
    (full-magenta      "#ff00ff")
    (full-cyan         "#00ffff")))


(setq
 subatomic256-faces
 '((default
     :background background-1 :foreground foreground-0)
   (fringe
    :background midnight)
   (vertical-border
    :foreground background-3)
   (region
    :background background-3 :foreground full-white)
   (show-paren-match-face
    :foreground full-green :bold t)
   (show-paren-mismatch-face
    :foreground full-red :bold t)
   (isearch
    :background background-3 :foreground full-green :bold t)
   (lazy-highlight
    :background background-3 :foreground emph-6 :bold t)
   (query-replace
    :inherit lazy-highlight)
   (trailing-whitespace
    :inherit show-paren-mismatch-face :underline t)
   (mode-line
    :foreground full-white :weight bold :box (:line-width 1 :style released-button))
   (powerline-active1
    :background background-3)
   (powerline-active2
    :background background-2)
   (modeline-inactive
    :background background-3 :foreground emph-1)
   (powerline-inactive1
    :background background-3)
   (powerline-inactive2
    :background background-2)
   (header-line
    :background background-3 :foreground full-white :weight bold)
   (hl-line
    :background background-2)
   (highlight-current-line-face
    :inherit hl-line)
   (minibuffer-prompt
    :foreground emph-7 :weight bold)
   (escape-glyph
    :foreground foreground-1 :weight bold)
   (link
    :foreground emph-3 :weight bold :underline t)
   ;; font lock
   (font-lock-keyword-face
    :foreground emph-6 :weight bold)
   (font-lock-function-name-face
    :foreground victory-blue)
   (font-lock-warning-face
    (:foreground emph-8))
   (font-lock-builtin-face
    :foreground emph-6)
   (font-lock-variable-name-face
    :foreground victory-blue)
   (font-lock-constant-face
    :foreground full-white :weight bold :italic t)
   (font-lock-type-face
    :foreground emph-3 :weight bold)
   (font-lock-negation-char-face
    :foreground emph-8 :weight bold)
   (font-lock-preprocessor-face
    :foreground foreground-1)
   (font-lock-comment-face
    :foreground emph-1)
   (font-lock-string-face
    :foreground emph-4)
   (font-lock-comment-delimiter-face
    :foreground background-4)
   (font-lock-doc-face
    :foreground emph-7 :italic t)
   ;; flymake
   (flymake-errline
    :underline full-red)
   (flymake-warnline
    :underline full-yellow)
   ;; eshell
   (eshell-ls-clutter
    :inherit font-lock-comment-face)
   (eshell-ls-executable
    :foreground emph-4)
   (eshell-ls-directory
    :foreground emph-2 :bold t)
   (eshell-ls-archive
    :foreground emph-6)
   (eshell-ls-backup
    :inherit font-lock-comment-face)
   (eshell-ls-missing
    :inherit font-lock-warning-face)
   (eshell-ls-unreadable
    :inherit font-lock-warning-face)
   (eshell-ls-symlink
    :inherit font-lock-builtin-face)
   (eshell-prompt
    :inherit minibuffer-prompt)
   (eshell-ls-backup
    :foreground emph-8 :slant italic)
   (eshell-ls-product
    :inherit default :weight bold)
   (eshell-ls-readonly
    :inherit font-lock-comment)
   (eshell-ls-special
    :foreground foreground-1)
   ;; calendar
   (calendar-today-face
    :foreground emph-4 :bold t)
   (holiday-face
    :foreground emph-8)
   (diary-face
    :foreground emph-7)
   ;; erc
   (erc-default-face
    :inherit default)
   (erc-current-nick-face
    :inherit font-lock-keyword-face)
   (erc-action-face
    :foreground foreground-1)
   (erc-dangerous-host-face
    :inherit font-lock-warning-face)
   (erc-highlight-face
    :weight bold)
   (erc-direct-msg-face
    :foreground emph-4)
   (erc-nick-msg-face
    :foreground emph-3 :weight bold)
   (erc-fool-face
    :inherit font-lock-comment-face)
   (erc-input-face
    :inherit default :weight bold)
   (erc-error-face
    :inherit font-lock-warning-face)
   (erc-keyword-face
    :inherit font-lock-keyword-face)
   (erc-nick-default-face
    :inherit default)
   (erc-prompt-face
    :inherit eshell-prompt)
   (erc-notice-face
    :foreground emph-7)
   (erc-timestamp-face
    :inherit font-lock-comment-face)
   (erc-pal-face
    :foreground emph-4)
   ;; highlight-symbol
   (highlight-symbol-face
    :background background-3)
   ;; diff
   (diff-file-header
    :background background-1 :foreground victory-blue)
   (diff-header
    :inherit default :foreground emph-1)
   (diff-indicator-changed
    :foreground full-yellow :weight bold)
   (diff-changed
    :foreground emph-6)
   (diff-indicator-removed
    :foreground full-red :weight bold)
   (diff-removed
    :foreground emph-8)
   (diff-indicator-added
    :foreground full-green :weight bold)
   (diff-added
    :foreground emph-4)
   (diff-hunk-header
    :foreground full-white)
   (diff-refine-change
    :background background-3 :foreground full-white :weight bold)
   ;; magit
   (magit-branch
    :foreground emph-4 :weight bold)
   (magit-diff-add
    :inherit diff-added)
   (magit-diff-del
    :inherit diff-removed)
   (magit-diff-file-header
    :inherit diff-file-header)
   (magit-diff-hunk-header
    :inherit diff-hunk-header)
   (magit-diff-none
    :inherit default)
   (magit-header
    :inherit diff-header)
   (magit-item-highlight
    :background background-3)
   (magit-item-mark
    :background background-3)
   (magit-log-graph
    :foreground victory-blue)
   (magit-log-head-label-bisect-bad
    :foreground emph-8)
   (magit-log-head-label-bisect-good
    :foreground emph-4)
   (magit-log-head-label-default
    :foreground emph-7 :weight bold)
   (magit-log-head-label-local
    :inherit magit-log-head-label-default :foreground emph-4)
   (magit-log-head-label-patches
    :inherit magit-log-head-label-default)
   (magit-log-head-label-remote
    :inherit magit-log-head-label-default)
   (magit-log-head-label-tags
    :inherit magit-log-head-label-default)
   (magit-log-message
    :inherit default)
   (magit-log-sha1
    :foreground emph-6)
   (magit-section-title
    :inherit header-line)
   (magit-section-highlight
    :background background-3)
   (magit-whitespace-warning-face
    :inherit font-lock-warning)
   ;; markdown-mode
   (markdown-markup-face
    :foreground emph-3)
   ;; compilation
   (compilation-info
    :inherit default)
   (compilation-warning
    :inherit font-lock-warning)
   ;; twittering-mode
   (twittering-username-face
    :inherit font-lock-keyword-face)
   (twittering-uri-face
    :inherit link)
   (twittering-timeline-header-face
    :foreground foreground-1 :weight bold)
   (twittering-timeline-footer-face
    :inherit twittering-timeline-header-face)
   ;; outline
   (outline-1
    :foreground emph-6 :weight bold)
   (outline-2
    :foreground emph-4 :weight bold)
   (outline-4
    :foreground foreground-0 :weight bold)
   (outline-3
    :foreground foreground-1 :weight bold)
   (outline-5
    :foreground emph-3 :weight bold)
   (outline-6
    :foreground emph-7 :weight bold)
   (outline-7
    :foreground emph-5 :weight bold)
   (outline-8
    :foreground emph-1 :weight bold)
   ;; org-mode
   (org-level-1
    :inherit outline-1)
   (org-level-2
    :inherit outline-2)
   (org-level-3
    :inherit outline-3)
   (org-level-4
    :inherit outline-4)
   (org-level-5
    :inherit outline-5)
   (org-level-6
    :inherit outline-6)
   (org-level-7
    :inherit outline-7)
   (org-level-8
    :inherit outline-8)
   (org-hide
    :foreground midnight)
   (org-link
    :inherit link)
   (org-checkbox
    :background background-1 :foreground full-white :weight bold :box (:line-width 1 :style released-button))
   (org-done
    :foreground emph-4 :weight bold)
   (org-todo
    :foreground foreground-0 :weight bold)
   (org-table
    :foreground foreground-1)
   (org-date
    :foreground foreground-0 :weight bold)
   (org-document-info-keyword
    :foreground emph-1)
   (org-document-info
    :foreground foreground-1 :weight bold :slant italic)
   (org-block-begin-line
    :background background-3 :foreground emph-1 :weight bold)
   (org-block-background
    :background background-2)
   (org-block-end-line
    :inherit org-block-begin-line)
   (org-agenda-date-today
    :foreground emph-4 :background background-3 :weight bold)
   (org-agenda-date
    :foreground emph-3)
   (org-agenda-date-weekend
    :foreground foreground-0)
   (org-agenda-structure
    :inherit header-line)
   (org-warning
    :inherit font-lock-warning-face)
   (org-agenda-clocking
    :inherit org-date)
   (org-deadline-announce
    :inherit font-lock-warning-face)
   (org-formula
    :inherit font-lock-doc-face)
   (org-special-keyword
    :inherit font-lock-keyword)
   ;; dired+
   (diredp-compressed-file-suffix
    :foreground emph-6 :weight bold)
   (diredp-date-time
    :foreground emph-1)
   (diredp-deletion
    :foreground emph-8 :weight bold :slant italic)
   (diredp-deletion-file-name
    :foreground emph-8 :underline t)
   (diredp-symlink
    :foreground emph-6)
   (diredp-dir-heading
    :inherit minibuffer-prompt)
   (diredp-display-msg
    :inherit default)
   (diredp-exec-priv
    :foreground emph-4)
   (diredp-write-priv
    :foreground emph-8)
   (diredp-read-priv
    :foreground emph-6)
   (diredp-dir-priv
    :foreground emph-3 :weight bold)
   (diredp-link-priv
    :foreground emph-6)
   (diredp-other-priv
    :foreground emph-6 :weight bold)
   (diredp-rare-priv
    :foreground emph-8 :weight bold)
   (diredp-no-priv
    :foreground emph-1)
   (diredp-file-name
    :foreground foreground-0)
   (diredp-file-suffix
    :inherit dired-file-name)
   (diredp-number
    :foreground victory-blue)
   (diredp-executable-tag
    :foreground emph-4 :weight bold)
   (diredp-flag-mark
    :foreground emph-8 :weight bold)
   (diredp-flag-mark-line
    :background background-3)
   (diredp-mode-line-marked
    :foreground emph-8)
   (diredp-mode-line-flagged
    :foreground emph-6)
   (diredp-ignored-file-name
    :foreground background-4)
   ))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'subatomic256)
(subatomic256-set-faces)
;;; subatomic256-theme.el ends here
