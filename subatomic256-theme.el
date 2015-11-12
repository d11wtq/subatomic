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
(defvar subatomic256-palette nil)

(defmacro let-bindings (bindings &rest body)
  `(let ,(eval bindings) ,@body))

(defmacro subatomic256-face (name &rest alist)
  `(,name ((t alist))))

(defun subatomic256--get-colour (symbol)
  (cadr (assq symbol subatomic256-palette)))

(defun subatomic256--resolve-vars (list)
  (cond
   ((not list) nil)
   ((listp (car list))
    (cons (subatomic256--resolve-vars (car list))
          (subatomic256--resolve-vars (cdr list))))
   ((or (-contains-p '(:foreground :background) (car list))
        (and (eq ':underline (car list))
             (not (-contains-p '(t nil) (cadr list)))))
    (let ((colour (subatomic256--get-colour (cadr list))))
      (unless colour
        (message "Invalid colour %s" (cadr list)))
      `(,(car list) ,colour ,@(subatomic256--resolve-vars (cddr list)))))
   (t
    (cons (car list) (subatomic256--resolve-vars (cdr list))))))


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
  (let ((watch-active subatomic256--watch-desc))
    (subatomic256-watch-palette-stop)
    (with-temp-buffer
      (insert "GIMP Palette\n")
      (insert "Name: subatomic256\n")
      (insert "Columns: 4\n")
      (insert "#\n")
      (cl-mapc
       (lambda (l)
         (let ((s (symbol-name (car l)))
               (c (subatomic256-string-colour (cadr l))))
           (insert (eval `(format "%03d %03d %03d %s\n" ,@c ,s)))))
       subatomic256-palette)
      (write-region
       (point-min) (point-max) subatomic256-palette-file))
    (when watch-active
      (subatomic256-watch-palette))))

(defvar subatomic256--watch-desc nil)

(defun subatomic256-watch-palette ()
  (interactive)
  (require 'filenotify)
  (subatomic256-watch-palette-stop)
  (setq subatomic256--watch-desc
        (file-notify-add-watch
         subatomic256-palette-file
         '(change)
         #'subatomic256--watch-callback)))

(defun subatomic256-watch-palette-stop ()
  (interactive)
  (require 'filenotify)
  (when subatomic256--watch-desc
    (file-notify-rm-watch subatomic256--watch-desc)
    (setq subatomic256--watch-desc nil)))

(defun subatomic256--watch-callback (event)
  (when (or (eq 'changed (cadr event))
            (eq 'created (cadr event)))
    (subatomic256-load-palette)))


(progn
  (setq subatomic256-palette
        '((background-1 "#1c1c1c")
          (background-2 "#262626")
          (background-3 "#444444")
          (foreground-1 "#d7d7d7")
          (foreground-2 "#afafff")
          (foreground-3 "#94ccd1")
          (foreground-4 "#7dcc96")
          (foreground-5 "#d1af93")
          (foreground-6 "#e08787")
          (foreground-7 "#aec8f4")
          (emph-1 "#77a4f1")
          (emph-2 "#4bb4ac")
          (emph-3 "#69c74d")
          (emph-4 "#cfba60")
          (emph-5 "#e17b51")
          (emph-6 "#ff5e5e")
          (emph-7 "#cd64de")
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
       :background background-1 :foreground foreground-1)
     (fringe
      :background background-2)
     (cursor
      :background foreground-7)
     (secondary-selection
      :background emph-4)
     (vertical-border
      :foreground background-3)
     (region
      :background background-3 :foreground foreground-1)
     (show-paren-match-face
      :foreground full-green)
     (show-paren-mismatch-face
      :foreground full-red)
     (isearch
      :background background-3 :foreground full-green)
     (lazy-highlight
      :background background-3 :foreground emph-6)
     (query-replace
      :inherit lazy-highlight)
     (trailing-whitespace
      :inherit show-paren-mismatch-face :underline t)
     (mode-line
      :foreground foreground-1 :weight bold :box (:line-width 1 :style released-button))
     (powerline-active1
      :background background-3)
     (powerline-active2
      :background background-2)
     (modeline-inactive
      :background background-3 :foreground foreground-2)
     (powerline-inactive1
      :background background-3)
     (powerline-inactive2
      :background background-2)
     (header-line
      :background background-3 :foreground foreground-1 :weight bold)
     (hl-line
      :background background-2)
     (highlight-current-line-face
      :inherit hl-line)
     (minibuffer-prompt
      :foreground emph-2)
     (escape-glyph
      :foreground foreground-2)
     (link
      :foreground foreground-2 :underline t)
     (link-visited
      :inherit link
      :foreground emph-7)
     (highlight
      :background foreground-7)
     (success
      :foreground emph-3)
     (tool-bar
      :foreground foreground-2)
     (tooltip
      :background background-3)

     ;; calendar
     (calendar-today-face
      :foreground emph-1)
     (holiday-face
      :foreground emph-2)
     (diary-face
      :foreground emph-3)
     (calendar-weekend-header
      :inherit calendar-weekday-header)

     ;; compilation
     (compilation-info
      :inherit default)
     (compilation-warning
      :inherit font-lock-warning)

     ;; diff
     (diff-added
      :inherit magit-diff-added)
     (diff-changed
      :inherit magit-diff-base)
     (diff-removed
      :inherit magit-diff-removed)
     (diff-context
      :inherit magit-diff-context)
     (diff-file-header
      :inherit magit-diff-file-heading)
     (diff-header
      :inherit diff-file-header)
     (diff-hunk-header
      :inherit magit-diff-hunk-heading)
     (diff-indicator-changed
       :foreground emph-5)
     (diff-indicator-removed
      :foreground emph-6)
     (diff-indicator-added
      :foreground emph-3)
     (diff-refine-added
      :inherit diff-added
      :background background-3)
     (diff-refine-changed
      :inherit diff-changed
      :background background-3)
     (diff-refine-removed
      :inherit diff-removed
      :background background-3)

     (smerge-base
      :inherit magit-diff-base)
     (smerge-markers
      :inherit magit-diff-conflict-heading)
     (smerge-mine
      :inherit magit-diff-our)
     (smerge-other
      :inherit magit-diff-their)
     (smerge-refined-added
      :inherit magit-diff-added)
     (smerge-refined-changed
      :inherit magit-diff-base)
     (smerge-refined-removed
      :inherit magit-diff-removed)

     ;; dired+
     (diredp-compressed-file-suffix
      :foreground foreground-5 :weight bold)
     (diredp-date-time
      :foreground foreground-3)
     (diredp-deletion
      :foreground emph-6 :weight bold :slant italic)
     (diredp-deletion-file-name
      :foreground emph-6 :underline t)
     (diredp-symlink
      :foreground foreground-2)
     (diredp-dir-heading
      :inherit minibuffer-prompt)
     (diredp-display-msg
      :inherit default)
     (diredp-exec-priv
      :foreground emph-3)
     (diredp-write-priv
      :foreground emph-2)
     (diredp-read-priv
      :foreground emph-2)
     (diredp-dir-priv
      :foreground emph-4 :weight bold)
     (diredp-link-priv
      :foreground emph-4)
     (diredp-other-priv
      :foreground emph-4 :weight bold)
     (diredp-rare-priv
      :foreground emph-4 :weight bold)
     (diredp-no-priv
      :foreground foreground-5)
     (diredp-file-name
      :foreground foreground-1)
     (diredp-file-suffix
      :inherit dired-file-name)
     (diredp-number
      :foreground emph-5)
     (diredp-executable-tag
      :foreground emph-3 :weight bold)
     (diredp-flag-mark
      :foreground emph-7 :weight bold)
     (diredp-flag-mark-line
      :background background-3)
     (diredp-mode-line-marked
      :foreground emph-7)
     (diredp-mode-line-flagged
      :foreground emph-7)
     (diredp-ignored-file-name
      :foreground foreground-3)

     ;; erc
     (erc-default-face
      :inherit default)
     (erc-current-nick-face
      :inherit font-lock-keyword-face)
     (erc-action-face
      :foreground foreground-2)
     (erc-dangerous-host-face
      :inherit font-lock-warning-face)
     (erc-highlight-face
      :weight bold)
     (erc-direct-msg-face
      :foreground emph-1)
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
      :foreground emph-1)

     ;; eshell
     (eshell-ls-clutter
      :inherit font-lock-comment-face)
     (eshell-ls-directory
      :foreground foreground-3)
     (eshell-ls-special
      :foreground emph-2)
     (eshell-ls-archive
      :foreground emph-4)
     (eshell-ls-executable
      :foreground emph-5)
     (eshell-ls-backup
      :foreground emph-7 :slant italic)
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
     (eshell-ls-product
      :inherit default :weight bold)
     (eshell-ls-readonly
      :inherit font-lock-comment)

     ;; font lock
     (font-lock-variable-name-face
      :foreground emph-1)
     (font-lock-type-face
      :foreground emph-2)
     (font-lock-constant-face
      :foreground emph-4)
     (font-lock-string-face
      :foreground emph-4)
     (font-lock-warning-face
      :foreground emph-6)
     (font-lock-negation-char-face
      :foreground emph-7)

     (font-lock-comment-face
      :foreground foreground-5)
     (font-lock-comment-delimiter-face
      :foreground foreground-5)
     (font-lock-doc-face
      :foreground foreground-5)

     (font-lock-builtin-face
      :foreground emph-7)
     (font-lock-function-name-face
      :foreground emph-1)
     (font-lock-keyword-face
      :foreground emph-7)
     (font-lock-preprocessor-face
      :foreground foreground-6)

     ;; flymake
     (flymake-errline
      :underline full-red)
     (flymake-warnline
      :underline full-yellow)

     ;; highlight-symbol
     (highlight-symbol-face
      :background background-3)

     ;; info
     (info-header-node
      :foreground emph-6)
     (info-menu-star
      :foreground emph-5)
     (info-node
      :foreground emph-6)
     (info-title-1
      :inherit outline-1)
     (info-title-2
      :inherit outline-2)
     (info-title-3
      :inherit outline-3)
     (info-title-4
      :inherit outline-4)
     (info-xref
      :inherit link)
     (info-xref-visited
      :inherit link-visited)

     ;; isearch
     (isearch
      :foreground emph-3
      :background background-3)
     (isearch-fail
      :foreground emph-6
      :background background-3)

     ;; replace
     (match
      :background background-2
      :foreground emph-3)

     ;; magit
     (magit-dimmed
      :foreground foreground-2)
     (magit-hash
      :foreground emph-4)
     (magit-tag
      :inherit magit-hash)
     (magit-branch-remote
      :foreground emph-1)
     (magit-branch-local
      :foreground emph-2)
     (magit-refname
      :foreground emph-7)
     (magit-signature-good
      :foreground emph-3)
     (magit-signature-bad
      :foreground emph-6)
     (magit-signature-untrusted
      :foreground emph-2)
     (magit-cherry-unmatched
      :foreground emph-2)
     (magit-cherry-equivalent
      :foreground emph-7)
     (magit-section-hilight
      :background background-2)
     (magit-section-heading
      :foreground emph-5)
     (magit-section-heading-selection
      :foreground emph-4)
     (magit-log-graph
      :foreground background-3)
     (magit-log-author
      :foreground foreground-4)
     (magit-log-date
      :foreground foreground-5)
     (magit-reflog-commit
      :foreground emph-3)
     (magit-reflog-amend
      :foreground emph-7)
     (magit-reflog-merge
      :foreground emph-3)
     (magit-reflog-checkout
      :foreground emph-1)
     (magit-reflog-reset
      :foreground emph-6)
     (magit-reflog-rebase
      :foreground emph-7)
     (magit-reflog-cherry-pick
      :foreground emph-3)
     (magit-reflog-remote
      :foreground emph-2)
     (magit-reflog-other
      :foreground emph-2)
     (magit-blame-heading
      :background background-2)
     (magit-sequence-stop
      :foreground emph-3)
     (magit-sequence-part
      :foreground emph-4)
     (magit-sequence-head
      :foreground emph-2)
     (magit-sequence-drop
      :foreground emph-6)
     (git-rebase-hash
      :background background-2)
     (magit-bisect-good
      :foreground emph-3)
     (magit-bisect-skip
      :foreground emph-4)
     (magit-bisect-bad
      :foreground emph-6)
     (magit-process-ok
      :foreground emph-3)
     (magit-process-ng
      :foreground emph-6)
     (magit-diff-file-heading
      :background background-2
      :foreground emph-4)
     (magit-diff-file-heading-selection
      :inherit magit-diff-file-heading-highlight
      :foreground emph-4)
     (magit-diff-hunk-heading
      :background background-2
      :foreground emph-1)
     (magit-diff-hunk-heading-highlight
      :inherit magit-diff-hunk-heading
      :background background-3)
     (magit-diff-hunk-heading-selection
      :inherit magit-diff-hunk-heading-highlight :foreground emph-4)
     (magit-diff-lines-heading
      :inherit magit-diff-hunk-heading-highlight :foreground emph-3)
     (magit-diff-added
      :background background-2
      :foreground foreground-4)
     (magit-diff-removed
      :background background-2
      :foreground foreground-6)
     (magit-diff-base
      :background background-2
      :foreground foreground-5)
     (magit-diff-context
      :foreground foreground-2)
     (magit-diff-added-highlight
      :inherit magit-diff-added
      :background background-3)
     (magit-diff-removed-highlight
      :inherit magit-diff-removed
      :background background-3)
     (magit-diff-base-highlight
      :inherit magit-diff-base
      :background background-3)
     (magit-diff-context-highlight
      :background background-1
      :foreground foreground-2)
     (magit-diffstat-added
      :foreground emph-3)
     (magit-diffstat-removed
      :foreground emph-6)

     ;; markdown-mode
     (markdown-italic-face
      :foreground emph-2 :slant italic)
     (markdown-bold-face
      :foreground emph-2 :weight bold)
     (markdown-strike-through-face
      :foreground emph-2 :strike-through t)
     (markdown-markup-face
      :foreground foreground-2)
     (markdown-header-face
      :foreground emph-1)
     (markdown-header-delimiter-face
      :inherit markdown-header-face)
     (markdown-metadata-key-face
      :foreground foreground-3)
     (markdown-metadata-value-face
      :foreground foreground-4)
     (markdown-link-face
      :inherit markdown-markup-face)
     (markdown-pre-face
      :foreground foreground-7)
     (markdown-url-face
      :foreground foreground-3)
     (markdown-highlight-face
      :inherit highlight)

     ;; outline
     (outline-1
      :foreground emph-2 :weight bold)
     (outline-2
      :foreground emph-3 :weight bold)
     (outline-4
      :foreground foreground-2 :weight bold)
     (outline-3
      :foreground emph-4 :weight bold)
     (outline-5
      :foreground foreground-3 :weight bold)
     (outline-6
      :foreground foreground-4 :weight bold)
     (outline-7
      :foreground foreground-5 :weight bold)
     (outline-8
      :foreground foreground-7 :weight bold)

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
     (org-agenda-done
      :foreground emph-3)
     (org-agenda-column-dateline
      :background background-2
      :foreground emph-4)
     (org-agenda-restriction-lock
      :background background-2
      :foreground emph-5)
     (org-column
      :background background-2
      :foreground emph-2)
     (org-column-title
      :inherit org-column
      :foreground emph-1)
     (org-date
      :inherit org-agenda-date)
     (org-date-selected
      :inherit org-agenda-date-today)
     (org-footnote
      :inherit link)
     (org-mode-line-clock
      :inherit org-agenda-date)
     (org-mode-line-clock-overrun
      :inherit org-agenda-date-today)
     (org-macro
      :foreground emph-5)
     (org-meta-line
      :foreground emph-4)
     (org-clock-overlay
      :background background-3)
     (org-hide
      :foreground background-1)
     (org-document-title
      :foreground emph-1)
     (org-link
      :inherit link)
     (org-checkbox
      :background background-1 :foreground full-white :weight bold :box (:line-width 1 :style released-button))
     (org-done
      :foreground emph-4 :weight bold)
     (org-todo
      :foreground foreground-1 :weight bold)
     (org-table
      :foreground foreground-2)
     (org-date
      :foreground foreground-1 :weight bold)
     (org-document-info-keyword
      :foreground foreground-3)
     (org-document-info
      :foreground foreground-2 :weight bold :slant italic)
     (org-block-begin-line
      :background background-3 :foreground foreground-3 :weight bold)
     (org-block-background
      :background background-2)
     (org-block-end-line
      :inherit org-block-begin-line)
     (org-agenda-date-today
      :foreground emph-4 :background background-3 :weight bold)
     (org-agenda-date
      :foreground emph-3)
     (org-agenda-date-weekend
      :foreground foreground-1)
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
     (org-scheduled
      :foreground emph-3)
     (org-scheduled-previously
      :foreground emph-6)
     (org-scheduled-today
      :inherit org-scheduled
      :underline t)
     (org-upcoming-deadline
      :foreground emph-6
      :underline t)
     (org-verbatim
      :inherit font-lock-string-face)
     (org-verse
      :inherit org-verbatim)
     (org-time-grid
      :foreground emph-5)
     (org-special-keyword
      :inherit font-lock-keyword-face)
     (org-tag
      :foreground emph-2)
     (org-tag-group
      :inherit org-tag
      :underline t)
     (org-property-value
      :foreground foreground-5)
     (org-todo
      :foreground foreground-4)

     (c-annotation-face
      :inherit font-lock-constant-face)
     (c-nonbreakable-space-face
      :background background-3)
     (change-log-acknowledgment
      :inherit font-lock-constant-face)

     ;; company-mode
     (company-echo-common
      :background background-3)
     (company-preview
      :background background-2)
     (company-preview-common
      :inherit company-preview)
     (company-preview-search
      :inherit company-preview
      :foreground foreground-6)
     (company-scrollbar-bg
      :inherit company-preview
      :foreground foreground-4)
     (company-scrollbar-fg
      :inherit company-preview
      :foreground foreground-6)
     (company-template-field
      :inherit company-preview
      :foreground emph-4)
     (company-tooltip
      :inherit company-preview
      :foreground foreground-2)
     (company-tooltip-annotation
      :inherit company-tooltip
      :foreground foreground-3)
     (company-tooltip-common
      :inherit company-tooltip
      :foreground foreground-6)
     (company-tooltip-common-selection
      :inherit company-tooltip-common
      :background background-3)
     (company-tooltip-mouse
      :inherit company-preview
      :foreground emph-4)
     (company-tooltip-search
      :inherit company-preview
      :foreground emph-2)
     (company-tooltip-selection
      :inherit company-tooltip
      :background background-3)

     ;; compilation
     (compilation-column-number
      :inherit font-lock-constant-face)
     (compilation-error
      :foreground emph-6)
     (compilation-mode-line-exit
      :foreground emph-3)
     (compilation-mode-line-fail
      :foreground emph-6)

     ;; ediff
     (ediff-even-diff-A
      :foreground foreground-2)
     (ediff-even-diff-Ancestor
      :foreground foreground-5)
     (ediff-even-diff-B
      :foreground foreground-3)
     (ediff-even-diff-C
      :foreground foreground-7)
     (ediff-current-diff-A
      :inherit ediff-even-diff-A
      :background background-3)
     (ediff-current-diff-Ancestor
      :inherit ediff-even-diff-Ancestor
      :background background-3)
     (ediff-current-diff-B
      :inherit ediff-even-diff-B
      :background background-3)
     (ediff-current-diff-C
      :inherit ediff-even-diff-C
      :background background-3)
     (ediff-fine-diff-A
      :inherit ediff-even-diff-A
      :background background-2)
     (ediff-fine-diff-Ancestor
      :inherit ediff-even-diff-Ancestor
      :background background-2)
     (ediff-fine-diff-B
      :inherit ediff-even-diff-B
      :background background-2)
     (ediff-fine-diff-C
      :inherit ediff-even-diff-C
      :background background-2)
     (ediff-odd-diff-A
      :inherit ediff-even-diff-A)
     (ediff-odd-diff-Ancestor
      :inherit ediff-even-diff-Ancestor)
     (ediff-odd-diff-B
      :inherit ediff-even-diff-B)
     (ediff-odd-diff-C
      :inherit ediff-even-diff-C)

     ;; ghc
     (ghc-face-error
      :foreground emph-6)
     (ghc-face-hole
      :foreground emph-4)
     (ghc-face-warn
      :foreground emph-5)

     ;; ido-mode
     (ido-indicator
      :background background-2
      :foreground emph-6)
     (ido-only-match
      :foreground emph-5)
     (ido-subdir
      :foreground emph-2)

     ;; message
     (message-cited-text
      :foreground emph-6)
     (message-header-cc
      :foreground emph-1)
     (message-header-name
      :foreground emph-2)
     (message-header-newsgroups
      :foreground emph-7)
     (message-header-other
      :inherit message-header-cc)
     (message-header-subject
      :inherit message-header-newsgroups)
     (message-header-to
      :inherit message-header-name)
     (message-header-xheader
      :foreground emph-4)
     (message-mml
      :foreground emph-3)
     (message-separator
      :foreground emph-6)
     (mm-command-output
      :foreground emph-6)

     (show-paren-mismatch-face
      :foreground emph-6)
     (show-paren-match-face
      :foreground emph-3)
     (show-paren-match
      :background emph-2)
     (show-paren-mismatch
      :background emph-7)

     (speedbar-button-face
      :foreground emph-3)
     (speedbar-directory-face
      :foreground emph-2)
     (speedbar-file-face
      :foreground emph-3)
     (speedbar-highlight-face
      :background background-3)
     (speedbar-selected-face
      :background background-2
      :foreground emph-7)
     (speedbar-tag-face
      :foreground emph-6)

     ;; whitespace
     (whitespace-empty
      :foreground emph-6
      :background emph-4)
     (whitespace-hspace
      :foreground emph-6
      :background foreground-7)
     (whitespace-indentation
      :inherit whitespace-empty)
     (whitespace-space
      :inherit whitespace-hspace)
     (whitespace-space-after-tab
      :inherit whitespace-empty)
     (whitespace-space-before-tab
      :background emph-5
      :foreground emph-4)
     (whitespace-tab
      :inherit whitespace-space)
     (whitespace-trailing
      :background emph-6
      :foreground emph-4)

     (widget-button-pressed
      :foreground emph-6)
     (widget-documentation
      :foreground emph-2)
     (widget-field
      :background background-3)
     (widget-single-line-field
      :background background-2)
     ))
  (subatomic256-set-faces))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'subatomic256)
;;; subatomic256-theme.el ends here
