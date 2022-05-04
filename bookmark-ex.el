;;; bookmark-ex.el -*- coding: utf-8-emacs -*-
;; Copyright (C) 2021, 2022 fubuki

;; Author: fubuki@frill.org
;; Version: @(#)$Revision: 1.10 $$Name:  $
;; Keywords: bookmark, matching

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extend for `bookmark.el.

;;; Installation:

;; (require 'bookmark-ex)

;;; Change Log:

;;; Code:
(require 'bookmark)
(defvar bookmark-mark-ring nil)
(setq bookmark-sort-flag nil)
(add-hook 'bookmark-after-jump-hook 'bookmark-last-visit-up)

(when (boundp 'show-marks-add-mark-ring)
  (setq show-marks-add-mark-ring '(bookmark-mark-ring global-mark-ring)))

(defun bookmark-last-visit-up ()
  "last visit bookmark move to top And collect same files.
If include bookmark-mark-ring in `show-marks-add-mark-ring'
then make `bookmark-mark-ring'."
  (let ((last (bookmark-get-bookmark (car bookmark-history)))
        file common others)
    (setq file (assoc-default 'filename last))
    (setq bookmark-alist (delete last bookmark-alist))
    (dolist (a bookmark-alist)
      (if (equal file (assoc-default 'filename a))
          (setq common (cons a common))
        (setq others (cons a others))))
    (when (and (fboundp 'show-marks-add-mark-ring)
               (member 'bookmark-mark-ring show-marks-add-mark-ring))
      (save-excursion
        (bookmark-default-handler last)
        (setq bookmark-mark-ring (list (copy-marker (point)))))
      (dolist (a common)
        (save-excursion
          (bookmark-default-handler a)
          (setq bookmark-mark-ring
                (cons (copy-marker (point)) bookmark-mark-ring)))))
    (and (boundp 'bookmark-alist-modification-count)
         (setq bookmark-alist-modification-count
               (1+ bookmark-alist-modification-count)))
    (setq bookmark-alist
          (cons last (append (reverse common) (reverse others))))))

;; bookmark-file-name+line-number part.
(defun bookmark-file-name+line-number ()
  "Return string \"filename:linenumber\"."
  (interactive)
  (let (file ln)
    (with-current-buffer bookmark-current-buffer
      (setq file (file-name-nondirectory (buffer-file-name))
            ln (line-number-at-pos)))
    (insert (format "%s:%s" file ln))))

(defvar bookmark-minibuffer-read-name-ex-map
  (let ((map  (make-sparse-keymap))
        (menu (make-sparse-keymap "minibuf")))
    (set-keymap-parent map (copy-keymap minibuffer-local-map))
    (define-key map "\C-w" 'bookmark-yank-word)
    (define-key map "\C-c#" 'bookmark-file-name+line-number)
    map))

(define-key-after
  (lookup-key bookmark-minibuffer-read-name-ex-map [menu-bar minibuf])
  [filenamelinenumber]
  '("Insert Filename:Linenumber" . bookmark-file-name+line-number)
  'isearch-forward)

(define-key-after
  (lookup-key bookmark-minibuffer-read-name-ex-map [menu-bar minibuf])
  [yankword]
  '("Yank word" . bookmark-yank-word)
  'isearch-forward)

(define-key-after
  (lookup-key bookmark-minibuffer-read-name-ex-map [menu-bar minibuf])
  [rem1] '("--") 'isearch-forward)

(define-key-after
  (lookup-key bookmark-minibuffer-read-name-ex-map [menu-bar minibuf])
  [rem2] '("--") 'filenamelinenumber)

(setq bookmark-minibuffer-read-name-map bookmark-minibuffer-read-name-ex-map)

;; bookmark-set-name-wap part.
(defun bookmark-set-name-wap (bookmark-name-or-record newname)
  "Set BOOKMARK-NAME-OR-RECORD's name to NEWNAME."
  (if (and (string-equal "" newname)
           (stringp bookmark-name-or-record))
      (setq newname bookmark-name-or-record))
  (setcar (bookmark-get-bookmark bookmark-name-or-record) newname))

(advice-add 'bookmark-set-name :override 'bookmark-set-name-wap)

;; bookmark-maybe-sort-alist-wap part.
(defface bookmark-directory
  '((t :inherit dired-directory))
  "bookmark bmenu directory face."
  :group 'bookmark)

(defun string-clone-copy-alist (alist)
  "copy-alist to clone the CAR string."
  (let (result)
    (dolist (a alist (reverse result))
      (setq result (cons (cons (substring (car a)) (cdr a)) result)))))

(defun bookmark-maybe-sort-alist-wap ()
  "Colors the Directory of `bookmark-alist' bookmark names."
  (let ((seq (string-clone-copy-alist bookmark-alist))
        name)
    (dolist (a seq)
      (setq name (assoc-default 'filename a))
      (and (string-match "/\\'" name)
           (add-text-properties
            0 (length (car a)) '(face bookmark-directory directory t) (car a))))
    (if bookmark-sort-flag
        (sort seq (function (lambda (x y) (string-lessp (car x) (car y)))))
      (if (= 28 emacs-major-version)
          (reverse seq)
        seq))))

(advice-add 'bookmark-maybe-sort-alist :override 'bookmark-maybe-sort-alist-wap)

(if (<= 28 emacs-major-version)
    (add-hook 'bookmark-bmenu-mode-hook
              #'(lambda () (setq tabulated-list-sort-key nil))))

;; bookmark-bmenu-mode-map add part.
(defun bookmark-next-directory (arg)
  (interactive "p")
  (dotimes (i arg)
    (text-property-search-forward 'directory t)))

(defun bookmark-previous-directory (arg)
  (interactive "p")
  (dotimes (i arg)
    (text-property-search-backward 'directory t t t)))

(add-hook 'bookmark-bmenu-mode-hook
          #'(lambda ()
              (local-set-key ">" 'bookmark-next-directory)
              (local-set-key "<" 'bookmark-previous-directory)))

;; show-annotation open window part.
(add-to-list 'display-buffer-alist
             '("\\*Bookmark Annotation\\*"
               (display-buffer-at-bottom display-buffer-below-selected)
               (window-height . fit-window-to-buffer)))

(provide 'bookmark-ex)
;; fin.
