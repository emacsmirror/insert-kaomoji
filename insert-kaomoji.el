;;; insert-kaomoji.el --- Easily insert kaomojis -*- lexical-binding: t -*-

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <~pkal/public-inbox@lists.sr.ht>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: wp
;; URL: https://git.sr.ht/~pkal/insert-kaomoji

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:
;;
;; Kaomojis are eastern/Japanese emoticons, which are usually displayed
;; horizontally, as opposed to the western vertical variants (":^)",
;; ";D", "XP", ...).
;;
;; To achieve this they make much more use of more obscure and often
;; harder to type unicode symbols, which often makes it more difficult
;; to type, or impossible if you don't know the symbols names/numbers.
;;
;; This package tries to make it easier to use kaomojis, by using
;; `completing-read' and different categories.  The main user
;; functions are therefore `insert-kaomoji' to insert a kaomoji at
;; point, and `insert-kaomoji-into-kill-ring' to push a kaomoji onto
;; the kill ring.
;;
;; The emoticons aren't stored in this file, but (usually) in the
;; KAOMOJIS file that should be in the same directory as this source
;; file.  This file is parsed during byte-compilation and then stored
;; in `insert-kaomoji-alist'.
;;
;; The kaomojis in KAOMOJIS have been selected and collected from these
;; sites:
;; - https://wikileaks.org/ciav7p1/cms/page_17760284.html
;; - http://kaomoji.ru/en/
;; - https://en.wikipedia.org/wiki/List_of_emoticons

(require 'subr-x)
(require 'ring)

;;; Code:

(defvar insert-kaomoji--last-used (make-ring 32)
  "Ring of last kaomojis inserted.")
(defvar insert-kaomoji--last-category nil
  "Symbol of last category used to insert a kaomoji.")



(defconst insert-kaomoji-alist
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name
      "KAOMOJIS"
      (file-truename
       (if load-file-name
           (file-name-directory load-file-name)
         default-directory))))
    (let (records end)
      (goto-char (point-min))
      (while (save-excursion (setq end (search-forward "" nil t)))
        (save-restriction
          (narrow-to-region (point) (1- end))
          (let* ((names (split-string (buffer-substring
                                       (point-min)
                                       (1- (search-forward "")))
                                      ""))
                 (kaomojis (split-string (buffer-substring
                                          (point) (point-max))
                                         "")))
            (dolist (name names)
              (push (cons (string-trim-left name) kaomojis)
                    records))))
        (goto-char end))
      records))
  "Alist of various kaomojis.")



(defun insert-kaomoji--select-kaomoji (category)
  "General function to interactively select a kaomoji.

Will first query a category from `insert-kaomoji-alist', then a
specific kaomoji within the category.  If CATEGORY is non-nil,
don't query the user and just use that category instead."
  (let* ((moods (mapcar #'car insert-kaomoji-alist))
         (category (or category (completing-read "Category: " moods nil t)))
         (list (cdr (assoc category insert-kaomoji-alist)))
         (kaomoji (completing-read (format "Kaomoji (%s): " category) list)))
    (setf insert-kaomoji--last-category category)
    (ring-insert insert-kaomoji--last-used kaomoji)
    kaomoji))

(defun insert-kaomoji--choose-kaomoji (arg)
  "Helper function to choose a kaomoji.

No prefix argument opens a category then kaomoji menu, a single
prefix ARG argument re-opens the last category, and a double prefix
argument lists the last used kaomojis."
  (cond ((and (>= arg 16) (< 0 (ring-size insert-kaomoji--last-used)))
         (completing-read "Last inserted Kaomojis: "
                          (ring-elements insert-kaomoji--last-used)))
        ((>= arg 4) (insert-kaomoji--select-kaomoji insert-kaomoji--last-category))
        (t (insert-kaomoji--select-kaomoji nil))))



;;;###autoload
(defun insert-kaomoji-into-kill-ring (arg)
  "Insert a kaomoji directly into `kill-ring'.

See `insert-kaomoji--choose-kaomoji' in regards to how the prefix argument
ARG is handled."
  (interactive "p")
  (kill-new (insert-kaomoji--choose-kaomoji arg)))

;;;###autoload
(defun insert-kaomoji (arg)
  "Insert a kaomoji directly into the current buffer.

See `insert-kaomoji--choose-kaomoji' in regards to how the prefix argument ARG
is handled."
  (interactive "p")
  (insert (insert-kaomoji--choose-kaomoji arg)))

(provide 'insert-kaomoji)

;;; insert-kaomoji.el ends here
