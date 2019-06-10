;;; kamoji.el --- easily insert kamojis -*- lexical-binding: t -*-

;; Author: Philip K. <philip@warpmail.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: wp
;; URL: https://git.sr.ht/~zge/kamoji.el

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:
;; 
;; Kamojis are eastern/Japanese emoticons, which are usually displayed
;; horizontally, as opposed to the western vertical variants (":^)",
;; ";D", "XP", ...).
;;
;; To achieve this they make much more use of more obscure and often
;; harder to type unicode symbols, which often makes it more difficult
;; to type, or impossible if you don't know the symbols names/numbers.
;;
;; This package tries to make it easier to use kamojis, by using
;; `completing-read' and different categories. The main user functions
;; are therefore `kamoji-insert' to insert a kamoji at point, and
;; `kamoji-insert-kill-ring' to push a kamoji onto the kill ring.
;;
;; The emoticons aren't stored in this file, but (usually) in the
;; KAMOJIS file that should be in the same directory as this source
;; file. This file is parsed by `kamoji--parse-buffer' and then stored
;; in `kamoji-alist'.
;;
;; The kamojis in KAMOJIS have been selected and collected from these
;; sites:
;; - https://wikileaks.org/ciav7p1/cms/page_17760284.html
;; - http://kaomoji.ru/en/
;; - https://en.wikipedia.org/wiki/List_of_emoticons


(require 'ring)

;;; Code:

(defvar kamoji--last-used (make-ring 32)
  "Ring of last kamojis inserted.")
(defvar kamoji--last-category nil
  "Symbol of last category used to insert a kamoji.")



(defun kamoji--parse-buffer (buf)
  "Parse a buffer BUF creating a alist.

Categories are delimited by an group separator (ASCII 35), which
are in turn split into tags and kamojis. These two are kept apart
by a record separator (ASCII 36). Both tags and kamojis split
their unit components by unit separators (ASCII 37)."
  (with-current-buffer buf
    (let (records end)
      (goto-char (point-min))
      (while (save-excursion (setq end (search-forward "" nil t)))
        (save-restriction
          (narrow-to-region (point) end)
          (let* ((names (split-string (buffer-substring
                                       (point-min)
                                       (1- (search-forward "")))
                                      ""))
                 (kamojis (split-string (buffer-substring
                                         (point) (point-max)) "")))
            (dolist (name names)
              (push (cons (string-trim-left name) kamojis)
                    records))))
        (goto-char end))
      records)))

(defun kamoji-parse-file (filename)
  "Parse FILENAME for a list of Kamoji categories."
  (with-temp-buffer
    (insert-file-contents filename)
    (kamoji--parse-buffer (current-buffer))))

(defconst kamoji-alist
  (let* ((dir (if load-file-name
                  (file-name-directory load-file-name)
                default-directory))
         (file (expand-file-name "KAMOJIS" dir)))
    (kamoji-parse-file file))
  "Alist of various kamojis.")



(defun kamoji--select-kamoji (category)
  "General function to interactively select a kamoji.

Will first query a category from `kamoji-alist', then a specific
kamoji within the category. If CATEGORY is non-nil, don't query
the user and just use that category instead."
  (let* ((moods (mapcar #'car kamoji-alist))
         (category (or category (completing-read "Category: " moods nil t)))
         (list (cdr (assoc category kamoji-alist)))
         (kamoji (completing-read (format "Kamoji (%s): " category) list)))
    (setf kamoji--last-category category)
    (ring-insert kamoji--last-used kamoji)
    kamoji))

(defun kamoji--choose-kamoji (arg)
  "Helper function to choose a kamoji.

No prefix argument opens a category then kamoji menu, a single
prefix ARG argument re-opens the last category, and a double prefix
argument lists the last used kamojis."
  (cond ((and (>= arg 16) (< 0 (ring-size kamoji--last-used)))
         (completing-read "Last inserted Kamojis: " (ring-elements kamoji--last-used)))
        ((>= arg 4) (kamoji--select-kamoji kamoji--last-category))
        (t (kamoji--select-kamoji nil))))



;;;###autoload
(defun kamoji-insert-kill-ring (arg)
  "Insert a kamoji directly into `kill-ring'.

See `kamoji--choose-kamoji' in regards to how the prefix argument
ARG is handled."
  (interactive "p")
  (kill-new (kamoji--choose-kamoji arg)))

;;;###autoload
(defun kamoji-insert (arg)
  "Insert a kamoji directly into the current buffer.

See `kamoji--choose-kamoji' in regards to how the prefix argument ARG
is handled."
  (interactive "p")
  (insert (kamoji--choose-kamoji arg)))

(provide 'kamoji)

(provide 'kamoji)

;;; kamoji.el ends here
