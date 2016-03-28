;;; abolish.el --- Emacs Port of abolish.vim  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/emacs-abolish
;; Version: 0.01
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)

(defun abolish-split (string)
  (save-match-data
    (if (string-match "{\\(.*?\\)}" string)
        (let ((head (substring string 0 (match-beginning 0)))
              (body (match-string 1 string))
              (tail (substring string (match-end 0))))
          (mapcar (lambda (variety)
                    (concat head variety tail))
                  (split-string body ",")))
      (list string))))

;;;###autoload
(defun abolish-replace (from-string to-string)
  (interactive
   (let* ((from (read-from-minibuffer "Replace string: "))
          (to (read-from-minibuffer (format "Replace string %s with: " from))))
     (list from to)))
  (let* ((search (abolish-split from-string))
         (replace (abolish-split to-string))
         (s-r
          (cl-mapcar
           #'cons
           search
           ;; Make sure `search' and `replace' contain the same number of elements
           (if (= (length search) (length replace))
               replace
             (make-list (length search) (car replace)))))
         (replace-count 0))
    (while (re-search-forward (regexp-opt search) nil t)
      (let* ((match (downcase (match-string 0)))
             (replace (cdr (assoc match s-r))))
        (replace-match replace)
        (setq replace-count (+ 1 replace-count))))
    (message "Replaced %d occurrence(s)" replace-count)))

(provide 'abolish)
;;; abolish.el ends here
