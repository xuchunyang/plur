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

(defun abolish-split-string (s)
  (let ((start 0) strings aux)
    (while (string-match "{\\([^{}]*\\)}" s start)
      (let ((prefix (substring s start (match-beginning 0))))
        (unless (string= "" prefix)
          (push prefix strings)))
      (push (list (match-string 1 s)) strings)
      (setq start (match-end 0)))
    (when (/=  start (- (length s) 1))
      (push (substring s start) strings ))
    (nreverse strings)))

(defun abolish-build-rx-form (strings)
  (let ((form '(and)))
    (dolist (item strings form)
      (setq form
            (append form (if (stringp item)
                             (list item)
                           (list (append '(or) (split-string (car item) ",")))))))))

(defun abolish-isearch-search-func ()
  "Return a function to use for the search."
  (lambda (string &optional bound noerror count)
    (let ((s (rx-to-string
              (abolish-build-rx-form
               (abolish-split-string string)))))
      (condition-case err
          (funcall
           (if isearch-forward #'re-search-forward #'re-search-backward)
           s
           bound noerror count)
        (search-failed nil)))))

(setq isearch-search-fun-function 'abolish-isearch-search-func)

(provide 'abolish)
;;; abolish.el ends here
