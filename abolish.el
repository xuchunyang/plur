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
  ;; "m{ice,ouse}" => ("m" ("ice,ouse"))
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

(defun abolish-normalize-strings (strings)
  ;; ("m" ("ice,ouse") => (("m") ("ice" "ouse"))
  (let (result)
    (dolist (elt strings)
      (if (stringp elt)
          (push (list elt) result)
        (push (split-string (car elt) ",") result)))
    (nreverse result)))

(defun abolish-expand-string (string)
  ;; facilit{y,ies} => ("facility" "facilities")
  (let ((strings (abolish-normalize-strings
                  (abolish-split-string string)))
        (results '("")) aux)
    (dolist (elt strings results)       ; List
      (setq aux nil)
      (dolist (elt1 elt)                ; String
        (dolist (prefix results)        ; String
          (push (concat prefix elt1) aux)))
      (setq results (nreverse aux)))))

(defun abolish-query-replace (from-string to-string &optional delimited start end backward region-noncontiguous-p)
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Query replace"
                   (if current-prefix-arg
                       (if (eq current-prefix-arg '-) " backward" " word")
                     "")
                   (if (use-region-p) " in region" ""))
           nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           ;; These are done separately here
           ;; so that command-history will record these expressions
           ;; rather than the values they had this time.
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end))
           (nth 3 common)
           (if (use-region-p) (region-noncontiguous-p)))))
  (let ((matches
         (cl-mapcar 'cons
                    (abolish-expand-string from-string)
                    (abolish-expand-string to-string))))
    (setq to-string (cons (lambda (_data _count)
                            (cdr (assoc (match-string 0) matches)))
                          nil)))
  (setq from-string (rx-to-string
                     (abolish-build-rx-form
                      (abolish-split-string from-string))))
  (perform-replace from-string to-string t t delimited nil nil start end backward region-noncontiguous-p))


(provide 'abolish)
;;; abolish.el ends here
