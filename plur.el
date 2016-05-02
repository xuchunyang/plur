;;; plur.el --- Easily (I hope) search and replace multiple variants of a word  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; URL: https://github.com/xuchunyang/plur
;; Version: 0.01
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5"))

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
;; ** Usage

;; To replace "mouse" with "cat" and "mice" with "cats" using:

;; #+BEGIN_SRC undefined
;; M-x plur-query-replace RET m{ouse,ice} RET cat{,s} RET
;; #+END_SRC

;; For more examples,

;; - Facility to Building

;; facilit{y,ies}  building{,s}

;; - Mouse to Trackpad

;; m{ouse,ice}  trackpad{,s}

;; - Swap Emacs and Vim

;; {emacs,vim}  {vim,emacs}

;; To search "mouse" and "mice" using:

;; #+BEGIN_SRC undefined
;; M-x plur-isearch-mode RET
;; C-s m{ouse,ice}
;; #+END_SRC

;; ** Acknowledge

;; This package is inspired by [[https://github.com/tpope/vim-abolish][vim-abolish]].

;;; Code:

(require 'cl-lib)

(defun plur-split-string (s)
  ;; "m{ice,ouse}" => ("m" ("ice,ouse"))
  (let ((start 0) strings)
    (while (string-match "{\\([^{}]*\\)}" s start)
      (let ((prefix (substring s start (match-beginning 0))))
        (unless (string= "" prefix)
          (push prefix strings)))
      (push (list (match-string 1 s)) strings)
      (setq start (match-end 0)))
    (let ((tail (substring s start)))
      (unless (string= "" tail)
        (push tail strings)))
    (nreverse strings)))

(defun plur-build-rx-form (strings)
  (let ((form '(and)))
    (dolist (item strings form)
      (setq form
            (append form (if (stringp item)
                             (list item)
                           (list (append '(or) (split-string (car item) ",")))))))))

(defun plur-isearch-search-func ()
  "Return a function to use for the search."
  (lambda (string &optional bound noerror count)
    (let ((s (rx-to-string
              (plur-build-rx-form
               (plur-split-string string)))))
      (condition-case nil
          (funcall
           (if isearch-forward #'re-search-forward #'re-search-backward)
           s
           bound noerror count)
        (search-failed nil)))))

;;;###autoload
(define-minor-mode plur-isearch-mode
  "Inject plur into isearch."
  :global t
  (if plur-isearch-mode
      (setq isearch-search-fun-function 'plur-isearch-search-func)
    (setq isearch-search-fun-function 'isearch-search-fun-default)))

(defun plur-normalize-strings (strings)
  ;; ("m" ("ice,ouse") => (("m") ("ice" "ouse"))
  (let (result)
    (dolist (elt strings)
      (if (stringp elt)
          (push (list elt) result)
        (push (split-string (car elt) ",") result)))
    (nreverse result)))

(defun plur-expand-string (string)
  ;; facilit{y,ies} => ("facility" "facilities")
  (let ((strings (plur-normalize-strings
                  (plur-split-string string)))
        (results '("")) aux)
    (dolist (elt strings results)       ; List
      (setq aux nil)
      (dolist (elt1 elt)                ; String
        (dolist (prefix results)        ; String
          (push (concat prefix elt1) aux)))
      (setq results (nreverse aux)))))

;;;###autoload
(defun plur-query-replace (from-string to-string &optional delimited start end backward)
  "Adapt from `query-replace'."
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Plur Query replace"
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
           (nth 3 common))))
  (let ((matches
         (cl-mapcar 'cons
                    (plur-expand-string from-string)
                    (plur-expand-string to-string))))
    (setq to-string (cons (lambda (_data _count)
                            (cdr (assoc (match-string 0) matches)))
                          nil)))
  (setq from-string (rx-to-string
                     (plur-build-rx-form
                      (plur-split-string from-string))))
  (perform-replace from-string to-string t t delimited nil nil start end backward))

;;;###autoload
(defun plur-replace (from-string to-string &optional delimited start end backward)
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Plur Replace"
                   (if current-prefix-arg
                       (if (eq current-prefix-arg '-) " backward" " word")
                     "")
                   (if (use-region-p) " in region" ""))
           t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end))
           (nth 3 common))))
  (let ((matches
         (cl-mapcar 'cons
                    (plur-expand-string from-string)
                    (plur-expand-string to-string))))
    (setq to-string (cons (lambda (_data _count)
                            (cdr (assoc (match-string 0) matches)))
                          nil)))
  (setq from-string (rx-to-string
                     (plur-build-rx-form
                      (plur-split-string from-string))))
  (perform-replace from-string to-string nil t delimited nil nil start end backward))


(provide 'plur)

;; Local Variables:
;; fill-column: 90
;; End:

;;; plur.el ends here
