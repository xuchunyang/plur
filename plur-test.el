;;; plur-test.el --- Tests of plur.el                -*- lexical-binding: t; -*-

(require 'plur)

(ert-deftest plur-split-string-test ()
  (should (equal (plur-split-string "pre-")              '("pre-")))
  (should (equal (plur-split-string "{AAA,BBB}")         '(("AAA,BBB"))))
  (should (equal (plur-split-string "pre-{AAA,BBB}")     '("pre-" ("AAA,BBB"))))
  (should (equal (plur-split-string "pre-{AAA,BBB}-ing") '("pre-" ("AAA,BBB") "-ing")))
  (should (equal (plur-split-string "beg-{A,B}-mid-{C,D}-end") '("beg-" ("A,B") "-mid-" ("C,D") "-end"))))

(ert-deftest plur-build-regexp-test ()
  (should (string= (plur-build-regexp "child{,ren}") (rx (and "child" (or "" "ren")))))
  (should (string= (plur-build-regexp "beg-{A,B}-mid-{C,D}-end") (rx (and "beg-" (or "A" "B") "-mid-" (or "C" "D") "-end"))))
  (should (string= (plur-build-regexp "{emacs,vim}") (rx (or "emacs" "vim")))))

(ert-deftest plur-replace-test ()
  (cl-flet ((do-replace (text from-string to-string)
                        (with-temp-buffer
                          (insert text)
                          (plur-replace from-string to-string nil 1 (point-max))
                          (buffer-string))))
    (should (string= "The plural form of adult is adults"
                     (do-replace "The plural form of child is children" "child{,ren}" "adult{,s}")))
    ;; Case
    (should (string= "The plural form of Adult is ADULTS"
                     (do-replace "The plural form of Child is CHILDREN" "child{,ren}" "adult{,s}")))
    ;; Swap
    (should (string= "vim and emacs"
                     (do-replace "emacs and vim" "{emacs,vim}" "{vim,emacs}")))))

;;; plur-test.el ends here
