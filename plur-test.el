;;; plur-test.el --- Tests of plur.el                -*- lexical-binding: t; -*-

(require 'plur)

(ert-deftest plur-split-string-test ()
  (should (equal (plur-split-string "pre-")              '("pre-")))
  (should (equal (plur-split-string "{AAA,BBB}")         '(("AAA,BBB"))))
  (should (equal (plur-split-string "pre-{AAA,BBB}")     '("pre-" ("AAA,BBB"))))
  (should (equal (plur-split-string "pre-{AAA,BBB}-ing") '("pre-" ("AAA,BBB") "-ing"))))

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
