;;; plur-test.el --- Tests of plur.el                -*- lexical-binding: t; -*-

(require 'plur)

(ert-deftest plur-split-string-test ()
  (should (equal (plur-split-string "pre-")              '("pre-")))
  (should (equal (plur-split-string "{AAA,BBB}")         '(("AAA,BBB"))))
  (should (equal (plur-split-string "pre-{AAA,BBB}")     '("pre-" ("AAA,BBB"))))
  (should (equal (plur-split-string "pre-{AAA,BBB}-ing") '("pre-" ("AAA,BBB") "-ing"))))

;;; plur-test.el ends here
