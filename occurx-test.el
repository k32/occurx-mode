;;; occurx-test.el ---                              -*- lexical-binding: t; -*-

(require 'occurx-mode)

;;; Interval handling

(ert-deftest occurx--match-intervals-t ()
  (should (equal (occurx--match-intervals 0 0 50 '((0 1 hi-green)))
                 '((0 1   hi-green)
                   (1 11  nil))))
  (should (equal (occurx--match-intervals 10 20 50 '((30 35 hi-green)))
                 '((10 20 font-lock-comment-face)
                   (20 30 nil)
                   (30 35 hi-green)
                   (35 45 nil))))
  (should (equal (occurx--match-intervals 0 0 50 '((45 50 hi-red)))
                 '((35 45 nil)
                   (45 50 hi-red)))))

;;; Matcher

(ert-deftest occurx--matcher-create-t ()
  (let ((m1 (occurx-matcher-create :re '(or "aa" "bb")))
        (m2 (occurx-matcher-create :re "foo" :n-matches 10 :sub-expr 1)))
    (should (equal (occurx-matcher-re m1)
                   "\\(?:aa\\|bb\\)"))
    (should (equal (occurx-matcher-n-matches m1)
                   1))
    (should (equal (occurx-matcher-sub-expr m1)
                   0))
    (should (equal (occurx-matcher-sub-expr m2)
                   1))
    (should (equal (occurx-matcher-n-matches m2)
                   10))))

;;; Pattern

(ert-deftest occurx--pattern-t ()
  (setq-local occurx--default-faces nil)
  (setq-local occurx-default-faces '(hi-pink hi-green hi-blue hi-yellow hi-red-b))
  (should (equal (occurx-pattern-create '("foo"))
                 #s(occurx-pattern (#s(occurx-matcher "foo" 1 0))
                                   nil
                                   hi-pink
                                   nil)))
  (should (equal (occurx-pattern-create '(:not "foo" "bar"))
                 #s(occurx-pattern (#s(occurx-matcher "bar" 1 0))
                                   (#s(occurx-matcher "foo" 1 0))
                                   hi-green
                                   nil)))

  (should (equal (occurx-pattern-create '(:sub 5 :n 6 "foo" "bar" :face hi-black-hb))
                 #s(occurx-pattern (#s(occurx-matcher "bar" 1 1) #s(occurx-matcher "foo" 6 5))
                                   nil
                                   hi-black-hb
                                   nil))))

;;; Pattern buffer

;;; Occur
