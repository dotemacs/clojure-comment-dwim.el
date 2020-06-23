(require 'ert)
(load-file "clojure-comment-dwim.el")


(defmacro test-case (test-name doc-string input test-steps output)
  "TEST-NAME is a name of the test as a symbol.
   DOC-STRING, description of the test.
   INPUT is a string.
   TEST-STEPS is a list of steps needed to preform.
   OUTPUT is the outcome of the INPUT after TEST-STEPS
   have been run."
  `(ert-deftest ,(intern-soft (eval test-name)) ()
     ,doc-string
     (should
      (string=
       (with-temp-buffer
         (progn (clojure-mode)
                (insert ,input)
                ,@test-steps
                (buffer-string)))
       ,output))))


(defun jump-to (point &optional doc-string)
  "Just a helpful crutch that allows for the DOC-STRING in the
code, where you can describe where the POINT should be at."
  (goto-char point))


(test-case 'ns-commenting
           "Simple ns commenting"
           "
(ns foo.bar
  (:require [foo.baz]))"
           ((goto-char 2)
            (clojure-comment-dwim (point)))
           "
#_(ns foo.bar
  (:require [foo.baz]))")


(test-case 'backward-commenting
           "Commenting sexpressions backward"
           "
(ns foo.bar
  (:require [foo.bar]))"
           ((jump-to 34 "on the closing ]")
            (clojure-comment-dwim-move-by-sexp -2)
            (clojure-comment-dwim (point)))
           "
(ns foo.bar
  #_(:require [foo.bar]))")


(test-case 'uncommenting-in-the-middle-of-a-map
           "Uncommenting the map the point happens to be in."
           "(foo #_{:bar :baz})"
           ((jump-to 13 "between :bar & :baz")
            (clojure-comment-dwim-find-comment-and-delete))
           "(foo {:bar :baz})")


(test-case 'commenting-in-middle-of-maps
           "Commenting a key-value pair in a middle of a map"
           "
{:a 1
 :b 2
 :c 3}"
           ((jump-to 9 "on ':' of :b")
            (push-mark)
            (end-of-line)
            (clojure-comment-dwim (region-beginning) (region-end)))
           "
{:a 1
 #_:b #_ 2
 :c 3}")
;; TODO: fix this so that there is no space before the second '#_'. Or
;; maybe remove the space altogether. But look at this none the less.


(test-case 'commenting-in-middle-of-sexp
           "Commenting a region, where one sexp is already commented out"
           "
(do
  (do
    (inc 1)
    #_(inc 2)))"
           ((jump-to 16 "on '(' of (inc 1)")
            (push-mark)
            (jump-to 37 "on ')' of the second do")
            (clojure-comment-dwim (region-beginning) (region-end)))
           "
(do
  (do
    #_(inc 1)
    #_(inc 2)))")


(test-case 'commenting-a-region
           "Commenting a region, but without commenting a trailing ]"
           "
(defn foo
  []
  (let [a (inc 1)]
    (inc a)))"
           ((jump-to 25 "on 'a' within let")
            (push-mark)
            (jump-to 35 "after ']' of let")
            (clojure-comment-dwim (region-beginning) (region-end)))
           "
(defn foo
  []
  (let [#_a #_ (inc 1)]
    (inc a)))")

(test-case 'dont-comment-curly-brace
           "Commenting a region, but without commenting a trailing }"
           "
{:deps {foo/bar {:mvn/version \"0.0.1\"}"
           ((jump-to 19 "on ':' of :mvn")
            (push-mark)
            (jump-to 42 "after '}'")
            (clojure-comment-dwim (region-beginning) (region-end)))
           "
{:deps {foo/bar {#_:mvn/version #_ \"0.0.1\"}")


(test-case 'uncommenting-comment-of-type-comment
           "Uncommenting `comment' type comment."
           "
(do
  (do
    (comment (inc 1))
    #_(inc 2)))"
           ((jump-to 16 "on '(' of (comment")
            (clojure-comment-dwim (point)))
           "
(do
  (do
    (inc 1)
    #_(inc 2)))")


(test-case 'commenting-region-with-a-comment
           "Commenting a region that already has a comment."
           "
(do
  (do
    (inc 1)
    #_(inc 2)))"
           ((jump-to 16 "on '(' of (inc 1)")
            (clojure-comment-dwim (point)))
           "
(do
  (do
    #_(inc 1)
    #_(inc 2)))")


(test-case 'uncommenting-a-region
           "Uncommenting a region"
           "
(do
  (do
    #_(inc 1)
    #_(inc 2)))"
           ((jump-to 16 "on '#' of #_(inc 1)")
            (push-mark)
            (jump-to 39 "on the closing ')' of the second do")
            (clojure-comment-dwim (region-beginning) (region-end)))
           "
(do
  (do
    (inc 1)
    (inc 2)))")

(ert t)
