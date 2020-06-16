;;; clojure-comment-dwim.el --- Easier Clojure(Script) code commenting.

;;; Commentary:
;;
;; Description
;;
;; This package provides an easy way to (un)comment: Clojure(Script)
;; code. If `clojure-comment-dwim' is invoked it will insert the #_
;; comment to comment out the s-expression at the point.
;;
;; For example, with the below sample code (with '█' being the
;; cursor):
;;
;;  █(ns foo.bar
;;     (:require [foo.baz]))
;;
;; after invoking `clojure-comment-dwim' it would transform to:
;;
;;  #_(ns foo.bar
;;      (:require [foo.baz]))
;;
;; The next invocation of `clojure-comment-dwim' would uncomment the
;; above code.
;;
;; If a prefix argument is passed in, via `C-u n' (where `n' is a
;; digit), it would traverse up `n' s-expressions before inserting the
;; comment.
;;
;; For example, with this code (where `.' is under the cursor `█'):
;;
;;  (ns foo.bar
;;    (:require [foo█baz]))
;;
;; and invocation of C-u -2 M-x clojure-comment-dwim, it would produce
;; the following:
;;
;;  (ns foo.bar
;;    #_(:require [foo.baz]))
;;
;; The mode also operates on regions, prefixing the region with `#_',
;; if it's a single s-expression, or prefixing each part of the region
;; if there are multiple s-expressions.
;;
;; For example, with this code:
;;
;;  (ns foo.bar
;;    (:require [foo.baz]))
;;
;; if the whole of :require clause was selected and
;; `clojure-comment-dwim' invoked, it would produce:
;;
;;  (ns foo.bar
;;    #_(:require [foo.baz]))
;;
;; But if you were to select the middle line here:
;;
;; {:a 1
;;  :b 2
;;  :c 3}
;;
;; it would wrap all with `#_' like:
;;
;; {:a 1
;;  #_:b #_ 2
;;  :c 3}
;;
;; To configure the mode, replace the default `comment-dwim' binding,
;; by adding this to your configuration, in order to have
;; `clojure-comment-dwim' active only in clojure-mode &
;; clojurescript-mode:
;;
;; (add-hook 'clojure-mode-hook
;;           '(lambda ()
;;               (local-set-key (kbd "M-;") #'clojure-comment-dwim)))
;; (add-hook 'clojurescript-mode-hook
;;           '(lambda ()
;;               (local-set-key (kbd "M-;") #'clojure-comment-dwim)))


;;; Code:

(defun clojure-comment-dwim-commentp (arg)
  "Check if the current point `arg' is within comment."
  (string= "font-lock-comment-face"
           (or (get-char-property arg 'read-face-name)
               (get-char-property arg 'face)
               (plist-get (text-properties-at arg) 'face))))


(defun clojure-comment-dwim-move-by-sexp (arg)
  "Move by number of ARG s-expressions.
If going forward, the point is on the opening paren, if going
backward, the point is on the closing paren.  Credit to rightfold
from https://stackoverflow.com/a/5194568"
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (clojure-comment-dwim-move-by-sexp (1- arg)))
          ((up-list arg)))))


(defun clojure-comment-dwim-find-comment-and-delete ()
  (while (not (looking-at "#_\\s-?+"))
    (backward-char))
  (delete-char 2)
  (cond ((and (looking-at " ")
              (looking-back " "))
         (delete-char -1))
        ((looking-back "\\((\\|\\[\\) ") (delete-char -1))))


(defun clojure-comment-dwim-insert-comment ()
  "Insert #_ comment, padding it with whitespace if needed."
  (insert (if (or (looking-back " ")
                  (bolp))
              "#_"
            " #_")))


(defun clojure-comment-dwim-wrap-with-comment (beg end)
  "Wrap a region with the appropriate comment.

   If it's a s-expression, use #_, otherwise add #_ before each
   s-expression."
  (progn
    (goto-char beg)
    (let ((new-end end))
      (while (not (> (point) (1- new-end)))
        (insert " #_")
        (setq new-end (+ new-end 2))
        (forward-sexp)))))

(defun clojure-comment-dwim-on-region (beg end)
  (progn
    (goto-char beg)
    (if (looking-at "\\s-?+#_\\s-?+")
        (progn
          (back-to-indentation)
          (let ((new-end end))
            (while (not (>= (point) (1- new-end)))
              (when (or (looking-at "\\n?\\s-?+#_\\s-?+")
                        (looking-back "\\n?\\s-?+#_\\s-?+"))
                (clojure-comment-dwim (point)))
              (forward-sexp)
              (setq new-end (- new-end 2)))))
      (let ((new-end end))
        (while (not (>= (point) (1- new-end)))
          (if (and (not (looking-at "\\s-?+#_\\s-?+"))
                   (not (looking-back "\\s-?+#_\\s-?+"))
                   (not (eolp))
                   (not (looking-at ")")))
              (progn
                (setq new-end (+ new-end 1))
                (insert "#_"))
            (when (and (not (clojure-comment-dwim-commentp (point)))
                       (not (eolp))
                       (not (looking-at "#_"))
                       (not (looking-at ")"))
                       (not (looking-back "#_")))
              (insert (if (not (looking-at " "))
                          (progn (setq new-end (+ new-end 2))
                                 "#_")
                        (progn (setq new-end (+ new-end 3))
                               " #_")))))
          (if (not (= (point) new-end))
              (if (and (eolp)
                       (save-excursion
                         (backward-char)
                         (not (clojure-comment-dwim-commentp (point)))))
                  (progn (forward-sexp)
                         (backward-sexp)
                         (setq new-end (+ new-end 2))
                         (when (not (looking-at "\\s-?+#_"))
                           (insert "#_")))
                (progn
                  (forward-sexp)
                  (when (and (not (eolp))
                             (not (looking-at ")"))
                             (not (looking-at "]"))
                             (not (looking-at "\\s-?+#_"))
                             ;; not within a comment
                             (not (looking-back "\\s-?+#_")))
                    (insert " #_"))))
            (progn (backward-sexp)
                   (setq new-end (+ new-end 2))
                   (insert "#_"))))
        (when (and (= (point) new-end)
                   (not (clojure-comment-dwim-commentp (point)))
                   (bolp))
          (progn (backward-sexp)
                 (setq new-end (+ new-end 2))
                 (insert "#_")
                 (forward-sexp)))))))


(defun clojure-comment-dwim-delete-comment (arg)
  "Deletion of (comment) type comment."
  (progn (let* ((return-to arg)
                (comment-size (length "(comment"))
                (new-end nil))
           (forward-sexp)
           (delete-char -1)
           (setq new-end (- (point) comment-size))
           (goto-char return-to)
           (let ((count 0))
             (while (and (re-search-forward "(comment " nil t)
                         (< count 1))
               (progn (replace-match "")
                      (setq count (1+ count)))))
           (indent-region (point) new-end))))


;;;###autoload
(defun clojure-comment-dwim (beg &optional end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point))))
  (save-excursion
    (if end
        (clojure-comment-dwim-on-region beg end)
      (if current-prefix-arg
          (progn (clojure-comment-dwim-move-by-sexp current-prefix-arg)
                 (if (not (clojure-comment-dwim-commentp (point)))
                     (clojure-comment-dwim-insert-comment)
                   (clojure-comment-dwim-find-comment-and-delete)))
        (cond ((clojure-comment-dwim-commentp beg)
               (progn (while (clojure-comment-dwim-commentp (point))
                        (backward-char))
                      (clojure-comment-dwim-find-comment-and-delete)))
              ((or (looking-back "#_\\s-?+")
                   (and (looking-back "#")
                        (looking-at "_")))
               (progn (backward-char)
                      (clojure-comment-dwim (point))))
              ((looking-at "#_")
               (let ((count 0))
                 (while (and (re-search-forward "#_\\s-?+" nil t)
                             (< count 1))
                   (progn (replace-match "")
                          (setq count (1+ count))))))
              (t (if (> (first (syntax-ppss)) 0)
                     (if (looking-at "\\s-?+(comment")
                         (clojure-comment-dwim-delete-comment beg)
                       (save-excursion
                         (let* ((found nil)
                                (comment-point nil))
                           (while (and (not found)
                                       (> (first (syntax-ppss)) 0))
                             (backward-up-list 1)
                             (when (looking-at "(comment")
                               (setq found t)
                               (setq comment-point (point))))
                           (if found
                               (progn (goto-char comment-point)
                                      (clojure-comment-dwim-delete-comment (point)))
                             (progn (goto-char beg)
                                    (if (not (looking-at "\\s-?+#_"))
                                        (clojure-comment-dwim-insert-comment)
                                      (let ((count 0))
                                        (while (and (re-search-forward "#_\\s-?+" nil t)
                                                    (< count 1))
                                          (progn (replace-match "")
                                                 (setq count (1+ count)))))))))))
                   (clojure-comment-dwim-insert-comment))))))))

(provide 'clojure-comment-dwim)
;;; clojure-comment-dwim.el ends here
