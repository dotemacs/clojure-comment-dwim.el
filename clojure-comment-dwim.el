;;; clojure-comment-dwim.el --- Comment Clojure(Script) by cycling through comment order.

;;; Commentary:
;;
;; Description
;;
;; This package provides an easy way to (un)comment: Clojure(Script)
;; code, by cycling through different comment: types: `#_' & `;;',
;; which are inserted at the beginning of the given line.
;;
;; There is an optional customisation via
;; `clojure-comment-dwim-ignore-trailing-comment', when set the
;; comments will be cycled and the very last comment: will add a
;; trailing comment (mimicking the behaviour of comment-dwim-2):
;;
;;     (fn [x] (inc x)) ; <- this trailing comment
;;
;; You can further customise the mode by changing the order in which
;; the comments are cycled. By default it's the setting of
;; `clojure-comment-dwim-order', but you can reverse the order to suit
;; your taste.
;;
;; To configure the mode, replace the default `comment-dwim' binding,
;; by adding this to your configuration:
;;
;; (add-hook 'clojure-mode-hook
;;           '(lambda ()
;;               (local-set-key (kbd "M-;") #'clojure-comment-dwim)))
;; (add-hook 'clojurescript-mode-hook
;;           '(lambda ()
;;               (local-set-key (kbd "M-;") #'clojure-comment-dwim)))


;;; Code:

(setq clojure-comment-dwim-order ["#_" ";;"])
;;(setq clojure-comment-dwim-order [";;" "#_"])

(setq clojure-comment-dwim-ignore-trailing-comment t)
;;(setq clojure-comment-dwim-ignore-trailing-comment nil)

(defun clojure-comment-dwim-insert-comment (arg)
  "Determine what type of comment: should be inserted based on the `arg'."
  (let ((arg-position (1+ (seq-position clojure-comment-dwim-order arg))))
    (message "called 2")
    (if (= arg-position (length clojure-comment-dwim-order))
        (progn
          (delete-region (point) (+ 2 (point))))
      (progn (delete-region (point) (+ 2 (point)))
             (insert (aref clojure-comment-dwim-order arg-position))))))

(defun clojure-comment-dwim-next-comment-or-uncomment (&optional arg)
  (if arg
      (clojure-comment-dwim-insert-comment arg)
    (insert (aref clojure-comment-dwim-order 0))))

(defun clojure-comment-dwim-comment-at-eol ()
  (progn (end-of-line)
         (nth 4 (syntax-ppss))))

(defun clojure-comment-dwim-trailling-comment ()
  (progn (delete-region (point) (+ 2 (point)))
         (end-of-line)
         (insert " ;")))

(defun clojure-comment-dwim-leading-comment (comment:)
  (if (and (not clojure-comment-dwim-ignore-trailing-comment)
           (string= comment: (aref clojure-comment-dwim-order 1)))
      (clojure-comment-dwim-trailling-comment)
    (clojure-comment-dwim-next-comment-or-uncomment comment:)))

(defun clojure-comment-dwim-trailing-comment-or-no-comment ()
  (if (clojure-comment-dwim-comment-at-eol)
      (progn (goto-char (line-beginning-position))
             (let ((beg (comment-search-forward (line-end-position) t)))
               (when beg (goto-char (- (point) 2))))
             (kill-line)
             (end-of-line))
    (progn (back-to-indentation)
           (clojure-comment-dwim-next-comment-or-uncomment))))

;;;###autoload
(defun clojure-comment-dwim ()
  "Cycle comments for Clojure(Script): `#_', `;;' & `;' (but only
at the end of line). The order they are inserted is defined in
`clojure-comment-dwim-order' (where only the order of `;;' & `#_'
can be defined, the third comment: type, a single `;' at the end
of line is only turned on if
`clojure-comment-dwim-ignore-trailing-comment' is set to false)."
  (interactive)
  (if (use-region-p)
      (comment-region)
    (progn (back-to-indentation)
           (cond ((looking-at "#_")
                  (clojure-comment-dwim-leading-comment "#_"))
                 ((looking-at ";;")
                  (clojure-comment-dwim-leading-comment ";;"))
                 (t (clojure-comment-dwim-trailing-comment-or-no-comment))))))

(provide 'clojure-comment-dwim)
;;; clojure-comment-dwim.el ends here
