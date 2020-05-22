

;; https://www.emacswiki.org/emacs/RegularExpression

(defun quote-symbol-at-point ()
  (interactive)
  (beginning-of-thing 'symbol)
  (insert ?\`)
  (end-of-thing 'symbol)
  (insert ?`))

(defun quote-keyword-at-point ()
  (interactive)
  (beginning-of-thing 'symbol)
  (backward-char)
  (insert ?\`)
  (forward-char)
  (end-of-thing 'symbol)
  (insert ?`))

(global-set-key (kbd "<f7>") 'quote-keyword-at-point)
(global-set-key (kbd "<f6>") 'quote-symbol-at-point)

