; For Emacs 24.x
(require 'gnus-art)

; I <3 orgmode
(require 'org-notmuch)
(define-key global-map "\C-cl" 'org-store-link)


;(defun notmuch-refresh-unstupify ()
;  "Fix the point appearing in random locations when refreshing
;the hello screen"
;  (goto-char (point-min))
;  (goto-char (or (search-forward "unread" nil t)
;                 (search-forward "inbox" nil t)))
;  (backward-word))
;(add-hook 'notmuch-hello-refresh-hook 'notmuch-refresh-unstupify)
