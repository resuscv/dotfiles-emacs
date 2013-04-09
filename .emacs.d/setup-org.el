;;; Org Mode
;;;
;;; [1] http://doc.norang.ca/org-mode.html
;;; [2] https://github.com/purcell/emacs.d/blob/master/init-org.el
;;; [3] http://doc.norang.ca/org-mode.html#AgendaSetup
;;; [4] https://github.com/cjohansen/.emacs.d/blob/master/setup-org.el
;;; [5] http://orgmode.org/manual/TODO-dependencies.html
;;; [6] http://orgmode.org/worg/org-configs/org-customization-guide.html

;; Standard key bindings  [1]
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; Various preferences [2]
(setq org-log-done t
;      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
 ;     org-fast-tag-selection-single-key 'expert        ;;  I don't know what these mean
 ;     org-export-kill-product-buffer-when-displayed t  ;;  I don't know what these mean
      org-tags-column 80)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
	      (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))

(setq org-tag-alist '((:startgroup . nil)
		      ("work" . ?w) ("home" . ?h)
		      (:endgroup . nil)
		      ("@computer" . ?c) ("@phone" . ?p) ("@reading" . ?r)
		      (:newline . nil)
		      ("@desk" . ?d) ("@email" . ?e) ("@meeting" . ?m)
		      (:newline . nil)
		      ("@SQL" . ?1) ("@R" . ?2) ("@Matlab" . ?3)))



;; Update cookies [4]
(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))


;; Set up org directories, like agenda [3, 6]
(setq org-directory "~/Documents/org")
(setq org-agenda-files (quote ("~/Documents/org"
			       "~/Documents/org/personal"
			       "~/Documents/org/work-main")))


;; Enforce TODO behaviour
(setq org-enforce-todo-dependencies t)


;; Hide those stars
(setq org-startup-indented t)


;; Almost END
(provide 'setup-org)
