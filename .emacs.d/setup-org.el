;;; Org Mode
;;;
;;; [1] http://doc.norang.ca/org-mode.html
;;; [2] https://github.com/purcell/emacs.d/blob/master/init-org.el
;;; [3] http://doc.norang.ca/org-mode.html#AgendaSetup
;;; [4] https://github.com/cjohansen/.emacs.d/blob/master/setup-org.el
;;; [5] http://orgmode.org/manual/TODO-dependencies.html
;;; [6] http://orgmode.org/worg/org-configs/org-customization-guide.html
;;; [7] http://orgmode.org/manual/Block-agenda.html

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
		      ("@computer" . ?c) ("@email" . ?e) ("@desk" . ?d)
		      (:newline . nil)
		      ("@reading" . ?r) ("@meeting_phone" . ?m)
		      (:newline . nil)
		      ("maintenance" . nil)
		      (:newline . nil)
		      ("coding" . ?1)))


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


;; MobileOrg
(require 'org-mobile)
(setq org-mobile-directory "~/Documents/org/MobileOrg")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Documents/org/mobile-new.org")

(defvar org-mobile-push-timer nil)

(defun org-mobile-push-with-delay (secs)
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))

(add-hook 'after-save-hook
	  (lambda ()
	    (when (eq major-mode 'org-mode)
	      (dolist (file (org-mobile-files-alist))
		(if (string= (expand-file-name (car file)) (buffer-file-name))
		    (org-mobile-push-with-delay 30)))
	      )))

(run-at-time "00:05" 86400 '(lambda () (org-mobile-push-with-delay 1))) ;; refreshes agenda file each day

(defun install-monitor (file secs)
  (run-with-timer
   0 secs
   (lambda (f p)
     (unless (< p (second (time-since (elt (file-attributes f) 5))))
       (org-mobile-pull)))
   file secs))

(install-monitor (file-truename
                  (concat
                   (file-name-as-directory org-mobile-directory)
		   org-mobile-capture-file))
                 5)

;; Do a pull every 5 minutes to circumvent problems with timestamping
;; (ie. dropbox bugs)
(run-with-timer 0 (* 5 60) 'org-mobile-pull)


;; Enforce TODO behaviour
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-include-all-todo nil)


;; Agenda setup
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-todo-ignore-scheduled t)

;; Hide those stars
(setq org-startup-indented t)


;; Custom agenda - weekly agenda and global TODO
(setq org-agenda-custom-commands
      '(("h" "Agenda and Home-related tasks"
	 ((agenda "")
	  (tags-todo "home")
;	  (tags "garden")
	  ))
	("w" "Agenda and Office-related tasks"
	 ((agenda "")
	  (tags-todo "@email+work")
	  (tags-todo "@computer+work")
	  (tags-todo "@coding+work")
	  (tags-todo "@desk+work")
	  (tags-todo "@reading+work")
	  (tags-todo "@meeting_phone+work")
	  (tags-todo "work-@email-@computer-@coding-@desk-@reading-@meeting_phone")
	  ))))

;; Almost END
(provide 'setup-org)
