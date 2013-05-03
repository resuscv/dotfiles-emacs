;; Org Mode
;;;
;;; [1] http://doc.norang.ca/org-mode.html
;;; [2] https://github.com/purcell/emacs.d/blob/master/init-org.el
;;; [3] http://doc.norang.ca/org-mode.html#AgendaSetup
;;; [4] https://github.com/cjohansen/.emacs.d/blob/master/setup-org.el
;;; [5] http://orgmode.org/manual/TODO-dependencies.html
;;; [6] http://orgmode.org/worg/org-configs/org-customization-guide.html
;;; [7] http://orgmode.org/manual/Block-agenda.html


;; Use the git version of org-mode
(add-to-list 'load-path (expand-file-name "~/software/git/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)


;; Standard key bindings  [1]
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; Various preferences [2]
(setq org-log-done t
;      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 8                                 ;; Show one day ahead next week
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
 ;     org-fast-tag-selection-single-key 'expert        ;;  I don't know what these mean
 ;     org-export-kill-product-buffer-when-displayed t  ;;  I don't know what these mean
      org-tags-column 80
      org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil
      )



(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
	      (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))

(setq org-tag-alist '((:startgroup . nil)
		      ("work" . ?w) ("home" . ?h) ("other_organisations" . ?o)
		      (:endgroup . nil)
		      ("@computer" . ?c) ("@email" . ?e) ("@desk" . ?d)
		      (:newline . nil)
		      ("@reading" . ?r) ("@meeting_phone" . ?m) ("@shopping" . ?s)
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
(setq org-agenda-files (quote ("~/Documents/org")))


;; Capture setup
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;; Enforce TODO behaviour
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-agenda-include-all-todo nil)


;; Agenda setup
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-scheduled 'future)
;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Hide those stars
(setq org-startup-indented t)

(setq org-startup-folded 'overview)

;; Custom agenda - weekly agenda and global TODO
(setq org-agenda-custom-commands
      '(("h" "Agenda and Home-related tasks"
	 (
	  (tags-todo "@email+home/TODO")
	  (tags-todo "@computer+home/TODO")
	  (tags-todo "@desk+home/TODO")
	  (tags-todo "@reading+home/TODO")
	  (tags-todo "@meeting_phone+home/TODO")
	  (tags-todo "@shopping+home/TODO")
	  (tags-todo "@maintenance+home/TODO")
	  (tags-todo "-{@+}+home/TODO")
	  (tags-todo "home/-TODO")
;	  (tags "garden")
	  (tags-todo "other_organisations")
	  (agenda "")
	  ))
	("w" "Agenda and Office-related tasks"
	 (
;	  (tags-todo "@email+work/TODO" ((org-agenda-overriding-header "Context: @email")))
	  (tags-todo "@email+work/TODO")
	  (tags-todo "@computer+work/TODO")
	  (tags-todo "coding+work/TODO")
	  (tags-todo "@desk+work/TODO")
	  (tags-todo "@reading+work/TODO")
	  (tags-todo "@meeting_phone+work/TODO")
	  (tags-todo "work-{@+}-coding/TODO")
	  (tags-todo "work/-TODO")
	  (tags-todo "other_organisations")
	  (agenda "")
	  ))
  ("q" "All standard contexts"
   (
    (tags-todo "@email")
    (tags-todo "@computer")
    (tags-todo "coding")
    (tags-todo "@desk")
    (tags-todo "@reading")
    (tags-todo "@meeting_phone")
    (tags-todo "@shopping")
    (tags-todo "@maintenance")
    (tags-todo "-{@+}")
    (agenda "")
    ))
	))



;;;;;;;;;;
;; MobileOrg
(require 'org-mobile)
(setq org-mobile-directory (concat org-directory "/MobileOrg"))
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (concat org-directory "/mobile-new.org"))

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


;; Almost END
(provide 'setup-org)
