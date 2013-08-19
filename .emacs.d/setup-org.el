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
(add-to-list 'load-path (expand-file-name "~/software/git/org-mode/lisp/"))
(add-to-list 'Info-default-directory-list "~/software/git/org-mode/doc")
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)


;; Standard key bindings  [1]
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'org-narrow-to-subtree)
(global-set-key (kbd "<f9> w") 'widen)
(global-set-key (kbd "<f9> u") 'bh/narrow-up-one-level)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> T") 'tabify)
(global-set-key (kbd "<f9> U") 'untabify)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c r") 'org-capture)


(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "~/tmp/scratch.org"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Change the repeat-todo-state to NEXT rather than TODO
(setq org-todo-repeat-to-state "NEXT")

;; Set up org directories, like agenda [3, 6]
(setq org-directory "~/Documents/org")
(setq org-agenda-files (quote ("~/Documents/org")))
(setq org-agenda-diary-file (concat org-directory "/diary.org"))
;; Capture setup
(setq org-default-notes-file (concat org-directory "/refile.org"))
(define-key global-map "\C-cc" 'org-capture)


;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
               "* TODO %?\n%U\n%a\n")
              ("r" "respond" entry (file org-default-notes-file)
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n")
              ("n" "note" entry (file org-default-notes-file)
               "* %? :NOTE:\n%U\n%a\n")
              ("j" "Journal - Personal" entry (file+datetree org-agenda-diary-file)
               "* %?\n")
              ("J" "Journal - Work" entry (file+datetree (concat org-directory "/diary-work.org"))
               "* %?\n")
              ("w" "org-protocol" entry (file org-default-notes-file)
               "* TODO Review %c\n%U\n")
              ("p" "Phone call" entry (file org-default-notes-file)
               "* PHONE %? :PHONE:\n%U")
              ("h" "Habit" entry (file org-default-notes-file)
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))


; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)


(setq org-agenda-todo-keyword-format "%-6s")
;(setq org-agenda-prefix-format "  %-18c%?-16t% s %i")
(setq org-agenda-prefix-format "  %12c%?-16t% s %i")

;; From my StackOverflow question:   http://stackoverflow.com/questions/17182954/write-and-call-function-from-agenda-org-mode
(defun zin/agenda-test (tag &optional signp)
  "Simplify agenda coding, only require TAG to create new block.

SIGNP determines whether to use `+' or `-' when adding the tag.
Defaulting to `-'."
  (let ((sign (if signp "+" "-")))
    `(tags-todo ,(format "-WAITING-CANCELLED%s%s/!NEXT" sign tag)
        ((org-agenda-overriding-header ,(format "Next Tasks: %s%s" sign tag))
         (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
         (org-agenda-todo-ignore-scheduled 'future)
         (org-agenda-todo-ignore-deadlines 'future)
         (org-tags-match-list-sublevels t)
         (org-agenda-sorting-strategy
          '(todo-state-down effort-up category-keep))))))

;; Custom agenda command definitions
(setq org-agenda-custom-commands
;      (quote 
      `
      (("N" "Notes" tags "NOTE"
	((org-agenda-overriding-header "Notes")
	 (org-tags-match-list-sublevels t)))
       ("h" "Habits" tags-todo "STYLE=\"habit\""
	((org-agenda-overriding-header "Habits")
	 (org-agenda-sorting-strategy
	  '(todo-state-down effort-up category-keep))))
       (" " "Agenda"
	(
	 (tags "REFILE"
	       ((org-agenda-overriding-header "Tasks to Refile")
		(org-tags-match-list-sublevels nil)))
	 (tags-todo "-CANCELLED/!"
		    ((org-agenda-overriding-header "Stuck Projects")
		     (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
	 ;; (tags-todo "-WAITING-CANCELLED/!NEXT"
	 ;;            ((org-agenda-overriding-header "Next Tasks")
	 ;;             (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
	 ;;             (org-agenda-todo-ignore-scheduled 'future)
	 ;;             (org-agenda-todo-ignore-deadlines 'future)
	 ;;             (org-tags-match-list-sublevels t)
	 ;;             (org-agenda-sorting-strategy
	 ;;              '(todo-state-down effort-up category-keep))))
	 (tags-todo "-REFILE-CANCELLED+@today/!-HOLD-WAITING"
		    ((org-agenda-overriding-header "Things to do TODAY")
		     (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
		     (org-agenda-todo-ignore-scheduled 'future)
		     (org-agenda-todo-ignore-deadlines 'future)
		     (org-agenda-sorting-strategy
		      '(category-keep))))
	 , (zin/agenda-test "@computer" '+)
	 , (zin/agenda-test "@email" '+)
	 , (zin/agenda-test "@desk" '+)
	 , (zin/agenda-test "@reading" '+)
	 , (zin/agenda-test "@meeting_phone" '+)
	 (tags-todo "-WAITING-CANCELLED-@today-@computer-@email-@desk-@reading-@meeting_phone/!NEXT"
		    ((org-agenda-overriding-header "Next Tasks: Rest")
		     (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
		     (org-agenda-todo-ignore-scheduled 'future)
		     (org-agenda-todo-ignore-deadlines 'future)
		     (org-tags-match-list-sublevels t)
		     (org-agenda-sorting-strategy
		      '(todo-state-down effort-up category-keep))))
	 (tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
		    ((org-agenda-overriding-header "Tasks")
		     (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
		     (org-agenda-todo-ignore-scheduled 'future)
		     (org-agenda-todo-ignore-deadlines 'future)
		     (org-agenda-sorting-strategy
		      '(category-keep))))
	 (tags-todo "-HOLD-CANCELLED/!"
		    ((org-agenda-overriding-header "Projects")
		     (org-agenda-skip-function 'bh/skip-non-projects)
		     (org-agenda-sorting-strategy
		      '(category-keep))))
	 (tags-todo "-CANCELLED+WAITING/!"
		    ((org-agenda-overriding-header "Waiting and Postponed Tasks")
		     (org-agenda-skip-function 'bh/skip-stuck-projects)
		     (org-tags-match-list-sublevels nil)
		     (org-agenda-todo-ignore-scheduled 'future)
		     (org-agenda-todo-ignore-deadlines 'future)))
	 (tags "-REFILE/"
	       ((org-agenda-overriding-header "Tasks to Archive")
		(org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		(org-tags-match-list-sublevels nil)))
	 ;; (agenda "" nil)
	 )
	nil)
       ("r" "Tasks to Refile" tags "REFILE"
	((org-agenda-overriding-header "Tasks to Refile")
	 (org-tags-match-list-sublevels nil)))
       ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
	((org-agenda-overriding-header "Stuck Projects")
	 (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
       ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
	((org-agenda-overriding-header "Next Tasks")
	 (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
	 (org-agenda-todo-ignore-scheduled 'future)
	 (org-agenda-todo-ignore-deadlines 'future)
	 (org-tags-match-list-sublevels t)
	 (org-agenda-sorting-strategy
	  '(todo-state-down effort-up category-keep))))
       ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
	((org-agenda-overriding-header "Tasks")
	 (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
	 (org-agenda-sorting-strategy
	  '(category-keep))))
       ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
	((org-agenda-overriding-header "Projects")
	 (org-agenda-skip-function 'bh/skip-non-projects)
	 (org-agenda-sorting-strategy
	  '(category-keep))))
       ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
	((org-agenda-overriding-header "Waiting and Postponed tasks"))
	(org-tags-match-list-sublevels nil))
       ("A" "Tasks to Archive" tags "-REFILE/"
	((org-agenda-overriding-header "Tasks to Archive")
	 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
	 (org-tags-match-list-sublevels nil)
	 )
	)
       )
;       )
      )



(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)


(setq org-agenda-span 'day)

(setq org-stuck-projects (quote ("" nil nil "")))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))



(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((and (bh/is-project-p)
                 (marker-buffer org-agenda-restrict-begin))
            nil)
           ((and (bh/is-project-p)
                 (not (marker-buffer org-agenda-restrict-begin))
                 (not (bh/is-project-subtree-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; Consider only tasks with done todo headings as archivable candidates
      (if (member (org-get-todo-state) org-done-keywords)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (daynr (string-to-int (format-time-string "%d" (current-time))))
                 (a-month-ago (* 60 60 24 (+ daynr 1)))
                 (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                 (this-month (format-time-string "%Y-%m-" (current-time)))
                 (subtree-is-current (save-excursion
                                       (forward-line 1)
                                       (and (< (point) subtree-end)
                                            (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
            (if subtree-is-current
                next-headline ; Has a date in this month or last month, skip it
              nil))  ; available to archive
        (or next-headline (point-max))))))


; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))


; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-attach
			  org-bibtex
                          org-crypt
			  org-id
                          org-info
                          org-jsinfo
                          org-habit
                          org-inlinetask
                          org-irc
			  org-mhe
                          org-protocol
                          org-rmail
                          org-vm
                          org-wl
                          org-w3m)))

; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

;; Attachments
(require 'org-attach)


;; Various preferences [2]
(setq org-log-done t
;      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
;      org-agenda-span 8                                 ;; Show one day ahead next week
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
 ;; ;     org-fast-tag-selection-single-key 'expert        ;;  I don't know what these mean
 ;; ;     org-export-kill-product-buffer-when-displayed t  ;;  I don't know what these mean
      org-tags-column 80
      org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil
      )



;(setq org-todo-keywords
;      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
;	      (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))

(setq org-tag-alist '((:startgroup . nil)
		      ("work" . ?w)
		      ("home" . ?h)
		      ("other_organisations" . ?o)
		      ("@today" . ?t)
		      (:endgroup . nil)
		      ("@computer" . ?c)
		      ("@email" . ?e)
		      ("@desk" . ?d)
                      (:newline . nil)
		      ("@reading" . ?r)
		      ("@meeting_phone" . ?m)
                      ("coding" . ?1)
                      (:newline . nil)
		      ("@shopping" . ?s)
		      ("@maintenance" . nil)
                      (:newline . nil)
		      ))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)


; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

(global-set-key (kbd "<f5>") 'bh/org-todo)

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(global-set-key (kbd "<S-f5>") 'bh/widen)

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-agenda-remove-restriction-lock)
    (widen)
    (org-agenda-remove-restriction-lock)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" 'bh/widen))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (if (equal major-mode 'org-agenda-mode)
        (bh/set-agenda-restriction-lock 4)
      (widen))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (bh/narrow-to-org-subtree)
        (save-restriction
          (org-agenda-set-restriction-lock)))
    (bh/narrow-to-org-subtree)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
        (bh/narrow-up-one-org-level))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
        (bh/narrow-to-org-project)
        (save-restriction
          (org-agenda-set-restriction-lock)))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/current-view-project nil)

(defun bh/view-next-project ()
  (interactive)
  (unless (marker-position org-agenda-restrict-begin)
    (goto-char (point-min))
    (setq bh/current-view-project (point)))
  (bh/widen)
  (goto-char bh/current-view-project)
  (forward-visible-line 1)
  (while (and (< (point) (point-max))
              (or (not (org-get-at-bol 'org-hd-marker))
                  (org-with-point-at (org-get-at-bol 'org-hd-marker)
                    (or (not (bh/is-project-p))
                        (bh/is-project-subtree-p)))))
    (forward-visible-line 1))
  (setq bh/current-view-project (point))
  (if (org-get-at-bol 'org-hd-marker)
      (progn
        (bh/narrow-to-project)
        (org-agenda-redo)
        (beginning-of-buffer))
    (beginning-of-buffer)
    (error "All projects viewed.")))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

(setq org-show-entry-below (quote ((default))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type)))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)









;; Update cookies [4]
(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

(setq org-agenda-include-diary nil)

(setq org-agenda-insert-diary-extract-time t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
              (todo category-up priority-down effort-up)
              (tags category-up priority-down effort-up)
              (search category-up))))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))

;; Display tags farther right
;(setq org-agenda-tags-column -102)
(setq org-agenda-tags-column -130)

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

     ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

     ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

     ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\.:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (define-key org-agenda-mode-map "q" 'bury-buffer))
          'append)


(setq org-cycle-separator-lines 0)

(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)
(setq org-deadline-warning-days 30)

; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(global-auto-revert-mode t)

;; ;; ;; (require 'org-crypt)
;; ;; ;; ; Encrypt all entries before saving
;; ;; ;; (org-crypt-use-before-save-magic)
;; ;; ;; (setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; ;; ;; ; GPG key to use for encryption
;; ;; ;; (setq org-crypt-key "F0B66B40")

;; ;; ;; (setq org-crypt-disable-auto-save nil)


(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . ignore)
                                      ("1" . ignore)
                                      ("2" . ignore)
                                      ("3" . ignore)
                                      ("4" . ignore)
                                      ("5" . ignore)
                                      ("6" . ignore)
                                      ("7" . ignore)
                                      ("8" . ignore)
                                      ("9" . ignore)

                                      ("a" . ignore)
                                      ("d" . ignore)
                                      ("h" . bh/hide-other)
                                      ("i" progn
                                       (forward-char 1)
                                       (call-interactively 'org-insert-heading-respect-content))
                                      ("k" . org-kill-note-or-show-branches)
                                      ("l" . ignore)
                                      ("m" . ignore)
                                      ("q" . bh/show-org-agenda)
                                      ("r" . ignore)
                                      ("s" . org-save-all-org-buffers)
                                      ("w" . org-refile)
                                      ("x" . ignore)
                                      ("y" . ignore)
                                      ("z" . org-add-note)

                                      ("A" . ignore)
                                      ("B" . ignore)
                                      ("E" . ignore)
                                      ("F" . bh/restrict-to-file-or-follow)
                                      ("G" . ignore)
                                      ("H" . ignore)
                                      ;; ("J" . org-clock-goto)
                                      ("J" . ignore)
                                      ("K" . ignore)
                                      ("L" . ignore)
                                      ("M" . ignore)
                                      ("N" . bh/narrow-to-org-subtree)
                                      ("P" . bh/narrow-to-org-project)
                                      ("Q" . ignore)
                                      ("R" . ignore)
                                      ("S" . ignore)
                                      ("T" . bh/org-todo)
                                      ("U" . bh/narrow-up-one-org-level)
                                      ("V" . ignore)
                                      ("W" . bh/widen)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))

(defun bh/show-org-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*")
  (delete-other-windows))



(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

(setq org-remove-highlights-with-change nil)


(setq org-read-date-prefer-future 'time)

(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-"))))

(setq org-tags-match-list-sublevels t)

(setq org-agenda-persistent-filter t)

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

(setq org-startup-folded t)

(setq org-alphabetical-lists t)


;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Disable C-c [ and C-c ] and C-c ; in org-mode
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c["    'undefined)
             (org-defkey org-mode-map "\C-c]"    'undefined)
             (org-defkey org-mode-map "\C-c;"    'undefined))
          'append)


(setq org-catch-invisible-edits 'error)



;; xxx HERE


;; Enforce TODO behaviour
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
;(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-include-all-todo nil)

;;;; Agenda setup
;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)
;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)
;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)
;??;(setq org-agenda-todo-ignore-scheduled t)
;??;(setq org-agenda-todo-ignore-scheduled 'future)
;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)
;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)
;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)


;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Hide those stars
(setq org-startup-indented t)

;;;;(setq org-startup-folded 'overview)





;; ;; Custom agenda - weekly agenda and global TODO
;; (setq org-agenda-custom-commands
;;       '(("h" "Agenda and Home-related tasks"
;; 	 (
;; 	  (tags-todo "@email+home/TODO")
;; 	  (tags-todo "@computer+home/TODO")
;; 	  (tags-todo "@desk+home/TODO")
;; 	  (tags-todo "@reading+home/TODO")
;; 	  (tags-todo "@meeting_phone+home/TODO")
;; 	  (tags-todo "@shopping+home/TODO")
;; 	  (tags-todo "@maintenance+home/TODO")
;; 	  (tags-todo "-{@+}+home/TODO")
;; 	  (tags-todo "home/-TODO")
;; ;	  (tags "garden")
;; 	  (tags-todo "other_organisations")
;; 	  (agenda "")
;; 	  ))
;; 	("w" "Agenda and Office-related tasks"
;; 	 (
;; ;	  (tags-todo "@email+work/TODO" ((org-agenda-overriding-header "Context: @email")))
;; 	  (tags-todo "@email+work/TODO")
;; 	  (tags-todo "@computer+work/TODO")
;; 	  (tags-todo "coding+work/TODO")
;; 	  (tags-todo "@desk+work/TODO")
;; 	  (tags-todo "@reading+work/TODO")
;; 	  (tags-todo "@meeting_phone+work/TODO")
;; 	  (tags-todo "work-{@+}-coding/TODO")
;; 	  (tags-todo "work/-TODO")
;; 	  (tags-todo "other_organisations")
;; 	  (agenda "")
;; 	  ))
;;   ("q" "All standard contexts"
;;    (
;;     (tags-todo "@email")
;;     (tags-todo "@computer")
;;     (tags-todo "coding")
;;     (tags-todo "@desk")
;;     (tags-todo "@reading")
;;     (tags-todo "@meeting_phone")
;;     (tags-todo "@shopping")
;;     (tags-todo "@maintenance")
;;     (tags-todo "-{@+}")
;;     (agenda "")
;;     ))
;; 	))


;; Configure Babel to support all languages included in the manuscript
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot        . t)
   (emacs-lisp . t)
;   (haskell    . t)
   (org        . t)
   (perl       . t)
   (python     . t)
   (R          . t)
;   (ruby       . t)
   (sh         . t)
   (sqlite     . t)))
(setq org-confirm-babel-evaluate nil)


;; Set default header arguments for the Org-mode blocks used to
;; showcase example Org-mode syntax.
(setq org-babel-default-header-args:org '((:results . "raw silent")
                                          (:exports . "code")))



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
