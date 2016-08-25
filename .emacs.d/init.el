;; No splash screen please ...
(setq inhibit-startup-message t)

;; Set up load path
(add-to-list 'load-path
	     (concat user-emacs-directory
		     (convert-standard-filename "lisp/")))
(add-to-list 'load-path
	     (concat user-emacs-directory
		     (convert-standard-filename "lisp/notmuch-emacs")))

;; Line wrapping
(global-visual-line-mode t)

;; Added a new line at the end of files
(setq require-final-newline t)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;; As emacsclient doesn't get the terminal background colour, it doesn't correctly set org-face
;; so org-startup-indented doesn't work properly.  So go old school and just start the server.
;(server-start)

;; http://melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'misc-functions)
(require 'epa-setup)
(require 'ess-setup)
(require 'ido-setup)
(require 'org-setup)
(require 'notmuch)
;!!;;; http://www.skamphausen.de/cgi-bin/ska/taskjuggler-mode
;!!;(require 'taskjuggler-mode nil 'noerror)
;; Machine specific setups - if they don't exist, don't error
(require 'osx-setup nil 'noerror)
;(require 'markdown-mode-setup nil 'noerror)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
