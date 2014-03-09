;; No splash screen please ...
(setq inhibit-startup-message t)

;; Set up load path
(add-to-list 'load-path user-emacs-directory)

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

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'ido-setup)
(require 'org-setup)
(require 'ess-setup)
