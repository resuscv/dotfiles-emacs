;; No splash screen please ...
(setq inhibit-startup-message t)

;; Set up load path
(add-to-list 'load-path user-emacs-directory)

;; Line wrapping
(global-visual-line-mode t)

;; Added a new line at the end of files
(setq require-final-newline t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'setup-ido)
(require 'setup-org)
