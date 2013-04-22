;; No splash screen please ...
(setq inhibit-startup-message t)

;; Set up load path
(add-to-list 'load-path user-emacs-directory)

;; Line wrapping
(global-visual-line-mode t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'setup-org)
