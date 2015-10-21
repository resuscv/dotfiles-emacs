;; Use the git version of ESS
(let ((ess-path "~/software/git/ESS/lisp/"))
  (add-to-list 'load-path ess-path)
  (add-to-list 'Info-default-directory-list "~/software/git/ESS/doc")
  (setq ess-ask-for-ess-directory nil)
  (require 'ess-site nil 'noerror))

(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map [up]
       'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [down]
       'comint-next-matching-input-from-input)
     ;; also recommended for ESS use --
     (setq comint-scroll-to-bottom-on-output 'others)
     (setq comint-scroll-show-maximum-output t)
     ;; somewhat extreme, almost disabling writing in *R*, *shell* buffers above prompt:
     (setq comint-scroll-to-bottom-on-input 'this)
     ))

;; Almost END
(provide 'ess-setup)
