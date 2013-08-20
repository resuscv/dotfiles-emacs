;; Use the git version of ESS
(let ((ess-path "~/software/git/ESS/lisp/"))
  (add-to-list 'load-path ess-path)
  (add-to-list 'Info-default-directory-list "~/software/git/ESS/doc")
  (setq ess-ask-for-ess-directory nil)
  (require 'ess-site nil 'noerror))


;; Almost END
(provide 'setup-ess)
