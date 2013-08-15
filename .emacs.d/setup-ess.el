;; Use the git version of ESS
(add-to-list 'load-path (expand-file-name "~/software/git/ESS/lisp/"))
(add-to-list 'Info-default-directory-list "~/software/git/ESS/doc")
(require 'ess-site nil 'noerror)


;; Almost END
(provide 'setup-ess)
