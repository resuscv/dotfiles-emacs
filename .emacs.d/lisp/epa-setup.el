;; EasyPG setup
;;
;; http://orgmode.org/worg/org-tutorials/encrypting-files.html
;; http://orgmode.org/manual/org_002dcrypt_002eel.html
;; https://emacs.stackexchange.com/questions/7230/how-to-automatically-encrypt-orgmode-files
;;
;; Add the following to the head of the XYZ.org.gpg file
;;     # -*- mode:org; epa-file-encrypt-to: ("me@mydomain.com"); buffer-auto-save-file-name: nil; -*-

(require 'epa-file)
(epa-file-enable)

(setq epg-gpg-program "gpg2")

;; Almost END
(provide 'epa-setup)
