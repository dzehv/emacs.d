;; Install gnutls package for your OS
;; For OS X: brew install gnutls

(setq user-mail-address "<EMAIL_ADDRESS>"
      user-full-name "<FULL_NAME>")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq send-mail-function    'smtpmail-send-it
          smtpmail-smtp-server  "smtp.gmail.com"
          smtpmail-stream-type  'ssl
          smtpmail-smtp-service 465)

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
