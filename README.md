# GNU Emacs, gnus, emacsclient configuration and documentation

## Create source folder and copy config file
mkdir ~/.emacs.d && cp emacs ~/.emacs

## Copy elisp libs directory
cp -r lisp/ ~/.emacs.d/

## Copy fonts directory
mkdir ~/.fonts && cp -r fonts/ ~/.fonts/

## Copy emacsclient wrapper script for local emacs-server usage
cp emacsclient/emacsclient_local /usr/local/bin/ec

## Copy emacsclient wrapper script for remote emacs-server usage by SSH (edit script with ssh credentials)
cp emacsclient/emacsclient_remote /usr/local/bin/ec

## Gnus email settings example
cp gnus_mail/gnus ~/.gnus

cp gnus_mail/authinfo ~/.authinfo && chmod 0600 ~/.authinfo

Edit ~/.gnus and ~/.authinfo with your secret info.

## vim-compared basic operations doc
vim2emacs/vim2emacs.html
