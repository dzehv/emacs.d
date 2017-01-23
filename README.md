# Emacs configuration and some migrate documentation

## Create source folder and copy config file
mkdir ~/.emacs.d && cp emacs ~/.emacs

## Copy emacsclient wrapper script for emacs-server usage
cp ec /usr/local/bin

OR

cp ec /bin

## vim-compared basic operations doc
vim2emacs_doc/vim2emacs_doc.html

## Gnus email settings example
cp gnus ~/.gnus

cp authinfo ~/.authinfo && chmod 0600 ~/.authinfo

Edit files with your secret info