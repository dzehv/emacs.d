# GNU Emacs, gnus, emacsclient configuration and some migration notes

### Create source folder and copy config file
```
mkdir ~/.emacs.d && cp emacs ~/.emacs
```

### Copy elisp libs dir
```
cp -r lisp/ ~/.emacs.d/
```

### Copy fonts directory
```
cp -r fonts ~/.fonts
```

### Copy emacsclient wrapper script for local emacs-server usage
```
cp emacsclient/emacsclient_local /usr/local/bin/ec
```

### Copy emacsclient wrapper script for remote emacs-server usage by SSH (edit script with ssh credentials)
```
cp emacsclient/emacsclient_remote /usr/local/bin/ec
```

### Gnus email settings example
```
cp gnus_mail/gnus ~/.gnus
cp gnus_mail/authinfo ~/.authinfo && chmod 0600 ~/.authinfo
```

Edit ~/.gnus and ~/.authinfo with your secret info.

### Set default editor to no-window emacs

#### WARNING: Use of no-window emacs needs to use lossless kbd input extensions for specified terminal, unexpected reactions otherwise
```
echo export EDITOR=\"emacs -nw\" >> ~/.bashrc
```

### vim-compared basic operations doc, also described some pretty features
```
ec vim2emacs/vim2emacs.html
M-x shr-render-buffer
```

### To compile .tex files, install
```
texlive-full
```
