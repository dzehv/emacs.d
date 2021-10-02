# GNU Emacs, gnus, emacsclient configuration and some migration notes

### Create source folder and copy config file

``` bash
mkdir ~/.emacs.d && cp emacs ~/.emacs
```

### Copy elisp libs dir

``` bash
cp -r lisp/ ~/.emacs.d/
```

### Copy fonts directory

``` bash
cp -r fonts ~/.fonts
```

### Copy custom themes folder

``` bash
cp -r themes ~/.emacs.d/
```

### Copy emacsclient wrapper script for local emacs-server usage

``` bash
cp emacsclient/emacsclient_local.sh /usr/local/bin/ec
```
or

``` bash
cp emacsclient/emacsclient_local.pl /usr/local/bin/ec
```

Then we can set emacsclient wrapper as VISUAL (optional):

``` bash
export VISUAL=ec
```

### Copy emacsclient wrapper script for remote emacs-server usage by SSH (edit script with ssh credentials)
```
cp emacsclient/emacsclient_remote.sh /usr/local/bin/ec
```

### Gnus email settings example

``` bash
cp gnus_mail/gnus ~/.gnus
cp gnus_mail/authinfo ~/.authinfo && chmod 0600 ~/.authinfo
```

Edit ~/.gnus and ~/.authinfo with your secret info.

``` bash
sed -i 's/<FULL_NAME>/My Pretty Name/g; s/<EMAIL_ADDRESS>/my.email@gmail.com/g' ~/.gnus
sed -i 's/<USER>/my_email_login/g; s/<PASSWORD>/my_email_password/g' ~/.authinfo
```

### Set default editor to no-window emacs

#### WARNING: Use of no-window emacs needs to use lossless kbd input extensions for specified terminal, unexpected reactions otherwise

```bash
echo export EDITOR=\"emacs -nw\" >> [~/.[bash|zsh]rc | ~/.profile]
```

### Minimal configuration for no-window usage
``` bash
cp emacs-nw ~/.emacs-nw
```

### Install to update-alternatives for editor

Create a script that starts emacs with -nw flag, e.g. /usr/local/bin/emacs-nw
``` bash
#!/bin/sh

emacs -nw "$@"
```

Install it with update-alternatives --install
``` bash
sudo update-alternatives --install /usr/bin/editor editor /usr/local/bin/emacs-nw 2
```

Configure editor to be your new script
``` bash
sudo update-alternatives --set editor /usr/local/bin/emacs-nw
```

Or select another time
``` bash
sudo update-alternatives --config editor
```

### Launch with min conf
``` bash
emacs -nw -l ~/.emacs-nw
```

It can be used both configurations. emacs-nw will not use whole configuration if ~/.emacs-nw exists

### Python mode additional settings

``` bash
pip3 install -U jedi virtualenv
```

``` bash
M-x package-install <RET> jedi <RET>
M-x jedi:install-server
```

### vim-compared basic operations doc, also described some pretty features

```
ec vim2emacs/vim2emacs.html
M-: (require 'shr)
M-x shr-render-buffer
```

### Emacs Lisp documentaion and examples

See elispdoc/emacs-lisp-doc.el

### To compile .tex files, install

Basic:
```
texlive
```

With extras:
```
texlive-full
```

### EMMS: The Emacs Multimedia System

``` bash
apt install emms vorbis-tools
```

### Daemon mode

See daemon/README.txt instructions to install emacs as daemon.

Available commands to manage daemon with emacsclient are listed in vim2emacs.html doc.
