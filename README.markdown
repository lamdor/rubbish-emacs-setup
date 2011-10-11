rubbish's magical world of emacs
==========

Has been verified to work on Emacs 23.3.1 You can download and install from [build from GNU source using the usual ./configure; make; sudo make install](http://ftp.gnu.org/pub/gnu/emacs/emacs-23.3a.tar.gz) or [Emacs for OSX](http://emacsforosx.com/)

# Installation #

* `git clone git://github.com/rubbish/rubbish-emacs-setup.git $HOME/.emacs.d`
* `git submodule init` in `$HOME/.emacs.d`
* `git submodule update` in `$HOME/.emacs.d`
* init/update a submodule's submodule in `site-lisp/rinari`
  * `git submodule init` in `$HOME/.emacs.d/site-lisp/rinari`
  * `git submodule update` in `$HOME/.emacs.d/site-lisp/rinari`
* if you want to use ensime in scala, do a `sbt update dist` in `$HOME/.emacs.d/site-lisp/ensime`
