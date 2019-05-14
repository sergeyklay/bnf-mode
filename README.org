* BNF Mode for GNU Emacs

[[https://www.gnu.org/licenses/gpl-3.0.txt][https://img.shields.io/badge/license-GPL_3-green.svg]]
[[https://travis-ci.com/sergeyklay/bnf-mode][https://travis-ci.com/sergeyklay/bnf-mode.svg]]
[[https://melpa.org/#/bnf-mode][https://melpa.org/packages/bnf-mode-badge.svg]]
[[https://stable.melpa.org/#/bnf-mode][https://stable.melpa.org/packages/bnf-mode-badge.svg]]

A GNU Emacs major mode for editing BNF grammars.

#+begin_quote
“Precise language is not the problem.  Clear language is the problem.”

Richard Feynman
#+end_quote

Currently provides basic syntax and font-locking for BNF files. BNF notation is
supported exactly form as it was first announced in the ALGOL 60 report.
EBNF and ABNF are not supported but  in my plans for the near future.

When developing this mode, the following documents were taken into account:

- [[https://www.masswerk.at/algol60/report.htm][Revised Report on the Algorithmic Language Algol 60]]
- [[https://tools.ietf.org/html/rfc822][RFC822]]: Standard for ARPA Internet Text Messages
- [[https://tools.ietf.org/html/rfc5234][RFC5234]]: Augmented BNF for Syntax Specifications: ABNF
- [[https://tools.ietf.org/html/rfc7405][RFC7405]]: Case-Sensitive String Support in ABNF

** Features

- Basic syntax definition
- Syntax highlighting

** Installation

Known to work with GNU Emacs 24.3 and later.  BNF Mode may work with
older versions of Emacs, or with other flavors of Emacs (e.g. XEmacs)
but this is /not/ guaranteed.  Bug reports for problems related to using
BNF Mode with older versions of Emacs will most like not be addressed.

The master of all the material is the Git repository at
https://github.com/sergeyklay/bnf-mode .

NOTE: The ~master~ branch will always contain the latest unstable version.
If you wish to check older versions or formal, tagged release, please switch
to the relevant [[https://github.com/sergeyklay/bnf-mode/tags][tag]].

The best way of installing this major mode, at least for GNU Emacs 24, is to
use the packaging system.  The following are ways to install using ELPA and
MELPA.

*** Using ELPA or MELPA
**** ELPA

Since version 0.4.1 BNF Mode is available for installation from ELPA.
Add ELPA to the list of repositories to access this mode:

#+begin_src emacs-lisp
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)
#+end_src

**** MELPA

Add MELPA or MELPA Stable to the list of repositories to access this mode.
MELPA tracks this Git repository and updates relatively soon after each commit
or formal release.  For more detail on setting up see [[https://melpa.org/#/getting-started][MELPA Getting Started]].

For those who want only formal, tagged releases use MELPA Stable:

#+begin_src emacs-lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
#+end_src

For those who want rolling releases as they happen use MELPA:

#+begin_src emacs-lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
#+end_src

After initializing packaging system you can install BNF Mode using preferred way:

***** =package-list-packages=

Use ~M-x package-refresh-contents~ and ~M-x package-list-packages~ to get to
the package listing and install ~bnf-mode~ from there.

***** Manual

You can install ~bnf-mode~ manually by adding following to your init file:

#+begin_src emacs-lisp
(unless (package-installed-p 'bnf-mode)
    (package-refresh-contents)
    (package-install 'bnf-mode))
#+end_src

***** Cask

Add following to your [[https://cask.github.io/][Cask]] file:

#+begin_src emacs-lisp
(source melpa)

(depends-on "bnf-mode")
#+end_src

***** =use-package=

Add following to your init file:

#+begin_src emacs-lisp
(use-package bnf-mode
  :ensure t
  ;; To use MELPA Stable use ":pin mepla-stable",
  ;; to use ELPA remove ":pin" line
  :pin melpa
  :mode "\\.bnf\\'")
#+end_src

*** El-get

If you use el-get, just create a recipe file ~bnf.rcp~:

#+begin_src emacs-lisp
(:name bnf-mode
 :website "https://github.com/sergeyklay/bnf-mode"
 :description "BNF Mode: A major mode for editing BNF grammars"
 :type github
 :pkgname "sergeyklay/bnf-mode")
#+end_src

and add it to a directory present in ~el-get-recipe-path~.
Then, use ~M-x el-get-install <RET> bnf-mode~ or add:

#+begin_src emacs-lisp
(el-get-bundle bnf-mode)
#+end_src

to your init file.

*** Manual Install

1. Download ~bnf-mode.el~
2. Put the file in your Elisp common folder like ~$HOME/.emacs.d/lisp/~
3. Then you can include like this:
   #+begin_src emacs-lisp
   (add-to-list 'load-path
                (expand-file-name "lisp" user-emacs-directory))
   #+end_src
4. Add /either/ of the two following lines to your initialization file.
   The first only loads BNF Mode when necessary, the 2nd always during startup
   of GNU Emacs.
   #+begin_src emacs-lisp
   (autoload 'bnf-mode "bnf-mode" nil t)
   ;; OR
   (require 'bnf-mode)
   #+end_src
5. Optionally byte compile ~bnf-mode.el~ for faster startup: ~M-x byte-compile~

** Usage

*** Interactive Commands

| Command (For the ~M-x~ prompt.) | Description                      |
|---------------------------------+----------------------------------|
| ~bnf-mode~                      | Switches to BNF Mode.            |

Any file that matches the glob ~*.bnf~ is automatically opened in ~bnf-mode~.

** Support

Feel free to ask question or make suggestions in our [[https://github.com/sergeyklay/bnf-mode/issues][issue tracker]] .

** Changes

To see what has changed in recent versions of BNF Mode, see the [[https://github.com/sergeyklay/bnf-mode/blob/master/CHANGELOG.org][CHANGELOG.org]] .

** External Links

- [[https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form][Wikipedia: Backus–Naur form]]
- [[https://en.wikipedia.org/wiki/Extended_Backus%25E2%2580%2593Naur_form][Wikipedia: Extended Backus–Naur form]]
- [[https://en.wikipedia.org/wiki/Augmented_Backus%25E2%2580%2593Naur_form][Wikipedia: Augmented Backus–Naur form]]
- [[https://www.cl.cam.ac.uk/~mgk25/iso-14977.pdf][ISO/IEC 14977: EBNF]]
- [[https://www.ics.uci.edu/~pattis/ICS-33/lectures/ebnf.pdf][EBNF: A Notation to Describe Syntax]]

** License

BNF Mode is open source software licensed under the [[https://github.com/sergeyklay/bnf-mode/blob/master/LICENSE][GNU General Public Licence version 3]].
Copyright © 2019, Free Software Foundation, Inc.
