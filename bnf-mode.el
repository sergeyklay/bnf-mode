;;; bnf-mode.el --- Major mode for editing BNF grammars -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; Maintainer: Serghei Iakovlev
;; Version: 0.2.0
;; URL: https://github.com/sergeyklay/bnf-mode
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (pkg-info "0.4") (emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;   GNU Emacs major mode for editing BNF grammars.  Currently this mode
;; provides basic syntax and font-locking for "*.bnf" files.
;;
;; When developing this mode, the following RFCs were taken into account:
;;
;; - RFC822: Standard for ARPA Internet Text Messages [1]
;; - RFC5234: Augmented BNF for Syntax Specifications: ABNF [2]
;;
;; [1]: https://www.ietf.org/rfc/rfc822.txt
;; [2]: https://www.ietf.org/rfc/rfc5234.txt
;;
;; Usage:  Put this file in your Emacs Lisp path (eg. site-lisp) and add to
;; your .emacs file:
;;
;;   (require 'bnf-mode)
;;
;; Bugs: Bug tracking is currently handled using the GitHub issue tracker at
;; https://github.com/sergeyklay/bnf-mode/issues
;;
;; History: History is tracked in the Git repository rather than in this file.
;; See https://github.com/sergeyklay/bnf-mode/blob/master/CHANGELOG.org

;;; Code:


;;; Requirements

;; Tell the byte compiler about autoloaded functions from packages
(declare-function pkg-info-version-info "pkg-info" (package))

(eval-when-compile
  (require 'rx))    ; `rx'

(require 'cl-lib)   ; `cl-defmacro'
(require 'pkg-info) ; `pkg-info-version-info'


;;; Customization

;;;###autoload
(defgroup bnf nil
  "Major mode for editing BNF grammars."
  :tag "BNF"
  :prefix "bnf-"
  :group 'languages
  :link '(url-link :tag "GitHub Page" "https://github.com/sergeyklay/bnf-mode")
  :link '(emacs-commentary-link :tag "Commentary" "bnf-mode"))

(defcustom bnf-mode-hook nil
  "List of functions to call when entering BNF Mode."
  :tag "Hook"
  :type 'hook
  :group 'bnf)


;;; Version information

(defun bnf-mode-version (&optional show-version)
  "Display string describing the version of BNF Mode.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'bnf-mode)))
    (when show-version
      (message "BNF Mode version: %s" version))
    version))


;;; Specialized rx

(eval-when-compile
  (defconst bnf-rx-constituents
    `(
      ;; rulename
      (rulename . ,(rx (and
                        symbol-start
                        letter
                        (0+ (or "-" alnum))
                        symbol-end)))
    "Additional special sexps for `bnf-rx'."))

  (defmacro bnf-rx (&rest sexps)
     "BNF-specific replacement for `rx'.

In addition to the standard forms of `rx', the following forms
are available:

`rulename'
      Any valid rule name.  The name of a rule is simply the
      name itself, that is, a sequence of characters, beginning
      with an alphabetic character, and followed by a combination
      of alphabetics, digits, and hyphens (dashes).
      For more see RFC5234#2.1

See `rx' documentation for more information about REGEXPS param."
     (let ((rx-constituents (append bnf-rx-constituents rx-constituents)))
       (cond ((null sexps)
              (error "No regexp"))
             ((cdr sexps)
              (rx-to-string `(and ,@sexps) t))
             (t
              (rx-to-string (car sexps) t))))))


;;; Font Locking

(defvar bnf-font-lock-keywords
  `(
    ;; LHS nonterminals
    (,(bnf-rx (and line-start
                   "<"
                   (group rulename)
                   ">"))
     1 font-lock-function-name-face)
    ;; other nonterminals
    (,(bnf-rx (and "<"
                   (group rulename)
                   ">"))
     1 font-lock-builtin-face)
    ;; "may expand into" symbol
    (,(bnf-rx (and (0+ space)
                   symbol-start
                   (group "::=")
                   symbol-end
                   (0+ space)))
     1 font-lock-constant-face)
    ;; Alternatives
    (,(bnf-rx (and (0+ space)
                   symbol-start
                   (group "|")
                   symbol-end
                   (0+ space)))
     1 font-lock-warning-face))
  "Font lock keywords for BNF Mode.")


;;; Initialization

(defvar bnf-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give CR the same syntax as newline
    (modify-syntax-entry ?\^m "> b" table)
    ;; Characters used to delimit string constants
    (modify-syntax-entry ?\"  "\""  table)
    ;; Comments setup (see RFC822#2.8)
    (modify-syntax-entry ?\;  "<"   table)
    (modify-syntax-entry ?\n  ">"   table)
    ;; Treat ::= as sequence of symbols
    (modify-syntax-entry ?\:  "_"   table)
    (modify-syntax-entry ?\=  "_"   table)
    ;; Treat | as a symbol
    (modify-syntax-entry ?\|  "_"   table)
    ;; Group angle brackets
    (modify-syntax-entry ?\<  "(>"  table)
    (modify-syntax-entry ?\>  ")<"  table)
    table)
  "Syntax table in use in `bnf-mode' buffers.")

;;;###autoload
(define-derived-mode bnf-mode prog-mode "BNF"
  "A major mode for editing BNF grammars."
  :syntax-table bnf-mode-syntax-table
  :group 'bnf-mode
  ;; Comment setup (for more see RFC822#2.8)
  (setq-local comment-use-syntax nil)
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local font-lock-keyword-face 'php-keyword)
  ;; Font locking
  (setq font-lock-defaults '(
                             ;; Keywords
                             bnf-font-lock-keywords
                             ;; keywords-only
                             nil
                             ;; Regarding to RFC5234#2.1 rule names are case
                             ;; insensitive.  The names <rulename>, <Rulename>,
                             ;; <RULENAME>, and <rUlENamE> all refer to the
                             ;; same rule.
                             t
                             )))

;; Invoke bnf-mode when appropriate

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bnf\\'" . bnf-mode))

(provide 'bnf-mode)

;; Local Variables:
;; firestarter: ert-run-tests-interactively
;; End:

;;; bnf-mode.el ends here
