;;; bnf-mode.el --- Major mode for editing BNF grammars. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; Maintainer: Serghei Iakovlev
;; Version: 0.4.1
;; URL: https://github.com/sergeyklay/bnf-mode
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (emacs "24.3"))

;; This file is NOT part of GNU Emacs.

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
;; provides basic syntax and font-locking for BNF files.  BNF notation is
;; supported exactly form as it was first announced in the ALGOL 60 report.
;;
;; When developing this mode, the following documents were taken into account:
;;
;; - RFC822: Standard for ARPA Internet Text Messages
;;   (see URL `https://www.ietf.org/rfc/rfc822.txt')
;; - RFC5234: Augmented BNF for Syntax Specifications: ABNF
;;   (see URL `https://www.ietf.org/rfc/rfc5234.txt')
;; - FRC7405: Case-Sensitive String Support in ABNF
;;   (see URL `https://www.ietf.org/rfc/rfc7405.txt')
;; - Revised Report on the Algorithmic Language Algol 60
;;   (see URL `https://www.masswerk.at/algol60/report.htm')
;;
;; Usage:  Put this file in your Emacs Lisp path (eg. site-lisp) and add to
;; your .emacs file:
;;
;;   (require 'bnf-mode)
;;
;; Bugs: Bug tracking is currently handled using the GitHub issue tracker
;; (see URL `https://github.com/sergeyklay/bnf-mode/issues')
;;
;; History: History is tracked in the Git repository rather than in this file.
;; See URL `https://github.com/sergeyklay/bnf-mode/blob/master/CHANGELOG.org'

;;; Code:


;;; Requirements

(eval-when-compile
  (require 'rx))    ; `rx'

(require 'cl-lib)   ; `cl-defmacro'


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

(defcustom bnf-mode-algol-commets-style nil
  "Non-nil means use for BNF comments style introduced in ALGOL 60.

For the purpose of including text among the symbols of a program the following
\"comment\" conventions will hold:

  :------------------------------------------------:------------------:
  | The sequence of basic symbols:                 | is equivalent to |
  :------------------------------------------------:------------------:
  | ; comment <any sequence not containing ;>;     | ;                |
  | begin comment <any sequence not containing ;>; | begin            |
  :------------------------------------------------:------------------:

Note: Enabling this feature disables ABNF/EBN comments style (just \";\")."
  :group 'bnf
  :type 'boolean)


;;; Specialized rx

(eval-when-compile
  (defconst bnf-rx-constituents
    `((bnf-rule-name . ,(rx (and
                             (1+ (or alnum digit))
                             (0+ (or alnum digit
                                     (in "!\"\#$%&'()*+,\-./:;=?@\[\\\]^_`{|}~")
                                     (in " \t"))))))
    "Additional special sexps for `bnf-rx'."))

  (cl-defmacro bnf-rx (&rest sexps)
     "BNF-specific replacement for `rx'.

In addition to the standard forms of `rx', the following forms
are available:

`bnf-rule-name'
      Any valid BNF rule name.  This rule was obtained by studying
      ALGOL 60 report, where the BNF was officially announced.
      Please note: This rule is not suitable for ABNF or EBNF
      (see URL `https://www.masswerk.at/algol60/report.htm').

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
    ;; LHS nonterminals may be preceded
    ;; by an unlimited number of spaces
    (,(bnf-rx (and line-start
                   (0+ space)
                   "<"
                   (group bnf-rule-name)
                   ">"
                   (0+ space)
                   "::="))
     1 font-lock-function-name-face)
    ;; Other nonterminals
    (,(bnf-rx (and (0+ space)
                   "<"
                   (group bnf-rule-name)
                   ">"
                   (0+ space)))
     1 font-lock-builtin-face)
    ;; “may expand into” symbol
    (,(bnf-rx (and symbol-start
                   (group "::=")
                   symbol-end))
     1 font-lock-constant-face)
    ;; Alternatives
    (,(bnf-rx (and (0+ space)
                   symbol-start
                   (group "|")
                   symbol-end
                   (0+ space)))
     1 font-lock-warning-face))
  "Font lock BNF keywords for BNF Mode.")


;;; Syntax

(defvar bnf-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give CR the same syntax as newline
    (modify-syntax-entry ?\^m "> b" table)

    ;; Treat ::= as sequence of symbols
    (modify-syntax-entry ?\: "_" table)
    (modify-syntax-entry ?\= "_" table)

    ;; Treat | as a symbol
    (modify-syntax-entry ?\| "_" table)

    ;; In BNF there are no strings
    ;; so treat ' and " as a symbols
    (modify-syntax-entry ?\" "_" table)
    (modify-syntax-entry ?\' "_" table)

    ;; In BNF there are no grouping
    ;; brackets except angle ones
    (modify-syntax-entry ?\( "_" table)
    (modify-syntax-entry ?\) "_" table)
    (modify-syntax-entry ?\{ "_" table)
    (modify-syntax-entry ?\} "_" table)
    (modify-syntax-entry ?\[ "_" table)
    (modify-syntax-entry ?\] "_" table)

    ;; Group angle brackets
    (modify-syntax-entry ?\< "(>" table)
    (modify-syntax-entry ?\> ")<" table)

    ;; Comments setup
    (if bnf-mode-algol-commets-style
        (modify-syntax-entry ?\; ">" table)
      (progn
        (modify-syntax-entry ?\; "<" table)
        (modify-syntax-entry ?\n ">" table)))

    table)
  "Syntax table in use in `bnf-mode' buffers.")

(defun bnf--bnf-syntax-propertize-function (start end)
  "Apply syntax table properties to special constructs in region START to END.
Currently handled:

 - Fontify terminals with ';' character correctly"
  (save-excursion
    (goto-char start)
    ;; Search for terminals like "<abc;>" or "<a;bc>".
    ;; Does not work for terminals like "<a;bc;>".
    (while (re-search-forward "\\(?:<[^>]*\\);" end t)
      (when (looking-at "\\(?:[^>]\\)*>")
        ;; Mark the ";" character as an extra character used in terminals
        ;; along with word constituents.
        (put-text-property (1- (point)) (point)
                           'syntax-table (string-to-syntax "_"))))))


;;; Initialization

(defconst bnf--bnf-syntax-propertize-macro
  (syntax-propertize-rules
   ("\\(?:begin\\s-+\\|;\\s-*\\)\\(comment\\)\\(;\\|\\s-+[^;]*;\\)" (1 "<")))
  "Fontify comments in ALGOL 60 style.
Provide a macro to apply syntax table properties to comments in ALGOL 60 style.
Will be used only if `bnf-mode-algol-commets-style' is set to t")

;;;###autoload
(define-derived-mode bnf-mode prog-mode "BNF"
  "A major mode for editing BNF grammars."
  :syntax-table bnf-mode-syntax-table
  :group 'bnf-mode

  ;; Comments setup
  (setq-local comment-use-syntax nil)
  (if bnf-mode-algol-commets-style
      (progn
        (setq-local comment-start "; comment ")
        (setq-local comment-end ";")
        (setq-local comment-start-skip "\\(?:\\(\\W\\|^\\)comment\\)\\s-+")
        (setq-local syntax-propertize-function
                    bnf--bnf-syntax-propertize-macro))
    (progn
      (setq-local comment-start "; ")
      (setq-local comment-end "")
      (setq-local comment-start-skip "\\(?:\\(\\W\\|^\\);+\\)\\s-+")
      (setq-local syntax-propertize-function
                  #'bnf--bnf-syntax-propertize-function)))

  ;; Basically `syntax-propertize-function' is a construct which belongs
  ;; to `font-lock'.  But correct indentation depends on
  ;; syntax properties of the text, and that should ideally be
  ;; independent of font-lock being activated or not.
  ;;
  ;; For `bnf-mode', this means that with `font-lock' disabled, we wont
  ;; have our syntax properties set correctly, and indentation will
  ;; suffer.
  ;;
  ;; To patch our way around this, we issue a `syntax-propertize' call
  ;; manually, `font-lock' enabled or not.
  (with-silent-modifications
    (if bnf-mode-algol-commets-style
        (funcall syntax-propertize-function (point-min) (point-max))
      (bnf--bnf-syntax-propertize-function (point-min) (point-max))))

  ;; Font locking
  (setq font-lock-defaults
        '(
          ;; Keywords
          bnf-font-lock-keywords
          ;; keywords-only
          nil
          ;; Regarding to RFC5234 rule names are case insensitive.
          ;; The names <rulename>, <Rulename>, <RULENAME>, and <rUlENamE>
          ;; all refer to the same rule.  As far as is known, this doesn't
          ;; conflict with original BNF version
          ;; (see URL `https://tools.ietf.org/html/rfc5234')
          t)))

;; Invoke bnf-mode when appropriate

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bnf\\'" . bnf-mode))

(provide 'bnf-mode)

;; Local Variables:
;; firestarter: ert-run-tests-interactively
;; End:

;;; bnf-mode.el ends here
