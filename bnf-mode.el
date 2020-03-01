;;; bnf-mode.el --- Major mode for editing BNF grammars. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.4.4
;; URL: https://github.com/sergeyklay/bnf-mode
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (emacs "24.3"))

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;   BNF Mode is a GNU Emacs major mode for editing BNF grammars.  Currently
;; this mode provides basic syntax and font-locking for BNF files.  BNF notation
;; is supported exactly form as it was first announced in the ALGOL 60 report.

;;; Code:


;;; Requirements

(eval-when-compile
  (require 'rx))    ; `rx'


;;; Customization

;;;###autoload
(defgroup bnf nil
  "Major mode for editing BNF grammars."
  :tag "BNF"
  :prefix "bnf-"
  :group 'languages
  :link '(url-link :tag "GitHub Page" "https://github.com/sergeyklay/bnf-mode")
  :link '(emacs-commentary-link :tag "Commentary" "bnf-mode"))

(defcustom bnf-mode-algol-comments-style nil
  "Non-nil means use for BNF comments style introduced in ALGOL 60.

For the purpose of including text among the symbols of a program the
following \"comment\" conventions will hold:

  :------------------------------------------------:------------------:
  | The sequence of basic symbols:                 | is equivalent to |
  :------------------------------------------------:------------------:
  | ; comment <any sequence not containing ;>;     | ;                |
  | begin comment <any sequence not containing ;>; | begin            |
  :------------------------------------------------:------------------:

Note: Enabling this feature will disable comments recognition which use
semicolon only (\";\")."
  :type 'boolean)


;;; Specialized rx

(eval-when-compile
  (defconst bnf-rx-constituents
    `((bnf-rule-name . ,(rx (and
                             (1+ (or alnum digit))
                             (0+ (or alnum digit
                                     (in "!\"#$%&'()*+,-./:;=?@[]^_`{|}~")
                                     (in " \t"))))))
    "Additional special sexps for `bnf-rx'."))

  (defmacro bnf-rx (&rest sexps)
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
       (rx-to-string (cond ((null sexps) (error "No regexp"))
                           ((cdr sexps)  `(and ,@sexps))
                           (t            (car sexps)))
                     t))))


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
    ;; FIXME: Why?
    (modify-syntax-entry ?\^m "> b" table)

    ;; FIXME: "_" doesn't mean "symbol" but "symbol constituent".
    ;; I.e. the settings below mean that Emacs will consider "a:b=(c" as one
    ;; symbol (aka "identifier") which can be seen if you try to C-M-f and
    ;; C-M-b to move by sexps.

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
    (if bnf-mode-algol-comments-style
        (modify-syntax-entry ?\; ">" table)
      (progn
        (modify-syntax-entry ?\; "<" table)
        (modify-syntax-entry ?\n ">" table)))

    table)
  "Syntax table in use in `bnf-mode' buffers.")

(defconst bnf--syntax-propertize
  (syntax-propertize-rules
   ;; Fontify comments in ALGOL 60 style.
   ("\\(?:begin\\s-+\\|;\\s-*\\)\\(comment\\)\\(;\\|\\s-+[^;]*;\\)" (1 "<")))
  "Apply syntax table properties to special constructs.
Provide a macro to apply syntax table properties to comments in ALGOL 60
style.  Will be used only if `bnf-mode-algol-comments-style' is set to t")


;;; Initialization

;;;###autoload
(define-derived-mode bnf-mode prog-mode "BNF"
  "A major mode for editing BNF grammars.

\\{bnf-mode-map}
The variable `bnf-mode-algol-comments-style' can be changed to control
comments style used in grammars.

Turning on BNF mode calls the value of `prog-mode-hook' and then of
`bnf-mode-hook', if they are non-nil."
  :syntax-table bnf-mode-syntax-table

  ;; Comments setup
  (setq-local comment-use-syntax nil)
  (if bnf-mode-algol-comments-style
      (progn
        (setq-local comment-start "; comment ")
        (setq-local comment-end ";")
        (setq-local comment-start-skip "\\(?:\\(\\W\\|^\\)comment\\)\\s-+")
        (setq-local syntax-propertize-function bnf--syntax-propertize))
    (progn
      (setq-local comment-start "; ")
      (setq-local comment-end "")
      (setq-local comment-start-skip "\\(?:\\(\\W\\|^\\);+\\)\\s-+")))

  ;; Font locking
  (setq font-lock-defaults
        '(
          ;; Keywords
          bnf-font-lock-keywords
          ;; keywords-only
          nil
          ;; According to RFC5234 rule names are case insensitive.
          ;; The names <rulename>, <Rulename>, <RULENAME>, and <rUlENamE>
          ;; all refer to the same rule.  As far as is known, this doesn't
          ;; conflict with original BNF version
          ;; (see URL `https://tools.ietf.org/html/rfc5234')
          t)))

;; Invoke bnf-mode when appropriate

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bnf\\'" . bnf-mode))

(provide 'bnf-mode)
;;; bnf-mode.el ends here
