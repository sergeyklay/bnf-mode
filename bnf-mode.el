;;; bnf-mode.el --- Major mode for editing BNF grammars. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.4.5
;; URL: https://github.com/sergeyklay/bnf-mode
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (emacs "24.3"))
;; Revision: $Format:%h (%cD %d)$

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

;;   BNF Mode is a GNU Emacs major mode for editing BNF grammars.  Presently it
;; provides basic syntax and font-locking for BNF files.  BNF notation is
;; supported exactly form as it was first announced in the ALGOL 60 report.

;;; Code:


;;;; Requirements

(eval-when-compile
  (require 'rx))    ; `rx'


;;;; Customization

;;;###autoload
(defgroup bnf nil
  "Major mode for editing BNF grammars."
  :tag "BNF"
  :prefix "bnf-"
  :group 'languages
  :link '(url-link :tag "GitHub Page" "https://github.com/sergeyklay/bnf-mode")
  :link '(emacs-commentary-link :tag "Commentary" "bnf-mode"))


;;;; Specialized rx

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
       (rx-to-string (cond ((null sexps) (error "No regexp is provided"))
                           ((cdr sexps)  `(and ,@sexps))
                           (t            (car sexps)))
                     t))))


;;;; Font Locking

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


;;;; Syntax

(defvar bnf-mode-syntax-table
  (let ((table (make-syntax-table)))
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

    ;; Comments are begins with “;” and ends with “\n”
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)

    table)
  "Syntax table in use in `bnf-mode' buffers.")


;;;; Initialization

;;;###autoload
(define-derived-mode bnf-mode prog-mode "BNF"
  "A major mode for editing BNF grammars.

\\{bnf-mode-map}

Turning on BNF Mode calls the value of `prog-mode-hook' and then of
`bnf-mode-hook', if they are non-nil."
  :syntax-table bnf-mode-syntax-table

  ;; Comments setup
  (setq-local comment-use-syntax nil)
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?:\\(\\W\\|^\\);+\\)\\s-+")

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
