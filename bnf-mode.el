;;; bnf-mode.el --- Major mode for editing BNF grammars. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <gnu@serghei.pl>
;; Maintainer: Serghei Iakovlev <gnu@serghei.pl>
;; Version: 0.4.5
;; URL: https://github.com/sergeyklay/bnf-mode
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (emacs "27.1"))
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
  (require 'rx))    ; `rx', `rx-define'


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

;; Any valid BNF rule name.  This rule was obtained by studying
;; ALGOL 60 report, where the BNF was officially announced.
;; Please note: This rule is not suitable for ABNF or EBNF
;; (see URL `https://www.masswerk.at/algol60/report.htm').
(rx-define bnf-rule-name
  (seq
   (1+ (or alnum digit))
   (0+ (or alnum digit
           (in "!\"#$%&'()*+,-./:;=?@[]^_`{|}~")
           (in " \t")))))


;;;; Font Locking

(defvar bnf-font-lock-keywords
  `(;; LHS nonterminals may be preceded
    ;; by an unlimited number of spaces
    (,(rx (and line-start
               (0+ space)
               "<"
               (group bnf-rule-name)
               ">"
               (0+ space)
               "::="))
     1 font-lock-function-name-face)
    ;; Other nonterminals
    (,(rx (and (0+ space)
               "<"
               (group bnf-rule-name)
               ">"
               (0+ space)))
     1 font-lock-builtin-face)
    ;; “may expand into” symbol
    (,(rx (and ">"
               (0+ (in " \t\n"))
               (group "::=")
               (0+ space)))
     1 font-lock-constant-face)
    ;; Alternatives
    (,(rx (and (0+ space)
               (group "|")
               (0+ space)))
     1 font-lock-warning-face))
  "Font lock BNF keywords for BNF Mode.")


;;;; Syntax

(defvar bnf-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Treat :, =, and | as punctuation
    (modify-syntax-entry ?\: "." table)
    (modify-syntax-entry ?\= "." table)
    (modify-syntax-entry ?\| "." table)

    ;; In BNF there are no strings
    ;; so treat ' and " as punctuation
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\' "." table)

    ;; In BNF there are no grouping
    ;; brackets except angle ones
    (modify-syntax-entry ?\( "." table)
    (modify-syntax-entry ?\) "." table)
    (modify-syntax-entry ?\{ "." table)
    (modify-syntax-entry ?\} "." table)
    (modify-syntax-entry ?\[ "." table)
    (modify-syntax-entry ?\] "." table)

    ;; Treat angle brackets as punctuation by default.
    ;; We'll ajust them later, in `bnf--syntax-propertize'.
    (modify-syntax-entry ?\< "." table)
    (modify-syntax-entry ?\> "." table)

    ;; Comments are begins with “;” and ends with “\n”
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)

    table)
  "Syntax table in use in `bnf-mode' buffers.")

(defconst bnf--syntax-propertize
  (syntax-propertize-rules
   ;; Group angle brackets
   ("\\(<\\)\\([^<>]*\\)\\(>\\)"
    (1 "(>")
    (3 ")<")))
  "Syntax propertization rules for `bnf-mode'.

These rules assign appropriate syntax properties to specific
sequences in BNF grammar files, ensuring correct syntax
highlighting and code navigation in `bnf-mode' buffers.")


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

  ;; Enable dynamic syntax properties for accurate parsing
  (setq-local syntax-propertize-function bnf--syntax-propertize)

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
