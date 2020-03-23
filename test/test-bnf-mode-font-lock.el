;;; test-bnf-mode-font-lock.el --- BNF Mode: Font locking tests -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.4.5
;; URL: https://github.com/sergeyklay/bnf-mode

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

;; Define test-suites to test `bnf-mode' font locking using `buttercup'.

;;; Code:

(require 'buttercup)

(when (require 'undercover nil t)
  ;; Track coverage, but don't send to coverage serivice.  Save in parent
  ;; directory as undercover saves paths relative to the repository root.
  (undercover "*.el" "test/utils.el"
              (:report-file "coverage-final.json")
              (:send-report nil)))

(let* ((current-dir (file-name-directory (or load-file-name (buffer-file-name)
                                             default-directory))))
  (load (concat current-dir "utils.el") nil 'nomessage 'nosuffix))


;;;; Tests

(describe "BNF Fontification"
  (it "does not fontify strings"
    (expect "<string delimers> ::= \" | ' | ` | ”"
            :to-be-fontified-as
            '(("string delimers" function-name "::=" constant "|" warning
               "|" warning "|" warning))))

  (it "fontifies line comments"
    (expect "; A
     <stm> ::= <decl> ; foo"
            :to-be-fontified-as
            '(("; " comment-delimiter "A" comment)
              ("stm" function-name "::=" constant "decl" builtin
               "; foo" comment))))

  (it "does not mix terminals and nonterminals"
    (expect "<stm> ::= <decl>
     angle-brackets ::= are-optional"
            :to-be-fontified-as
            '(("stm" function-name "::=" constant "decl" builtin)
              ("::=" constant))))

  (it "fontifies nonterminals despite the case"
    (expect "<RULE> ::= <foo>
     <RuLe> ::= <foO>"
            :to-be-fontified-as
            '(("RULE" function-name "::=" constant "foo" builtin)
              ("RuLe" function-name "::=" constant "foO" builtin))))

  (it "fontifies nonterminals despite the indentation"
    (expect "   <rule> ::= <subrule>"
            :to-be-fontified-as
            '(("rule" function-name "::=" constant "subrule" builtin))))

  (it "fontifies sequences"
    (expect "<rule> ::= <foo> <bar> <baz>"
            :to-be-fontified-as
            '(("rule" function-name "::=" constant "foo" builtin
               "bar" builtin "baz" builtin))))

  (it "fontifies alternatives"
    (expect "<foo> | <bar> | <baz>"
            :to-be-fontified-as
            '(("foo" builtin "|" warning "bar" builtin
               "|" warning "baz" builtin))))

  (it "fontifies rule punctuation"
    (expect "<proper string> ::=
        <any sequence of symbols not containing ` or ' >
        | <empty>"
            :to-be-fontified-as
            '(("proper string" function-name "::=" constant)
              ("any sequence of symbols not containing ` or ' " builtin)
              ("|" warning "empty" builtin)))))

;;; test-bnf-mode-font-lock.el ends here
