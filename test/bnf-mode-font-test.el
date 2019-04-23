;;; bnf-mode-font-test.el --- BNF Mode: Font highlighting test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; Maintainer: Serghei Iakovlev
;; Version: 0.4.1
;; URL: https://github.com/sergeyklay/bnf-mode

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

;;   Automate tests from the "test" directory using `ert', which comes bundled
;; with Emacs >= 24.1.

;;; Code:


;;;; Font locking

(ert-deftest bnf-mode-syntax-table/fontify-strings ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer
   "<string delimers> ::= \" | ' | ` | ”"
   (should-not (bnf-test-face-at 23))
   (should-not (bnf-test-face-at 27))
   (should-not (bnf-test-face-at 31))
   (should-not (bnf-test-face-at 35))))

(ert-deftest bnf-mode-syntax-table/fontify-line-comments ()
  :tags '(fontification syntax-table)
  (custom-set-variables '(bnf-mode-algol-comments-style nil))
  (bnf-test-with-temp-buffer
   "; A

<stm> ::= <decl> ; foo"
   (should (eq (bnf-test-face-at 1) 'font-lock-comment-delimiter-face))
   (should (eq (bnf-test-face-at 3) 'font-lock-comment-face))
   (should-not (bnf-test-face-at 5))
   (should (eq (bnf-test-face-at 24) 'font-lock-comment-face))))

;; TODO
(ert-deftest bnf-mode-syntax-table/fontify-algol-comments ()
  :tags '(fontification syntax-table)
  (custom-set-variables '(bnf-mode-algol-comments-style t))
  (bnf-test-with-temp-buffer "" ))

(ert-deftest bnf-mode-syntax-table/fontify-nonterminals ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer
   "<stm> ::= <decl>
angle-brackets ::= are-optional"
   ;; angle bracket
   (should-not (bnf-test-face-at 1))
   ;; "stm"
   (should (eq (bnf-test-face-at 2) 'font-lock-function-name-face))
   (should (eq (bnf-test-face-at 4) 'font-lock-function-name-face))
   ;; angle bracket
   (should-not (bnf-test-face-at 5))
   ;; "::=" symbol
   (should (eq (bnf-test-face-at 7) 'font-lock-constant-face))
   (should (eq (bnf-test-face-at 9) 'font-lock-constant-face))
   ;; angle bracket
   (should-not (bnf-test-face-at 11))
   ;; "dec" symbol
   (should (eq (bnf-test-face-at 12) 'font-lock-builtin-face))
   (should (eq (bnf-test-face-at 15) 'font-lock-builtin-face))))

(ert-deftest bnf-mode-syntax-table/fontify-nonterminals-case ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer
   "<RULE> ::= <foo>
<RuLe> ::= <foO>"
   (should (eq (bnf-test-face-at 2) 'font-lock-function-name-face))
   (should (eq (bnf-test-face-at 5) 'font-lock-function-name-face))
   (should-not (bnf-test-face-at 17))
   (should (eq (bnf-test-face-at 19) 'font-lock-function-name-face))
   (should (eq (bnf-test-face-at 22) 'font-lock-function-name-face))
   (should-not (bnf-test-face-at 23))
   (should (eq (bnf-test-face-at 30) 'font-lock-builtin-face))
   (should (eq (bnf-test-face-at 32) 'font-lock-builtin-face))
   (should-not (bnf-test-face-at 33))))

(ert-deftest bnf-mode-syntax-table/fontify-nonterminals-start-pos ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer
   "   <rule> ::= <foo>"
   (should-not (bnf-test-face-at 4))
   (should (eq (bnf-test-face-at 5) 'font-lock-function-name-face))
   (should (eq (bnf-test-face-at 6) 'font-lock-function-name-face))
   (should (eq (bnf-test-face-at 7) 'font-lock-function-name-face))
   (should (eq (bnf-test-face-at 8) 'font-lock-function-name-face))
   (should-not (bnf-test-face-at 9))))

(ert-deftest bnf-mode-syntax-table/fontify-sequence ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer
   "<rule> ::= <foo> <bar> <baz>"
   ;; "<" angle bracket
   (should-not (bnf-test-face-at 1))
   ;; "rule"
   (should (eq (bnf-test-face-at 2) 'font-lock-function-name-face))
   (should (eq (bnf-test-face-at 5) 'font-lock-function-name-face))
   ;; ">" angle bracket
   (should-not (bnf-test-face-at 6))
   ;; "foo"
   (should (eq (bnf-test-face-at 13) 'font-lock-builtin-face))
   (should (eq (bnf-test-face-at 15) 'font-lock-builtin-face))
   ;; space
   (should-not (bnf-test-face-at 17))
   ;; "bar"
   (should (eq (bnf-test-face-at 19) 'font-lock-builtin-face))
   (should (eq (bnf-test-face-at 21) 'font-lock-builtin-face))
   ;; space
   (should-not (bnf-test-face-at 23))
   ;; "baz"
   (should (eq (bnf-test-face-at 25) 'font-lock-builtin-face))
   (should (eq (bnf-test-face-at 27) 'font-lock-builtin-face))))

(ert-deftest bnf-mode-syntax-table/fontify-alternatives ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer
   "<foo> | <bar> | <baz>"
   ;; "foo"
   (should (eq (bnf-test-face-at 2) 'font-lock-builtin-face))
   (should (eq (bnf-test-face-at 4) 'font-lock-builtin-face))
   ;; "|"
   (should (eq (bnf-test-face-at 7) 'font-lock-warning-face))
   ;; "bar"
   (should (eq (bnf-test-face-at 10) 'font-lock-builtin-face))
   (should (eq (bnf-test-face-at 12) 'font-lock-builtin-face))
   ;; "|"
   (should (eq (bnf-test-face-at 15) 'font-lock-warning-face))
   ;; "baz"
   (should (eq (bnf-test-face-at 18) 'font-lock-builtin-face))
   (should (eq (bnf-test-face-at 20) 'font-lock-builtin-face))))

(ert-deftest bnf-mode-syntax-table/fontify-rule-punctuation ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer
   "
<proper string> ::=
        <any sequence of symbols not containing ` or ' >
        | <empty>"
   ;; "proper string"
   (should (eq (bnf-test-face-at 3) 'font-lock-function-name-face))
   (should (eq (bnf-test-face-at 15) 'font-lock-function-name-face))
   ;; "any sequence of symbols not containing ` or ' "
   (should (eq (bnf-test-face-at 31) 'font-lock-builtin-face))
   (should (eq (bnf-test-face-at 76) 'font-lock-builtin-face))
   ;; "empty"
   (should (eq (bnf-test-face-at 90) 'font-lock-builtin-face))
   (should (eq (bnf-test-face-at 94) 'font-lock-builtin-face))))

(provide 'bnf-mode-font-test)
;;; bnf-mode-font-test.el ends here
