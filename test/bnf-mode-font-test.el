;;; bnf-mode-font-test.el --- BNF Mode: Font highlighting test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; Maintainer: Serghei Iakovlev
;; Version: 0.2.0
;; URL: https://github.com/sergeyklay/bnf-mode

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

;;   Automate tests from the "test" directory using `ert', which comes bundled
;; with Emacs >= 24.1.

;;; Code:


;;;; Utilities

(defun bnf-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.

If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (bnf-test-with-temp-buffer content
                                 (get-text-property pos 'face))
    (get-text-property pos 'face)))


;;;; Font locking

(ert-deftest bnf-mode-syntax-table/fontify-strings ()
  :tags '(fontification syntax-table)
  (should (eq (bnf-test-face-at 11 "<foo> ::= \"bar\"") 'font-lock-string-face)))

(ert-deftest bnf-mode-syntax-table/fontify-line-comment ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer "; A

<stm> ::= <decl> ; foo"
                             (should (eq (bnf-test-face-at 1) 'font-lock-comment-face))
                             (should (eq (bnf-test-face-at 3) 'font-lock-comment-face))
                             (should-not (bnf-test-face-at 5))
                             (should (eq (bnf-test-face-at 24) 'font-lock-comment-face))))

(ert-deftest bnf-mode-syntax-table/fontify-nonterminals ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer "<stm> ::= <decl>
angle-brackets ::= are-optional"
                             ;; angle bracket
                             (should-not (bnf-test-face-at 1))
                             ;; “stm”
                             (should (eq (bnf-test-face-at 2) 'font-lock-function-name-face))
                             (should (eq (bnf-test-face-at 4) 'font-lock-function-name-face))
                             ;; angle bracket
                             (should-not (bnf-test-face-at 5))
                             ;; “::=” symbol
                             (should (eq (bnf-test-face-at 7) 'font-lock-constant-face))
                             (should (eq (bnf-test-face-at 9) 'font-lock-constant-face))
                             ;; angle bracket
                             (should-not (bnf-test-face-at 11))
                             ;; “dec” symbol
                             (should (eq (bnf-test-face-at 12) 'font-lock-builtin-face))
                             (should (eq (bnf-test-face-at 15) 'font-lock-builtin-face))
                             ;; “angle-brackets”
                             (should (eq (bnf-test-face-at 18) 'font-lock-function-name-face))
                             (should (eq (bnf-test-face-at 31) 'font-lock-function-name-face))
                             ;; space
                             (should-not (bnf-test-face-at 32))
                             ;; “are-optional” symbol
                             (should (eq (bnf-test-face-at 37) 'font-lock-builtin-face))
                             (should (eq (bnf-test-face-at 48) 'font-lock-builtin-face))))

(ert-deftest bnf-mode-syntax-table/fontify-nonterminals-case ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer "<RULE> ::= foo
<RuLe> ::= <foO>"
                             (should (eq (bnf-test-face-at 2) 'font-lock-function-name-face))
                             (should (eq (bnf-test-face-at 5) 'font-lock-function-name-face))
                             (should (eq (bnf-test-face-at 17) 'font-lock-function-name-face))
                             (should (eq (bnf-test-face-at 20) 'font-lock-function-name-face))
                             (should-not (bnf-test-face-at 21))
                             (should (eq (bnf-test-face-at 28) 'font-lock-builtin-face))
                             (should (eq (bnf-test-face-at 30) 'font-lock-builtin-face))
                             (should-not (bnf-test-face-at 31))))

(ert-deftest bnf-mode-syntax-table/fontify-nonterminals-start-pos ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer "   <rule> ::= doo"
                             (should-not (bnf-test-face-at 4))
                             (should (eq (bnf-test-face-at 5) 'font-lock-function-name-face))
                             (should (eq (bnf-test-face-at 6) 'font-lock-function-name-face))
                             (should (eq (bnf-test-face-at 7) 'font-lock-function-name-face))
                             (should (eq (bnf-test-face-at 8) 'font-lock-function-name-face))
                             (should-not (bnf-test-face-at 9))))

(ert-deftest bnf-mode-syntax-table/fontify-sequence ()
  :tags '(fontification syntax-table)
  (bnf-test-with-temp-buffer "rule ::= foo bar baz"
                             ;; “rule”
                             (should (eq (bnf-test-face-at 1) 'font-lock-function-name-face))
                             (should (eq (bnf-test-face-at 4) 'font-lock-function-name-face))
                             ;; “foo”
                             (should (eq (bnf-test-face-at 10) 'font-lock-builtin-face))
                             (should (eq (bnf-test-face-at 12) 'font-lock-builtin-face))
                             ;; space
                             (should-not (bnf-test-face-at 13))
                             ;; “bar”
                             (should (eq (bnf-test-face-at 14) 'font-lock-builtin-face))
                             (should (eq (bnf-test-face-at 16) 'font-lock-builtin-face))
                             ;; space
                             (should-not (bnf-test-face-at 17))
                             ;; “baz”
                             (should (eq (bnf-test-face-at 18) 'font-lock-builtin-face))
                             (should (eq (bnf-test-face-at 10) 'font-lock-builtin-face))))

(provide 'bnf-mode-font-test)

;;; bnf-mode-font-test.el ends here
