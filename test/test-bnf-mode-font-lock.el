;;; test-bnf-mode-font-lock.el --- BNF Mode: Font locking tests -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Free Software Foundation, Inc.

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.4.4
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
    (bnf-test-with-temp-buffer
     "<string delimers> ::= \" | ' | ` | ”"
     (should-not (bnf-get-face-at 23))
     (should-not (bnf-get-face-at 27))
     (should-not (bnf-get-face-at 31))
     (should-not (bnf-get-face-at 35))))

  (it "fontify line comments"
    (custom-set-variables '(bnf-mode-algol-comments-style nil))
    (bnf-test-with-temp-buffer
     "; A

<stm> ::= <decl> ; foo"
     (should (eq (bnf-get-face-at 1) 'font-lock-comment-delimiter-face))
     (should (eq (bnf-get-face-at 3) 'font-lock-comment-face))
     (should-not (bnf-get-face-at 5))
     (should (eq (bnf-get-face-at 24) 'font-lock-comment-face))))

  ;; TODO(sergei): Implement me
  (it "fontify ALGOL comments"
    (custom-set-variables '(bnf-mode-algol-comments-style t))
    (bnf-test-with-temp-buffer "" ))

  (it "fontify nonterminals"
    (bnf-test-with-temp-buffer
     "<stm> ::= <decl>
angle-brackets ::= are-optional"
     ;; angle bracket
     (should-not (bnf-get-face-at 1))
     ;; "stm"
     (should (eq (bnf-get-face-at 2) 'font-lock-function-name-face))
     (should (eq (bnf-get-face-at 4) 'font-lock-function-name-face))
     ;; angle bracket
     (should-not (bnf-get-face-at 5))
     ;; "::=" symbol
     (should (eq (bnf-get-face-at 7) 'font-lock-constant-face))
     (should (eq (bnf-get-face-at 9) 'font-lock-constant-face))
     ;; angle bracket
     (should-not (bnf-get-face-at 11))
     ;; "dec" symbol
     (should (eq (bnf-get-face-at 12) 'font-lock-builtin-face))
     (should (eq (bnf-get-face-at 15) 'font-lock-builtin-face))))

  (it "fontify nonterminals despite the case"
    (bnf-test-with-temp-buffer
     "<RULE> ::= <foo>
<RuLe> ::= <foO>"
     (should (eq (bnf-get-face-at 2) 'font-lock-function-name-face))
     (should (eq (bnf-get-face-at 5) 'font-lock-function-name-face))
     (should-not (bnf-get-face-at 17))
     (should (eq (bnf-get-face-at 19) 'font-lock-function-name-face))
     (should (eq (bnf-get-face-at 22) 'font-lock-function-name-face))
     (should-not (bnf-get-face-at 23))
     (should (eq (bnf-get-face-at 30) 'font-lock-builtin-face))
     (should (eq (bnf-get-face-at 32) 'font-lock-builtin-face))
     (should-not (bnf-get-face-at 33))))

  (it "fontify nonterminals despite the indentation"
    (bnf-test-with-temp-buffer
     "   <rule> ::= <foo>"
     (should-not (bnf-get-face-at 4))
     (should (eq (bnf-get-face-at 5) 'font-lock-function-name-face))
     (should (eq (bnf-get-face-at 6) 'font-lock-function-name-face))
     (should (eq (bnf-get-face-at 7) 'font-lock-function-name-face))
     (should (eq (bnf-get-face-at 8) 'font-lock-function-name-face))
     (should-not (bnf-get-face-at 9))))

  (it "fontify sequences"
    (bnf-test-with-temp-buffer
     "<rule> ::= <foo> <bar> <baz>"
     ;; "<" angle bracket
     (should-not (bnf-get-face-at 1))
     ;; "rule"
     (should (eq (bnf-get-face-at 2) 'font-lock-function-name-face))
     (should (eq (bnf-get-face-at 5) 'font-lock-function-name-face))
     ;; ">" angle bracket
     (should-not (bnf-get-face-at 6))
     ;; "foo"
     (should (eq (bnf-get-face-at 13) 'font-lock-builtin-face))
     (should (eq (bnf-get-face-at 15) 'font-lock-builtin-face))
     ;; space
     (should-not (bnf-get-face-at 17))
     ;; "bar"
     (should (eq (bnf-get-face-at 19) 'font-lock-builtin-face))
     (should (eq (bnf-get-face-at 21) 'font-lock-builtin-face))
     ;; space
     (should-not (bnf-get-face-at 23))
     ;; "baz"
     (should (eq (bnf-get-face-at 25) 'font-lock-builtin-face))
     (should (eq (bnf-get-face-at 27) 'font-lock-builtin-face))))

  (it "fontify alternatives"
    (bnf-test-with-temp-buffer
     "<foo> | <bar> | <baz>"
     ;; "foo"
     (should (eq (bnf-get-face-at 2) 'font-lock-builtin-face))
     (should (eq (bnf-get-face-at 4) 'font-lock-builtin-face))
     ;; "|"
     (should (eq (bnf-get-face-at 7) 'font-lock-warning-face))
     ;; "bar"
     (should (eq (bnf-get-face-at 10) 'font-lock-builtin-face))
     (should (eq (bnf-get-face-at 12) 'font-lock-builtin-face))
     ;; "|"
     (should (eq (bnf-get-face-at 15) 'font-lock-warning-face))
     ;; "baz"
     (should (eq (bnf-get-face-at 18) 'font-lock-builtin-face))
     (should (eq (bnf-get-face-at 20) 'font-lock-builtin-face))))

  (it "fontify rule punctuation"
    (bnf-test-with-temp-buffer
     "
<proper string> ::=
        <any sequence of symbols not containing ` or ' >
        | <empty>"
     ;; "proper string"
     (should (eq (bnf-get-face-at 3) 'font-lock-function-name-face))
     (should (eq (bnf-get-face-at 15) 'font-lock-function-name-face))
     ;; "any sequence of symbols not containing ` or ' "
     (should (eq (bnf-get-face-at 31) 'font-lock-builtin-face))
     (should (eq (bnf-get-face-at 76) 'font-lock-builtin-face))
     ;; "empty"
     (should (eq (bnf-get-face-at 90) 'font-lock-builtin-face))
     (should (eq (bnf-get-face-at 94) 'font-lock-builtin-face)))))

;;; test-bnf-mode-font-lock.el ends here
