;;; test-helper.el --- BNF Mode: Non-interactive unit-test setup -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; Maintainer: Serghei Iakovlev
;; Version: 0.3.2
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

;; Non-interactive test suite setup for ERT Runner.

;;; Code:

(require 'ert-x)          ; `ert-with-test-buffer'
(require 'cl-lib)         ; `cl-defmacro'

;; reading/writing/loading compressed files
(require 'jka-compr)

;; Make sure the exact Emacs version can be found in the build output
(message "Running tests on Emacs %s" emacs-version)

;; The test fixtures assume an indentation width of 4, so we need to set that
;; up for the tests.
(setq-default default-tab-width 4
              indent-tabs-mode nil)

(when (require 'undercover nil t)
  (undercover "bnf-mode.el"))

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Don't load old byte-compiled versions
       (load-prefer-newer t))
  ;; Load the file under test
  (load (expand-file-name "bnf-mode" source-directory)))

;; Helpers

(cl-defmacro bnf-deftest (name args &body body)
  (declare (indent 2))
  `(ert-deftest ,(intern (format "bnf-ert-%s" name)) ()
     ""
     ,@args))

(cl-defmacro bnf-ert-with-test-buffer ((&rest args) initial-contents &body body)
  (declare (indent 2))
  `(ert-with-test-buffer (,@args)
     (bnf-mode)
     (insert ,initial-contents)
     ,@body))

(defmacro bnf-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (bnf-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(cl-defmacro bnf-def-indentation-test (name args initial-contents expected-output)
  (declare (indent 2))
  `(bnf-deftest ,name ,args
                (bnf-ert-with-test-buffer (:name ,(format "(Expected)" name))
                                          ,initial-contents
                                          (let ((indented (ert-buffer-string-reindented)))
                                            (delete-region (point-min) (point-max))
                                            (insert ,expected-output)
                                            (ert-with-test-buffer (:name ,(format "(Actual)" name))
                                              (bnf-mode)
                                              (insert indented)
                                              (should (equal indented ,expected-output)))))))

(defun bnf-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.

If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (bnf-test-with-temp-buffer content
                                 (get-text-property pos 'face))
    (get-text-property pos 'face)))

(when (s-contains? "--win" (getenv "ERT_RUNNER_ARGS"))
  (defun ert-runner/run-tests-batch-and-exit (selector)
    (ert-run-tests-interactively selector)))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; test-helper.el ends here
