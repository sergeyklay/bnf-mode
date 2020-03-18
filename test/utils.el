;;; utils.el --- BNF Mode: Non-interactive unit-test setup -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Free Software Foundation, Inc

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

;; Non-interactive test suite setup for ERT Runner.

;;; Code:

(require 'buttercup)
(require 'cl-lib)    ; `cl-defmacro'

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Don't load old byte-compiled versions
       (load-prefer-newer t))
  ;; Load the file under test
  (load (expand-file-name "bnf-mode" source-directory)))

(cl-defmacro bnf-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (bnf-mode)

     ,(if (fboundp 'font-lock-ensure)
          '(font-lock-ensure)
        '(with-no-warnings (font-lock-fontify-buffer)))

     (pop-to-buffer (current-buffer))
     (goto-char (point-min))
     (unwind-protect
         (progn ,@body))))

(defun bnf-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.
If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (bnf-test-with-temp-buffer content
                                 (get-text-property pos 'face))
    (get-text-property pos 'face)))

;;; utils.el ends here