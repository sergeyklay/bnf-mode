;;; utils.el --- BNF Mode: Non-interactive unit-test setup -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;;         immerrr <immerrr+lua@gmail.com>
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

;; Non-interactive test suite setup for ERT Runner.

;;; Code:

(require 'buttercup)
(require 'cl-lib)    ; `cl-defmacro'

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Don't load old byte-compiled versions
       (load-prefer-newer t))
  ;; Load the file under test
  (load (expand-file-name "bnf-mode" source-directory) nil 'nomessage))

(cl-defmacro with-bnf-buffer (content &rest body)
  "Evaluate BODY in a temporary BNF buffer with CONTENT."
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

(defun bnf-make-font-lock-faces (sym)
  "Decorate SYM with font-lock-%s-face.
If SYM is a list, this function will be called recursively to
decorate each of symbol."
  (or (cond
       ((symbolp sym)
        (intern-soft (format "font-lock-%s-face" (symbol-name sym))))
       ((listp sym) (mapcar 'bnf-make-font-lock-faces sym)))
      sym))

(defun get-str-faces (str)
  "Find contiguous spans of non-default faces in STR.
E.g. for properly fontified Lua string \"local x = 100\" it should return
  '(\"local\" font-lock-keyword-face
    \"x\" font-lock-variable-name-face
    \"100\" font-lock-constant-face)"
  (let ((pos 0)
        nextpos
        result prop newprop)
    (while pos
      (setq nextpos (next-property-change pos str)
            newprop (or (get-text-property pos 'face str)
                        (get-text-property pos 'font-lock-face str)))
      (when (not (equal prop newprop))
        (setq prop newprop)
        (when (listp prop)
          (when (eq (car-safe (last prop)) 'default)
            (setq prop (butlast prop)))
          (when (= 1 (length prop))
            (setq prop (car prop)))
          (when (symbolp prop)
            (when (eq prop 'default)
              (setq prop nil))))
        (when prop
          (push (substring-no-properties str pos nextpos) result)
          (push prop result)))
      (setq pos nextpos))
    (nreverse result)))

(defun bnf-get-line-faces (str)
  "Find contiguous spans of non-default faces in each line of STR.
The result is a list of lists."
  (mapcar
   'get-str-faces
   (split-string
    (with-bnf-buffer str (buffer-string))
    "\n" nil)))

(defun to-be-fontified-as (text faces)
  "Check that TEXT is fontified using FACES.
Custom matcher to test font locking using `buttercup'."
  (let ((expected-faces (bnf-make-font-lock-faces faces))
        (result-faces (bnf-get-line-faces text))
        (lineno 1))
    (when (/= (length expected-faces) (length result-faces))
        (buttercup-fail "\
Fontification check failed for:
%S
  Text contains %d lines, face list contains %d lines"
                        text (length result-faces)
                        (length expected-faces)))
    (while expected-faces
      (unless (equal (car expected-faces) (car result-faces))
        (buttercup-fail "\
Fontification check failed on line %d for:
%S
  Result faces:   %S
  Expected faces: %S"
                        lineno text (car expected-faces) (car result-faces)))
      (setq expected-faces (cdr expected-faces)
            result-faces (cdr result-faces)
            lineno (1+ lineno)))
    (cons t "Fontification check passed")))

(buttercup-define-matcher :to-be-fontified-as (text faces)
 (to-be-fontified-as (funcall text) (funcall faces)))

;;; utils.el ends here
