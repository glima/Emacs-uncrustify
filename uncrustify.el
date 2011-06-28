;;; uncrustify.el --- apply uncrustify on buffer region

;; Copyright (C) 2010 Gustavo Lima Chaves

;; Author: Gustavo Lima Chaves <com dot gmail at limachaves, in reversed order>
;; Modified: Gordon Read <com dot f2s at gtread, in reversed order>
;;           Added Customisation vars, uncrustify-region, uncrustify-buffer and
;;           uncrustify-on-save hook (10/Jun/2011)
;; Website: TODO
;; Keywords: uncrustify

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Quick program. Use uncrustify to check buffer for improper
;; indentation. It will grab indentation configuration from
;; the configured uncrustify specification file.

;;; Usage:

;; M-x uncrustify-buffer to apply effects of code beautifier program
;; "uncrustify" to whole buffer.
;;
;; M-x uncrustify-region to apply effects of code beautifer program
;; "uncrustify" to selected region only.
;;
;; See also Customisation group "uncrustify"

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(defgroup uncrustify nil
 "Customization group for uncrustify"
 :group 'uncrustify)

(defcustom uncrustify-uncrustify-cfg-file "~/.uncrustify.cfg"
 "Path to uncrustify configuration file.\n"
 :type 'string
 :group 'uncrustify)

(defcustom uncrustify-args ""
 "Additional arguments to pass to uncrustify.
These may be, for example \"-l C\" to specify the C language, \"-l CPP\" to specify C++ etc."
 :type 'string
 :group 'uncrustify)

(defcustom uncrustify-uncrustify-on-save nil
 "Whether to uncrustify the buffer when file is saved.\n
When non-nil, uncrustify will be run when a cc-mode buffer is saved.
The configuration file will be read from the specification given by
`uncrustify-uncrustify-cfg-file'. The cursor will attempt to (re)locate the current
line, which might change as a result of the uncrustification."
 :type '(choice (const :tag "off" nil)
                (const :tag "on" t))
 :group 'uncrustify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vars

(defvar uncrustify-uncrustify-path nil
 "The uncrustify executable in path.\n
When non-nil return value is the path to local uncrustify.\n
:SEE (URL `http://uncrustify.sourceforge.net/index.php')")
;;
(unless (bound-and-true-p uncrustify-uncrustify-path)
 (let ((uncrustify-path
        (or (executable-find "uncrustify")
            (executable-find "uncrustify.exe"))))
   (when uncrustify-path (setq uncrustify-uncrustify-path
                               uncrustify-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private impl fns

(defun uncrustify-impl (point-a point-b)
 ""
 (if uncrustify-uncrustify-path
     (let* ((cmd (format "%s -c %s %s" uncrustify-uncrustify-path uncrustify-uncrustify-cfg-file uncrustify-args)))
       (shell-command-on-region point-a point-b cmd t t
                                null-device))
   (message "Uncrustify not found in path - no change"))
 nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public functions

(defun uncrustify ()
 "Uncrustify the marked region.
The configuration file will be read from the specification given by
`uncrustify-uncrustify-cfg-file'."
 (interactive)
 (save-excursion
   (uncrustify-impl (region-beginning) (region-end))))

(defun uncrustify-buffer ()
 "Uncrustify the entire buffer.
The configuration file will be read from the specification given by
`uncrustify-uncrustify-cfg-file'. The cursor will attempt to (re)locate the current
line, which might change as a result of the uncrustification."
 (interactive)
 (let* ((uncrustify-current-line (line-number-at-pos)))
   (save-excursion
     (uncrustify-impl (point-min) (point-max)))
   (goto-char (point-min)) (forward-line (1- uncrustify-current-line))))

;; If uncrustify-uncrustify-on-save is non nil, uncrustify the whole buffer.
(defun uncrustify-uncrustify-buffer-on-save ()
 (if uncrustify-uncrustify-on-save (uncrustify-buffer))
 nil)

;; add a c-mode-common-hook that uncrustifies the buffer when it is saved,
;; iff uncrustify-uncrustify-on-save is non nil.
(add-hook 'c-mode-common-hook
         '(lambda()
            (make-local-variable 'write-contents-hooks)
            (add-hook 'write-contents-hooks 'uncrustify-uncrustify-buffer-on-save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'uncrustify)
;; uncrustify.el ends here
