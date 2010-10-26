;;; uncrustify.el --- apply uncrustify on buffer region

;; Copyright (C) 2010 Gustavo Lima Chaves

;; Author: Gustavo Lima Chaves <com dot gmail at limachaves, in reversed order>
;; Website: TODO
;; Keywords: uncrustify

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Quick program. Use uncrustify to check buffer for improper
;; indentation. It will grab indentation configuration from
;; $HOME/.uncrustify.cfg

;;; Usage:

;; M-x uncrustify to apply effects code beautifier program
;; uncrustify at region's contents

;;; Code:

(defgroup uncrustify nil
  "Customization group for uncrustify"
  :group 'uncrustify)

(defvar uncrustify-uncrustify-path nil
  "Whether the uncrustify executable in path.\n
When non-nil return value is the path to local uncrustify.\n
:SEE (URL `http://uncrustify.sourceforge.net/index.php')")
;;
(unless (bound-and-true-p uncrustify-uncrustify-path)
  (let ((uncrustify-path
         (or (executable-find "uncrustify")
             (executable-find "uncrustify.exe"))))
    (when uncrustify-path (setq uncrustify-uncrustify-path
                                uncrustify-path))))

(defun uncrustify ()
  (interactive)
  (let* ((cmd (format "%s -l C" uncrustify-uncrustify-path)))
    (shell-command-on-region (region-beginning) (region-end) cmd t t
                             null-device)))

(provide 'uncrustify)
;; uncrustify.el ends here