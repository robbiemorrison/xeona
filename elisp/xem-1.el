;;; xem-1.el --- additional code 1 for 'xem.el'

;  file-purpose     : emacs additional code 1 for 'xem.el'
;  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
;  file-create-date : Tue 24-Aug-2010 09:51 UTC
;  file-status      : ongoing
;  file-keywords    : emacs xeona

;  $Revision: 5945 $
;  $Date: 2011-02-22 11:04:02 +0100 (Tue, 22 Feb 2011) $
;  $Author: robbie $
;  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/elisp/xem-1.el $

;; Copyright (C) 2010-2011 Robbie Morrison

;; This file is *NOT* part of GNU Emacs.

;; This file is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software
;; Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You may have received a copy of the GNU General
;; Public License along with GNU Emacs; see the file
;; COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; ---------------------------------
;;  rename utilities
;; ---------------------------------

(defun xem-uncamel ()
  "Uncamel current symbol using `xem-decamel'."
  (interactive)
  (let ((beg       nil )                          ; symbol start
        (end       nil )                          ; symbol end
        (original  nil ))                         ; current identifier
    ;; grab the current identifier
    (setq original (thing-at-point 'symbol))      ; normal word plus '_' for C language
    (if (stringp original)                        ; check for meaningful string
        (progn
          ;; move to start of the identifier (decimal character codes are used)
          (if (not (or (= (char-before) 10)       ; peek backwards and check for newline
                       (= (char-before) 32)))     ; peek backwards and check for space
              (backward-word 1))
          ;; process
          (setq beg (point))
          (forward-char (length original))
          (setq end (point))
          (kill-region beg end)
          (insert (xem-decamel original "-"))
          ;; completion reporting
          (message "  xem: uncamel complete using '%s'." original))
      (message "  xem: uncamel, nothing to process."))))

(defun xem-decamel (symbol &optional replace)
  "Decamel SYMBOL using underscores or optionally the string REPLACE.
For example (note the behavior of multiple uppercasing):\n
    someVarName -> some_var_name
    abCD_fg     -> ab-cd_fg\n"
  (let ((case-fold-search  nil )                  ; CAUTION: must make case sensitive
        (default-replace  "_"  )                  ; default to underscore
        (output            nil ))
    (if (not replace) (setq replace default-replace))
    (while (string-match "\\([[:lower:]]\\|[[:digit:]]\\)\\([[:upper:]]\\)" symbol)
      (setq symbol
            (replace-match (concat "\\1" replace "\\2") t nil symbol)))    ; fixed case, literal (no)
    (downcase symbol)))

;; ---------------------------------
;;  general utilities
;; ---------------------------------

(defun xem-string-trim (string)
  "Trim STRING from both ends.

Unlike the standard version, this function returns an empty string if supplied a nil."
  ;; [:space:] comprises space, carriage-return, newline, tab, vertical-tab, form-feed
  (let ((regex  "^[[:space:]]*\\(.*?\\)[[:space:]]*$" ))    ; CAUTION: non-greedy '*?' required
    (if (not (stringp string))
        ""                                   ; else could be () to return 'nil'
      (if (string-match regex string)
          (match-string 1 string)            ; 1 indicates first \( \) construct
        string))))                           ; no match, return original

(defun xem-squeeze-lines ()
  "Automatically squeeze all multiple strictly-blank lines in current buffer."
  (interactive)
  (save-excursion                            ; hold cursor position
    (let ((sections     0   )                ; sections treated
          (lines        0   )                ; lines processed
          (sect-plural  "s" )                ; "section" pluralization
          (line-plural  "s" ))               ; "line" pluralization
      (setq lines (count-lines (point-min) (point-max)))              ; original line count
      (goto-char (point-min))      ; start at beginning
      (while (re-search-forward    ; look for matches
              "\n\n\n+"
              nil t)               ; optional buffer position bound; return 'nil' not error on fail
        (replace-match
         "\n\n"
         t t)                      ; do not alter case; insert literally
        (setq sections (+ 1 sections)))
      (goto-char (point-min))      ; start at beginning again
      (while (looking-at "\n\n")   ; look for special match
        (kill-line)
        (setq sections (+ 1 sections)))
      (setq lines (- lines (count-lines (point-min) (point-max))))    ; original minus final line count
      (if (= sections 1) (setq sect-plural ""))
      (if (= lines    1) (setq line-plural ""))
      (message "  xem: squeeze-lines complete, processed %d section%s totaling %d line%s."
               sections sect-plural lines line-plural)) ))

(defun xem-trim-trailing ()
  "Trim trailing white space."
  (interactive)
  (delete-trailing-whitespace)
  (message "  xem: trim trailing whitespace complete"))

(defun xem-tar-interpret (code)
  "Interprete GNU tar exit CODE."
  ;; CAUTION: no `message' calls so that this function can be
  ;; used directly in completion reports
  (let* ((msg  nil))
    (unless (integerp code)
      (error "  xem: tar-interpret: error: non-integer argument given"))
    (cond
     ((= code 0) (setq msg (format "%d = success"                                  code)))
     ((= code 1) (setq msg (format "%d = some files differ (certain options only)" code)))
     ((= code 2) (setq msg (format "%d = unspecified fatal errors"                 code)))
     (t          (setq msg (format "%d = undocumented return"                      code))))
    msg ))                                   ; expose return value

;  $Id: xem-1.el 5945 2011-02-22 10:04:02Z robbie $
;  end of file

