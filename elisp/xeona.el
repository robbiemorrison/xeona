;;; xeona.el --- central point of contact for all xeona emacs support

;  file-purpose     : central point of contact for all xeona emacs support
;  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
;  file-create-date : Thu 22-Jul-2010 07:37 UTC
;  file-status      : working
;  file-keywords    : emacs xeona

;  $Revision: 7404 $
;  $Date: 2011-09-28 15:11:20 +0200 (Wed, 28 Sep 2011) $
;  $Author: robbie $
;  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/elisp/xeona.el $

;  XEONA CODEBASE COPY
;
;  * this code originated as part of the xeona codebase
;
;  * the GLPv2 license included here derives from Emacs Lisp
;    coding practice and from not the 'xeona' project --
;    users can optionally chose to adopt the 'xeona' norm
;    instead
;
;  * if this file is kept in your subversion working copy,
;    you may need to tell Emacs where to find it -- see
;    below for a representative '.emacs' entry

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

;;; Commentary:

;;  ToDos
;;
;;      * complete the decoupling from my '~/.emacs' file
;;        call : $ emacs --no-init-file \
;;                       --load /path/to/xeona.el \
;;                       --file submodel.15.guard.xem
;;        then : M-x xem-mode
;;               M-x xog-mode
;;
;;      * run `checkdoc' periodically
;;
;;  Known issues
;;
;;  Emacs configuration file (.emacs) entry
;;
;;    (defconst xeona-elisp-directory
;;      (substitute-in-file-name
;;       "$HOME/$SYNK/xeona/svn2/futz/trunk/elisp/xeona.el"))
;;
;;    (if (file-readable-p xeona-elisp-directory)
;;        (load-file xeona-elisp-directory)
;;      (message "  robbie: WARNING: cannot load %s" xeona-elisp-directory))

;;; History:

;;      * see the subversion log messages

;;; Code:

;; ---------------------------------
;;  file associations
;; ---------------------------------

;; the \' matches the end of a string, where as $ matches the
;; empty string before a newline -- thus $ may lead to unexpected
;; behavior when dealing with filenames containing newlines

(setq auto-mode-alist (cons '("\\.xem\\'" . xem-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xog\\'" . xog-mode) auto-mode-alist))

;; ------------------------------------------------------------------------
;;  overarching support for Xem and Xog major modes
;; ------------------------------------------------------------------------

;; list of files to load -- the 'dev-tests' are strictly for testing and can be omitted

(defconst xeona-file-stubs
  '("xem" "xem-1" "xem-2" "xem-3" "xem-4" "xog" "xrstat" "xumber" "dev-tests")
  "List of file stubs to load on startup.")

;; our common location

(defconst xeona-file-stem
  (file-name-directory xeona-elisp-directory)               ; how we got here!
  "Current directory.")

;; load everything

(defun xeona-load-files (filestubs)
  "Load all FILESTUBS."
  (let* ((stub   nil )
         (loops  0   ))
    (dolist (stub filestubs)
      (incf loops)
      (setq file (concat xeona-file-stem stub ".el"))  ; the ".el" is required
      (load-file file))                                ; this call reports to '*Messages*'
    (message "  xeona: load-files: should have just loaded %d files" loops) ))

;; actual call

(xeona-load-files xeona-file-stubs)

;; ------------------------------------------------------------------------
;;  overarching support for Xem and Xog major modes
;; ------------------------------------------------------------------------

;; the two hook functions can be moved back to the user's
;; configuration file and fleshed out if need be

;; Xem mode

(add-hook 'xem-mode-hook 'xem-mode-customization)

(defun xem-mode-customization ()
  "Xem mode customization."
  (message "  robbie: xem-mode-customization: complete (also hollow)"))

;; Xog mode

(add-hook 'xog-mode-hook 'xog-mode-customization)

(defun xog-mode-customization ()
  "Xog mode customization."
  (message "  robbie: xog-mode-customization: complete (also hollow)"))

;; ------------------------------------------------------------------------
;;  save non-visited buffers
;; ------------------------------------------------------------------------

(defconst xem-buffers-visit-alist
  '((xem-buffer-xeona-output     . ".xog")
    (xem-buffer-r-capture        . ".rog")
    (xem-buffer-dump-tar-capture . ".tog"))
  "Alist of buffer names and file extensions.")

(defun xem-buffers-visit-all ()
  "Save any buffers defined in `xem-buffers-save-all' which are currently open.

This call also destroys the original buffer names in the process."
  (interactive)
  (let* ((filename  (buffer-file-name nil)              )   ; current buffer
         (filestub  (file-name-sans-extension filename) )
         (buffers   xem-buffers-visit-alist             )
         (buffname  nil                                 )
         (savename  nil                                 )
         (loops     0                                   )
         (plural    "s"                                 ))
    (dolist (buffer buffers)
      (setq buffname (symbol-value (car buffer))) ; CAUTION: note function `symbol-value'
      (setq savename (concat filestub (cdr buffer)))
      (setq bf (get-buffer buffname))        ; non-nil if 'buffname' exists
      (when bf
        (incf loops)
        (set-buffer bf)                      ; make buffer current for editing
        (write-file savename)
        (rename-buffer buffname nil)         ; require uniqueness TOFIX: 19-Aug-2010: confirm action
        (message "  xem: buffers-visit-all: saved buffer '%s' as '%s'" buffname savename)))
    (if (= loops 1) (setq plural ""))
    (message "  xem: buffers-visit-all: complete with %d save%s" loops plural) ))

(defun xeona-list-buffers ()
  "Test function to slowly list `xeona-buffers-regex'."
  (interactive)
  (let* ((regexs  xeona-buffers-regexs )
         (loops   0                    )
         (plural  nil                  ))
    (dolist (regex regexs)
      (incf loops)
      (message "  xeona: list-buffers: buffer %02d regex '%s'" loops regex)
      (sit-for 2))                           ; wait some
    (if (= loops 1) (setq plural "") (setq plural "s"))
    (message "  xeona: list-buffers: complete with %d showing%s." loops plural)
    loops ))                                 ; expose potential return value

;; ------------------------------------------------------------------------
;;  across-mode buffer cycling
;; ------------------------------------------------------------------------

;; CAUTION: setting global key binds is not neighborly but it
;; does save providing a special minor mode for each buffer -
;; that said, this really should be revisited

(global-set-key [(        f3)] 'xeona-cycle-buffers)

;; CAUTION: function `xeona-regex-buffername' must be
;; parsed ahead of constant `xeona-buffers-regexs'

(defun xeona-regex-buffername (buffername)
  "Function to regexize a BUFFERNAME.

That is replace all \"*\" with \"\\*\"."
  (let* ((regex  (replace-regexp-in-string "\\*" "\\*" buffername nil t) ))     ; [1]
    (message "  xeona: regex-buffername: '%s' to '%s'." buffername regex)
    regex ))                                 ; CAUTION: expose return value
    ;; [1] CAUTION: t for 'fixedcase' in 'rep' is essential

;; the buffers 'xumber-sum-buffer' and 'xumber-history-buffer'
;; are not accessible from here but are instead duplicate
;; hardcoded -- upstream changes will not cause problems because these

(defconst xeona-buffers-regexs
  (list "\\.xem$"
        "\\.xog$"
        "\\.rog$"
        (xeona-regex-buffername xem-buffer-xeona-output)
        (xeona-regex-buffername xem-buffer-xeona-data-rules)
        (xeona-regex-buffername xem-buffer-xeona-usage)
        (xeona-regex-buffername xem-buffer-xem-diff)
        (xeona-regex-buffername xem-buffer-r-capture)
        (xeona-regex-buffername xem-buffer-dump-tar-capture)
        (xeona-regex-buffername xem-buffer-dump-review)
        (xeona-regex-buffername xem-buffer-bidsets-display)
        (xeona-regex-buffername "*xumber-tally*")
        (xeona-regex-buffername "*xumber-history*")
        (xeona-regex-buffername "*Shell Command Output*")   ; optional
        (xeona-regex-buffername "*Async Shell Command*")    ; optional
        (xeona-regex-buffername "*Process List*")           ; optional
        (xeona-regex-buffername "*Messages*")               ; optional
;;;     "\\*xumber-.*\\*"
        ) "Potentially open xeona buffers.")

(defun xeona-cycle-buffers ()
  "Cycle to next appropriate buffer and then maximize it.

This function also applies a darker background to guard models."
  (interactive)
  (let* ((guard-bgcolor  "beige"              )   ; subtle
         (guard-bgcolor  "sienna"             )   ; provides good text contrast
         (regexs         xeona-buffers-regexs )
         (rsize          (length regexs)      )
         (blist          (buffer-list)        )
         (bsize          (length blist)       )
         (bc             (current-buffer)     )   ; lisp buffer object
         (bf             nil                  )   ; lisp buffer object
         (regex          nil                  )
         (hit            nil                  ))  ; to pass `string-match' return upstream
    (message "  xeona: cycle-buffers: regexs %d, buffers %d" rsize bsize)
    (pop blist)                              ; discard the current buffer
    (while (and (setq bf (pop blist))        ; processed in order
                (dolist (regex regexs (null hit))
                  (if (string-match regex (buffer-name bf)) (setq hit t)))))
    (if bf                                   ; CAUTION: must protect against nil buffer
        (progn
          (bury-buffer bc)                   ; OPTIONAL: active = cycle, disable = toggle
          (switch-to-buffer bf)              ; switch
          (delete-other-windows)             ; make full-size
          ;; coloration conditional for guard files
          (if (string-match "\\.guard\\.xem$" (buffer-name bf))
              (set-background-color guard-bgcolor)
            (set-background-color "white"))
          (message nil)                      ; hack to resize minibuffer to one line
          (message "  xeona: cycle-buffers: next buffer found '%s'." (buffer-name bf)))
      (message "  xeona: cycle-buffers: no next buffer found.")) ))

;; CAUTION: you can only apply a background color to a frame in
;; emacs, you cannot do this at the granularity of windows or
;; buffers -- hence you need to cycle out of guard file buffers
;; using the `xeona-cycle-buffers' command if you want your
;; standard background color to return

;;; xeona.el ends here

;  $Id: xeona.el 7404 2011-09-28 13:11:20Z robbie $
;  end of file

