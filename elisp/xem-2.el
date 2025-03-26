;;; xem-2.el --- additional code 2 for 'xem.el'

;  file-purpose     : emacs additional code 2 for 'xem.el'
;  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
;  file-create-date : Tue 24-Aug-2010 09:52 UTC
;  file-status      : ongoing
;  file-keywords    : emacs xeona

;  $Revision: 7950 $
;  $Date: 2011-11-10 23:08:22 +0100 (Thu, 10 Nov 2011) $
;  $Author: robbie $
;  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/elisp/xem-2.el $

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
;;  dump to tar (sole topic of file)
;; ---------------------------------

(defconst xem-buffer-dump-tar-capture
  "*xem-dump-tar-capture*"
  "Buffer for tar output.")

(defconst xem-buffer-dump-readme-text
  "*xem-dump-readme-text*"
  "Buffer for readme file post-view.")

(defvar xem-dump-get-tag-history
  nil
  "Get history for `xem-dump'.")

(defun xem-dump-get-tag ()
  "Get tag string for `xem-dump'."
  ;; entered spaces get converted to "-"
  (let* ((prompt  "Enter a tarfile tag (may omit hyphen, can be null, spaces okay): " )
         (tag (read-string
               prompt                        ; prompt
               nil                           ; initial
               'xem-dump-get-tag-history     ; history list
               "") ))                        ; set default
    (setq tag (xem-string-trim tag))                             ; trim leading and trailing spaces
    (setq tag (replace-regexp-in-string "[[:blank:]]+" "-" tag)) ; replace remaining blanks
    (unless (or (string-match "^$" tag)      ; empty string
                (string-match "^-" tag))     ; hyphen offered
      (setq tag (concat "-" tag)))
    (message "  xem: dump-get-role: obtained tag '%s'" tag)
    tag ))                                   ; expose either empty or hyphen-lead string

(defvar xem-dump-get-role-history
  nil
  "Role history for `xem-dump'.")

(defun xem-dump-get-role ()
  "Get role string for `xem-dump'."
  (let* ((default  "(not given)"                                )
         (prompt  (concat "Enter a tar role [" default "] : ") )
         (role (read-string
                prompt                       ; prompt
                nil                          ; initial
                'xem-dump-get-role-history   ; history list
                default) ))                  ; set default
    (message "  xem: dump-get-tag: obtained role '%s'" role)
    role ))                                  ; expose non-empty string

;; based on `minibuffer-local-map'

(defvar xem-dump-get-readme-keymap
  '(keymap
    (menu-bar
     keymap
     (minibuf
      "Xdump"
      keymap
      (previous menu-item "Previous readme" previous-history-element :help "Previous readme")
      (next menu-item "Next readme" next-history-element :help "Next readme")
      (return menu-item "Finish" exit-minibuffer :help "Terminate input and exit minibuffer")
      (quit menu-item "Quit" abort-recursive-edit :help "Abort input and exit minibuffer")
                       "Xdump"))
    (10 . exit-minibuffer)                   ; ^J
;;;;(13 . exit-minibuffer)                   ; ^M or <RET>, disable because we want natural editing
    ( 7 . abort-recursive-edit)              ; ^G
    (C-tab . file-cache-minibuffer-complete)
    ( 9 . self-insert-command)
    (up . previous-history-element)
    (prior . previous-history-element)
    (down . next-history-element)
    (next . next-history-element)
    (27 keymap
        (114 . previous-matching-history-element)
        (115 . next-matching-history-element)
        (112 . previous-history-element)
        (110 . next-history-element)))
  "Custom minibuffer keymap for `xem-dump'.")

(defvar xem-dump-get-readme-history
  nil
  "Readme input history for `xem-dump'.")

(defun xem-dump-get-readme (readfilename)
  "Seek optional readme text and stream to READFILENAME file.

This function provides interface support for function `xem-dump'."
  (let* ((prompt-1  "Add additional readme text? "         )
         (prompt-2  "Enter readme text (C-j to finish):\n" )
         (readme    nil                                    )
         (code      nil                                    )
         (chars     0                                      ))
    (if (y-or-n-p prompt-1)
        (progn
          (setq readme
                (read-from-minibuffer
                 prompt-2                         ; prompt-string: defined above
                 nil                              ; initial-contents: note last history via M-p
                 xem-dump-get-readme-keymap       ; keymap: 'nil' means use `minibuffer-local-map'
                 nil                              ; read: 'nil' means no convert text to lisp objects
                 'xem-dump-get-readme-history     ; hist: history object
                 t                                ; inherit-input-method: 't' is inherit current state
                 ))
          (message "  xem: dump-get-readme: string:")
          (message "%s" readme)
          (setq chars (length readme))
          (setq ecall (concat "echo \"" readme "\" >> " readfilename)) ; CAUTION: soft-quotes essential
          (setq code (shell-command ecall))
          (setq ecall (concat "echo \"\n===\" >> "      readfilename)) ; CAUTION: soft-quotes essential
          (setq code (shell-command ecall))
          (message nil)                           ; hack to resize minibuffer to one line
          (message "  xem: dump-get-readme: echo call returned %d" code)
          (message "  xem: dump-get-readme: just wrote %d chars to file '%s'." chars readfilename))
      (message "  xem: dump-get-readme: user opt out."))
    chars ))

(defun xem-dump-get-htmls (dirname)
  "Provide a list of the html files in directory DIRNAME if required."

  ;; typical name : pro-235620_dc-1-014109-01.html
  ;; decode       : pro-HMS_ID-PID-CNT{.help}.html
  ;;                where HMS is the UTC timestamp, ID is the domain controller
  ;;                identifier, PID is the process id, and CNT is a counter
  ;; note         : the help files are naturally identical

  (interactive)
  (let* ((wildcard  (concat dirname "*.html")                      )
         (htmls     (file-expand-wildcards wildcard)               )  ; returns a list
         (prompt    nil                                            )
         (count     (length htmls)                                 )
         (prompt    (format "Including the %d html files? " count) )
         (retval    ()                                             ))
    (if (> count 0)
        (if (y-or-n-p prompt)
            (setq retval htmls)))
    (message "  xem: dump-get-htmls: ")
    retval ))                                ; expose return

(defun xem-dump-info (readfilename           ; file name
                      role                   ; string
                      dirname                ; absolute
                      filename)              ; absolute
  "Collate and record system info using supplied arguments and direct calls."
  ;; a nil plist value simply prints as "nil"
  (let* ((bf        (current-buffer)                  )     ; lisp buffer object
         (leafname  (file-name-nondirectory filename) )
         (slist     ()                                )     ; string list
         (sbuff     nil                               )     ; string buffer
         (chars     0                                 ))    ; number of chars of information
    ;; update information
    (xem-system-info)                        ; recharge property list container (plist)
    (xem-model-info bf)                      ; recharge property list container (plist)
    ;; create 'slist'
    (push ""                                                    slist)
    (push (format "xem-title  : %s" (get 'xem-modinf 'title  )) slist)
    (push (format "model-file : %s" leafname                  ) slist)
    (push (format "svnversion : %s" (get 'xem-sysinf 'svnver )) slist)
    (push (format "dump-role  : %s" role                      ) slist)
    (push (format "used-svn   : %s" (get 'xem-modinf 'usedsvn)) slist)
    (push (format "dirname    : %s" dirname                   ) slist)
    (push (format "timestamp  : %s" (get 'xem-sysinf 'zstamp )) slist)
    (push (format "dump-uuid  : %s" (get 'xem-sysinf 'uuid   )) slist)
    (push ""                                                    slist)
    (push (format "host       : %s" (get 'xem-sysinf 'host   )) slist)
    (push (format "system     : %s" (get 'xem-sysinf 'system )) slist)
    (push (format "user       : %s" (get 'xem-sysinf 'user   )) slist)
    (push (format "email      : %s" (get 'xem-sysinf 'email  )) slist)
    (push (format "timezone   : %s" (get 'xem-sysinf 'tz     )) slist)
    (push ""                                                    slist)
    (push (format "TERM       : %s" (get 'xem-sysinf 'TERM   )) slist)
    (push (format "USER       : %s" (get 'xem-sysinf 'USER   )) slist)
    (push (format "SYNK       : %s" (get 'xem-sysinf 'SYNK   )) slist)
    (push (format "XEONA      : %s" (get 'xem-sysinf 'XEONA  )) slist)
    (push (format "XEONAR     : %s" (get 'xem-sysinf 'XEONAR )) slist)
    (push ""                                                    slist)
    ;; unpack 'slist'
    (when slist
      (dolist (s (reverse slist))
        (setq sbuff (concat sbuff "\n" s)))                 ; CAUTION: 'car' not need on 's'
      (setq sbuff (substring sbuff (length "\n") nil)))     ; remove leading separator
    (message "%s" sbuff)                                    ; note quotes
    ;; write out 'slist'
    (setq chars (length sbuff))
    (setq ecall (concat "echo \"" sbuff "\" >> " readfilename)) ; CAUTION: soft-quotes are essential
    (setq code (shell-command ecall))
    (message nil)                            ; hack to resize minibuffer to one line
    ;; completion reporting
    (message "  xem: dump-info: complete.")
    (length slist) ))

(defun xem-dump ()
  "Dump model to tarball.  Interactive interface to various `xem-dump-*' utility functions.\n
See: `xem-dump-get-tag' `xem-dump-get-role' `xem-dump-get-readme' `xem-dump-get-htmls' `xem-dump-info'"
  ;; absolute names have the form "symbol*" whereas relative names do not
  (interactive)
  (let* ((filename* (buffer-file-name nil)               )  ; absolute name of current buffer
         (dirname*  (file-name-directory filename*)      )  ; base directory with trailing slash
         (filestub  (file-name-sans-extension filename*) )  ; absolute name without the ".xem"
         (fileext   (file-name-extension filename*)      )  ; should be "xem"
         (filename  (file-relative-name filename*)       )
         (dirname   (file-relative-name dirname*)        )
         (filestub  (file-relative-name filestub)        )
         (tarstub   nil                                  )
         (tarsdir   nil                                  )
         (tarname   nil                                  )
         (buffread   xem-buffer-dump-readme-text         )
         (readme    "readme.txt"                         )  ; readme file name
         (tarextz   "tgz"                                )  ; assumes --gzip
         (boname    xem-buffer-dump-tar-capture          )
         (bo        nil                                  )
         (readname  nil                                  )  ; readme file
         (lines     0                                    )  ; information lines
         (chars     0                                    )  ; readme chars
         (stubs     ()                                   )
         (htmls     ()                                   )  ; list of html files
         (words     nil                                  )
         (tag       ""                                   )  ; string used in file and directory names
         (role      ""                                   )  ; string used in xem file stanza
         (br        nil                                  )) ; optional readme file buffer
    ;; confirm the buffer visits a XEM file
    (unless (string-equal "xem" fileext)
      (error "  xem: dump: error: call must be run from a '.%s' buffer" fileext))
    ;; seek and check tag
    (message nil)                                      ; hack to resize minibuffer
    (setq tag (xem-dump-get-tag))                      ; callout with ask
    (setq tarstub (concat filestub tag))
    (setq tarsdir (concat tarstub "/"))                ; CAUTION: note trailing slash
    (setq tarname (concat tarstub "." tarextz))
    (if (file-exists-p tarname)
      (error "  xem: dump: error: tar file exits '%s'" tarname))
    (if (file-directory-p tarsdir)
      (error "  xem: dump: error: tar subdirectory exits '%s'" tarsdir))
    (make-directory tarsdir nil)
    ;; seek role
    (setq role (xem-dump-get-role))                    ; callout with ask
    ;; seek and save optional readme
;;; (setq readname (concat tarsdir readme))                 ; simple 'readme.txt'
    (setq readname (concat tarsdir filestub "." readme))    ; qualified 'test-00.readme.txt'
    (setq lines (xem-dump-info readname role           ; callout modifies readme
                               dirname* filename*))
    (setq chars (xem-dump-get-readme readname))        ; callout with ask, modifies readme
    ;; ask about including htmls if present
    (setq htmls (xem-dump-get-htmls dirname))          ; callout with asks
    ;; save logs
    (xem-buffers-visit-all)
    ;; identify and copy files
    (setq loops 0)
    (setq stubs (file-expand-wildcards (concat filestub ".*")))  ; returns a list
    (setq files (append stubs htmls))
    (dolist (file files)
      (incf loops)
      (copy-file file tarsdir                ; copy to directory is supported
                 1                           ; '1' is confirm overwrite
                 t                           ; 't' is retain modification
                 t))                         ; 't' is preserve ownerships
    (message "  xem: dump: copied %d files to '%s'" loops tarsdir)
    ;; confirm (this stanza could be improved)
    (get-buffer-create buffread)
    (set-buffer buffread)
    (insert-file-contents readname)      ; 'nil' means do not also visit
    (switch-to-buffer buffread)
    (delete-other-windows)
    (sit-for 2)
;;; could work with a wait for close (find-file readname) (rename-buffer buffread nil) (sit-for 30)
    ;; tar archive call
    (setq tcall (concat "tar --create --gzip --verbose --file " tarname " " tarstub))
    (message "  xem: dump: tar call '%s'" tcall)
    (setq bo (get-buffer-create boname))
    (setq code (shell-command tcall bo nil)) ; 'nil' means mingle stdout and stderr
    (sit-for 2)                              ; one is too short
    (cond
     ((= code   0) (message "  xem: dump: tar returned success"))
     ((= code   1) (message "  xem: dump: tar returned some files differ error %d" code))
     ((= code   2) (message "  xem: dump: tar returned fatal error %d" code))
     (t            (message "  xem: dump: tar returned subprocess error %d" code)))
    ;; make the tarball readonly
    (set-file-modes tarname #o440)           ; CAUTION: note the unusual octal syntax
    ;; delete branch (unfortunately this version of
    ;; `delete-directory' does not recurse, so the files need to
    ;; be deleted first
    (dolist (file files)
      (setq target (concat tarsdir (file-name-nondirectory file)))
      (delete-file target))
    (if (file-exists-p readname)
        (delete-file readname))
    (delete-directory tarstub)
    ;; report on tar file
    (setq fats (file-attributes tarname 'string))
    (message "  xem: dump: confirming tarfile '%s' with %d bytes" tarname (nth 7 fats))
    ;; completion reporting
    (message "\
  xem: dump: complete using:\n\
      tag     : '%s'\n\
      role    : '%s'\n\
      readme  : %d chars\n\
      tarfile : %s\n\
      tar     : %s"
             tag role chars tarname (xem-tar-interpret code)) ))

;  $Id: xem-2.el 7950 2011-11-10 22:08:22Z robbie $
;  end of file

