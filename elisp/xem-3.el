;;; xem-3.el --- additional code 3 for 'xem.el'

;  file-purpose     : emacs additional code 3 for 'xem.el'
;  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
;  file-create-date : Tue 24-Aug-2010 09:52 UTC
;  file-status      : ongoing
;  file-keywords    : emacs xeona

;  $Revision: 8356 $
;  $Date: 2011-12-06 19:24:51 +0100 (Tue, 06 Dec 2011) $
;  $Author: robbie $
;  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/elisp/xem-3.el $

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
;;  system information
;; ---------------------------------

(defun xem-system-info ()
  "Generate a plist of standard system information."

  ;; a property list container ("plist" for short) is a list of
  ;; paired elements -- here each of the pairs associates a
  ;; property name symbol with a string value (other possibilities
  ;; exist)
  ;;
  ;; function `getenv' returns 'nil' if ENVAR is undefined, "" if
  ;; set but null, and otherwise a meaningful string

  (let* ((xem-sysinf  ()                                                                     ) ; plist
         (tzstr       (car (cdr (current-time-zone)))                                        )
         (zformat      "%a %d-%b-%Y %H:%M UTC %z"                                            )
         (zstampstr   (format-time-string zformat (current-time) t)                          )
         (uuidstr     (replace-regexp-in-string "\n" "" (shell-command-to-string "uuidgen")) ))

    (put 'xem-sysinf  'email       user-mail-address    )   ; "robbie@actrix.co.nz"
    (put 'xem-sysinf  'host        (system-name)        )   ; "hinau.fb10.tu-berlin.de"
    (put 'xem-sysinf  'svnver      (xem-svnversion)     )   ; "0000:0000M" to "0000" or interpretation
    (put 'xem-sysinf  'system      system-configuration )   ; "x86_64-pc-linux-gnu" [1]
    (put 'xem-sysinf  'tz          tzstr                )   ; "CEST"
    (put 'xem-sysinf  'user        user-login-name      )   ; "robbie"
    (put 'xem-sysinf  'zstamp      zstampstr            )   ; [2]
    (put 'xem-sysinf  'uuid        uuidstr              )   ; 3331e6c1-9a9e-453f-abfd-163a9c3f93a6

    (put 'xem-sysinf  'SYNK        (getenv "SYNK")      )   ; "synk"
    (put 'xem-sysinf  'TERM        (getenv "TERM")      )   ; "dumb"
    (put 'xem-sysinf  'USER        (getenv "USER")      )   ; "robbie"
    (put 'xem-sysinf  'XEONA       (getenv "XEONA")     )   ; ".../xeona/svn2/futz/trunk/xeona1"
    (put 'xem-sysinf  'XEONAR      (getenv "XEONAR")    )   ; ".../xeona/svn2/futz/trunk/xeonar"

    ;; [1] note that variable 'system-type' holds a symbol
    ;; [2] format: Sun 04-Aug-2005 13:39 UTC +0000

    (message "  xem: system-info: complete.") ))

(defun xem-svnversion ()
  "Obtain current svn version and reinterpret if necessary."
  (interactive)
  (let* ((svncall    "svnversion"     )
         (xeona      (getenv "XEONA") )      ; may not be set
         (loops      0                )      ; nibble counter
         (nibble     nil              )      ; used in nibble exercise
         (redwcpath  nil              )      ; reduced subversion WC_PATH, a result of nibbling
         (value      nil              ))     ; generic value
    ;; confirm svnversion
    (setq code (shell-command (concat "which " svncall)))
    (if (not (= code 0))
        (error "  xem: subversion: error: utility not found '%s'" svncall))
    ;; nibble back 'xeona' to reveal working copy base (I could
    ;; not find a subversion call which could provide this
    ;; information)
    (setq xeona (replace-regexp-in-string "/$" "" xeona)) ; trim any trailing slash
    (setq nibble xeona)
    (when nibble
      (while (not (string-equal
                   "exported\n"              ; CAUTION: trailing newline match essential
                   (shell-command-to-string (concat svncall " " nibble))))
        (incf loops)
        (setq redwcpath nibble)
        (setq nibble (file-truename (concat nibble "/.."))) ; cute huh!
        ;; protection for bad programming
        (if (string-equal redwcpath "/")
            (error "  xem: subversion: error: root directory trip: loop count %d, redwcpath '%s'"
                   loops redwcpath))
        (if (>= loops 256)
            (error "  xem: subversion: error: loop count trip: loop count %d, redwcpath is '%s'"
                   loops redwcpath))))
    (message "\
  xem: svnversion: envar XEONA     '%s'\n\
                   reduced WC_PATH '%s'\n\
                   %d loops"
             xeona redwcpath loops)
    ;; call svnversion again
    (if redwcpath
        (setq svncall (concat svncall " " redwcpath))
      (setq svncall (concat svncall)))
    (setq value (shell-command-to-string svncall))     ; CAUTION: includes trailing newline
    (setq value (replace-regexp-in-string "\n" "" value))
    ;; reinterpret as required
    (cond ((string-equal value "")
           (setq value (concat "utility not found '" svncall "'")))
          ((string-equal value "exported")
           (setq value "envar 'XEONA' not set or call made from outside subversion working copy")))
    (message "  xem: svnversion: returning '%s' for '%s'." value redwcpath)
    value ))

;; ---------------------------------
;;  model information
;; ---------------------------------

;; general comment: the use of unqualified field names is great
;; practice -- it would be better to properly parse the model
;; file in the same way that the R scripts do

(defun xem-model-info (xembuf)
  "Interrogate the buffer XEMBUF for key data and generate a plist."
  ;; see also function `xem-system-info'
  (let* ((bfname        (buffer-name bf)  )  ; just for reporting
         (not-run-tag   "(model not run)" )
         (xem-modinf    ()                )  ; plist
         (temp          nil               ))
    ;; integrity check
    (if (string-match "\\.xem$" bfname)
        (message "  xem: model-info: commencing with data recovery from model '%s'" bfname)
      (error "  xem: model-info: error: given buffer inconsistent with model file '%s'" bfname))
    ;; continue
    (set-buffer bf)                          ; defensive programming, probably not needed
    ;; recover data and load plist
    (put 'xem-modinf 'title   (xem-value-get "r-title"  t)) ; 'program.r-processing.r-title'
    (setq temp (xem-value-get "used-svn" nil))              ; 'program.program-last-run.used-svn'
    (if (= (length temp) 0) (setq temp not-run-tag))        ; overwrite
    (put 'xem-modinf 'usedsvn temp)
    ;; completion reporting
    (message "  xem: model-info: complete.") ))

;; ---------------------------------
;;  review dumps
;; ---------------------------------

;; tar --extract --gzip --to-stdout --file="stub.tgz" "stub/readme.txt" | head -10

(defconst xem-buffer-dump-review
  "*xem-dump-review*"
  "Buffer for dump reviews.")

(defun xem-dump-summarize ()
  "Scan and summarize dump files."
  (interactive)
  (let* ((buffname   xem-buffer-dump-review          )
         (readhead   10                              )
         (readname   "readme.txt"                    )
         (tars       (file-expand-wildcards "*.tgz") )
         (loops      0                               )
         (leaf       nil                             )
         (timestamp  nil                             )
         (timelist   nil                             )
         (age        nil                             )
         (readme     nil                             )
         (plural     ""                              ))
    ;; create buffer
    (switch-to-buffer (get-buffer-create buffname))
    (delete-other-windows)
    ;; add line if needed
    (unless (= (point) (point-min))
      (insert "\n===\n\n"))
    ;; load
    (dolist (tar tars)
      (setq target (concat (file-name-sans-extension tar) "/" readname))
      (setq tcall (concat "tar --extract --gzip --to-stdout --file=" tar " " target))
      (setq tcall (concat tcall " | " "head --lines " (number-to-string readhead)))
      (setq readme (shell-command-to-string tcall))
      ;; check for tar output
      (if (string-match "^tar:" readme)
          (progn
            (insert "\n")
            (insert tcall)
            (insert readme)
            (insert "\n")
            (message "xem: dumps-summarize: tar call '%s' failed " tcall))
        (incf loops)
        (insert "\n")
        (setq leaf (file-name-sans-extension tar))
        (insert (format "%03d  %s\n" loops leaf))
        (insert "\n")
        (insert (xem-dump-extract "xem-title"   readme 'line t))
        (insert (xem-dump-extract "model-file"  readme 'line t))
        (insert (xem-dump-extract "svnversion"  readme 'line t))
        (insert (xem-dump-extract "used-svn"    readme 'line t))
        (insert (xem-dump-extract "dump-role"   readme 'line t))
        (insert (xem-dump-extract "timestamp"   readme 'line t))
        ;; age code
        (setq timestamp (xem-dump-extract "timestamp" readme 'value))
        (setq timelist (xem-time-decomp timestamp))
        (setq age (xem-time-delta timelist))
        (insert (format "dump-age   : %s\n" age))
        (insert "\n")))
    ;; summarize
    (insert "\n---\n\n")
    (if (= loops 1) (setq plural "") (setq plural "s"))
    (insert (format "%d dump file%s processed\n" loops plural))
    (insert "\n")
    (insert
     "tarbomb : tar -extract --verbose --verbose --gzip --strip-components=1 --overwrite --file\n")
    (insert
     "tarball : tar -extract --verbose --verbose --gzip --strip-components=0 --overwrite --file\n")
    (message "  xem: dumps summarize complete with %d readmes from %d dumps." loops (length tars))
    loops ))

;; good time code in: http://www.emacswiki.org/emacs/download/rmail-extras.el

;; ---------------------------------
;;  time functions
;; ---------------------------------

(defun xem-time-decomp (timestamp)
  "Turn the formatted TIMESTAMP into a standard time value.

The TIMESTAMP is assumed to be in the form \"%a %d-%b-%Y %H:%M UTC +0000\".
This code uses `encode-time' and the resulting list ranges from seconds to year.
Here are some representative intermediate and final results:

   timestamp 'Sat 21-Aug-2010 15:04 UTC +0000'
   decomp '(Sat 21 Aug 2010 15 04 UTC +0000)'
   split '00 04 15 21 08 2010'
   encode '(19567 60128)'"
  (let* ((sep        "[ :-]"                        )  ; CAUTION: "-" at end, else range operator
         (decomp     (split-string timestamp sep t) )
         (day        (nth 1 decomp)                 )
         (monstr     (nth 2 decomp)                 )
         (year       (nth 3 decomp)                 )
         (hour       (nth 4 decomp)                 )
         (min        (nth 5 decomp)                 )
         (sec        "00"                           )
         (zonestr    (nth 6 decomp)                 )
         (offsetstr  (nth 7 decomp)                 )  ; no used
         (monthmap   '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03") ("Apr" . "04")
                       ("May" . "05") ("Jun" . "06") ("Jul" . "07") ("Aug" . "08")
                       ("Sep" . "09") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))
         (mon        nil                            )
         (ret        nil                            ))
    (if (not (string-equal zonestr "UTC"))
        (error "  xem: time-decomp: error: timestamp not UTC: '%s'" zonestr))
    (setq mon (cdr (assoc monstr monthmap)))
    (message "  xem: time-decomp: timestamp '%s'" timestamp)
    (message "  xem: time-decomp: decomp '%s'" decomp)
    (message "  xem: time-decomp: split '%s %s %s %s %s %s'" sec min hour day mon year)
    (setq day  (string-to-number day ))
    (setq mon  (string-to-number mon ))
    (setq year (string-to-number year))
    (setq hour (string-to-number hour))
    (setq min  (string-to-number min ))
    (setq sec  (string-to-number sec ))
    (setq ret (encode-time sec min hour day mon year t))    ; CAUTION: t is essential for UTC
    (message "  xem: time-decomp: encode '%s'" ret)
    (message "  xem: time decomp complete.")
    ret ))                                   ; expose return value

(defun xem-time-delta (timelist)
  "Interpret the difference between TIMELIST and the current time.

It is assumed that TIMELIST is expressed as UTC and not in the local timezone.
An interpretation of \"3 days\" means 3 to 4 days."
  ;; CAUTION: this code developed and tested on a 64-bit machine,
  ;; more specifically the LP64 data model
  (let* ((now        nil )
         (was        nil )
         (epoch-now  nil )
         (epoch-was  nil )
         (epoch-dif  nil )
         (delta      nil )
         (msg        nil ))
    ;; establish time points
    (setq now (current-time))
    (setq was timelist)
    ;; convert to floating point seconds since UNIX epoch (starting 1970)
    (setq epoch-now (float-time now))
    (setq epoch-was (float-time was))
    (setq epoch-dif (- epoch-now epoch-was))
    ;; report
    (message "  xem: time-delta: epoch-now %s" epoch-now)
    (message "  xem: time-delta: epoch-was %s" epoch-was)
    (message "  xem: time-delta: epoch-dif %s" epoch-dif)
    ;; interpret
    (when t
      (setq delta (/ epoch-dif (* 60 60 24)))
      (setq delta (truncate delta))            ; convert to integer by rounding down
      (cond
       ((= delta 0) (setq msg "under one day"))
       ((= delta 1) (setq msg "one day"))
       (t           (setq msg (format "%d days" delta)))))
    (when (= delta 0)
      (setq delta (/ epoch-dif (* 60 60)))
      (setq delta (truncate delta))
      (cond
       ((= delta 0) (setq msg "under one hour"))
       ((= delta 1) (setq msg "one hour"))
       (t           (setq msg (format "%d hours" delta)))))
    (when (= delta 0)
      (setq delta (/ epoch-dif (* 60)))
      (setq delta (truncate delta))
      (cond
       ((= delta 0) (setq msg "under one minute"))
       ((= delta 1) (setq msg "one minute"))
       (t           (setq msg (format "%d minutes" delta)))))
    ;; completion reporting
    (message "  xem: time delta complete with interpretation = %s." msg)
    msg ))                                   ; expose return value

;; ---------------------------------
;;  utilities
;; ---------------------------------

(defun xem-dump-extract (key readtext type &optional newline)
  "Hunt for KEY in READTEXT, return TYPE, possibly with trailing NEWLINE.

Note TYPE in { 'line 'value } and NEWLINE in { t nil }."
  (let* (
         (val-trip  (concat "^" key "[[:blank:]]*:[[:blank:]]\\(.*\\)$") )
         (lin-trip  (concat "^\\(" key ".*\\)$")                         )
         (symname   (symbol-name type)                                   ))
    ;; select a 'trip' regex
    (cond
     ((equal type 'value) (setq trip val-trip))
     ((equal type 'line)  (setq trip lin-trip)))
    ;; attempt match
    (if (string-match trip readtext)
        (progn
          (setq ret (match-string 1 readtext))
          (message "  xem: dumps-extract: key '%s' yielded match '%s'" key ret)
          ;; add newline if requested
          (if newline
              (progn
                (setq ret (concat ret "\n"))
                (message "  xem: dumps-extract: newline added")))
          (message "  xem: dumps extract successful for key '%s' and trip regex '%s'." key trip))
      (setq ret "")
      (message "  xem: dumps extract failed for key '%s' and trip regex '%s'" key trip))
    ret ))                                   ; expose return value

;; ---------------------------------
;;  tarball functions
;; ---------------------------------

(defun xem-dump-unpack (dumpfile*)           ; the trailing * indicates a (probable) absolute name
  "Unpack dump file DUMPFILE* (the star is part of the symbol identifier).

This function first asks for a '.tgz' file, which is tested.  And then
whether to \"extract into subdir or bomb\".  Extract into subdir is the
safer option.  If bomb is chosen and this function unpacks a model file
which is also open, then that buffer will be reverted."
  (interactive "fSelect a '.tgz' file (tested): ")
  (let* ((ext       (file-name-extension dumpfile*)            )
         (dumpfile  (file-name-nondirectory dumpfile*)         )
         (ref       "tgz"                                      )
         (prompt    "Extract into subdir (yes) or bomb (no)? " )
         (tcall     nil                                        )
         (level     nil                                        )
         (code      nil                                        )
         (msg       nil                                        ))
    (unless (string-equal ext ref)
      (error "  xem: dump-unpack: extension '.%s' must be '.%s', abandoning without action" ext ref))
    (if (y-or-n-p prompt)
        (setq level 0)
      (setq level 1))
    (setq tcall (format "tar --extract --verbose --verbose --gzip \
--strip-components=%d --overwrite --file %s" level dumpfile))
    (setq code (shell-command tcall nil t))
    ;; competion reporting
    (message "\
  xem: dump unpack tar call '%s'\n\
       tar  %s" tcall (xem-tar-interpret code)) ))

;; ---------------------------------
;;  clean functions
;; ---------------------------------

(defun xem-dump-clean-buffer ()
  "Prompted en-masse clean up of files (not subdirs) related to current buffer.

This function is a wrapper to `xem-dump-clean'."
  (interactive)
  (let* ((filename*  (buffer-file-name nil)))               ; absolute name of current buffer
         (xem-dump-clean filename*) ))                      ; lack of 'message' intentional

(defun xem-dump-clean (refmodel)
  "Prompted en-masse clean up of files (but not subdirectories) related to REFMODEL."
  (let* ((filename* refmodel                             )  ; should be absolute (not tested)
         (dirname*  (file-name-directory filename*)      )  ; base directory with trailing slash
         (filestub  (file-name-sans-extension filename*) )  ; absolute name without the ".xem"
         (fileext   (file-name-extension filename*)      )  ; should be "xem"
         (filename  (file-relative-name filename*)       )
         (dirname   (file-relative-name dirname*)        )
         (filestub  (file-relative-name filestub)        )
         (skips     "\\.xem\\'\\|.tgz\\'"                )  ; file patterns to keep
         (pattern   nil                                  )  ; file match wildcard
         (matches   ()                                   )  ; list of files which match
         (deletes   ()                                   )  ; list of files to be deleted
         (count     nil                                  )  ; number of deletes
         (prompt    nil                                  )  ; dynamic prompt for `y-or-n-p'
         (loops     0                                    )) ; delete file counter
    ;; confirm the buffer visits a XEM file
    (unless (string-equal "xem" fileext)
      (error "  xem: dump-clean: error: call must be run from a '.%s' buffer" fileext))
    (setq pattern (concat filestub "*"))
    (setq matches (file-expand-wildcards pattern nil))      ; 'nil' for relative paths
    ;; exclude some
    (dolist (file matches)
      (unless (string-match skips file)                     ; note the "\'" end-of-buffer operator
        (push file deletes)))
    (message "  xem: dump-clean: pattern '%s' yields %d matches and %d deletes"
             pattern (length matches) (length deletes))
    (setq count (length deletes))
    (cond
     ((< count 0)
      (error "  xem: dump-clean: error: coding issue, encountered negative count %d" count))
     ((= count 0)
      (message "  xem: dump clean found %d matches, exiting without action." count))
     ((> count 0)
      (setq prompt (format "Pattern '%s' matched %d files, continue? " pattern count))
      (if (y-or-n-p prompt)
          (progn
            (dolist (file deletes)
              (incf loops)
              (delete-file file))
            (message "  xem: dump clean compete with %s deleted files." loops))
     (message "  xem: dump clean abandoned task without action." loops)))) ))

;; ---------------------------------
;;  r-vectorization
;; ---------------------------------

(defalias 'recline 'xem-r-vector-line)       ; TOFIX: 06-Dec-2011: for development purposes

(defun xem-r-vectorize (identifier,          ; identifier
                        timeseries)          ; space-separated string of numbers
  "R vectorize given TIMESERIES and assign it to IDENTIFIER."
  (if (not (stringp identifier)) (error "identifier not string type: %s" identifier))
  (if (not (stringp timeseries)) (error "timeseries not string type: %s" timeseries))
  (let* ((csvdata  (replace-regexp-in-string " " ", " timeseries) )
         (buff     (concat identifier " <- " "c(" csvdata ")")    ))  ; R assignment
    (message "  xem: r-vectorize: complete.")
    buff ))                                  ; expose return value

(defun xem-r-vector-line (identifier)
  "Capture current line, R vectorize, add IDENTIFIER, and place on kill ring.

Any whitespace within the IDENTIFIER is transformed into '.' dot characters.
Under R, a scalar is simply a single element vector, this call should work with
scalars too."
  (interactive "sEnter identifier for use with R: ")
  (let* ((idrep       "."                     )   ; character to replace any whitespace in 'identifier'
         (angles      "<>"                    )   ; key/value separator: out-data and in-data
         (regex       (concat "[" angles "]") )
         (line        nil                     )
         (parts       nil                     )
         (left        nil                     )
         (right       nil                     )
         (orig        nil                     )
         (data        nil                     )
         (buff        nil                     ))  ; buffer to be added to the kill ring
    ;; initial reporting
    (message "  xem-r-vector-line: commencing with identifier '%s'" identifier)
    ;; capture
    (setq line (thing-at-point 'line))
    ;; integrity check for in-data or out-data
    (if (not (string-match regex line))
        (message "  xem: r-vector-line: no '%s' trip in this line, giving up." angles)
      (progn
        ;; reprocess identifier
        (setq identifier (replace-regexp-in-string "[[:blank:]]+" idrep identifier))      ; substitute any whitespace
        ;; split field
        (setq parts (split-string line regex nil))                         ; 'nil' is accept null substrings
        (setq left  (nth 0 parts))
        (setq right (nth 1 parts))                                         ; could be nil
        (setq right (xem-string-trim right))                               ; could be an empty string
        (setq orig right)                                                  ; for completion reporting
        (setq right (replace-regexp-in-string "\\.\\.$" "" right))         ; trim any trailing double dots
        (setq right (xem-string-trim right))
        ;; process right : "+1.00e+08 +5.00e+07 +1.00e+08 +5.00e+07 +1.00e+08"
        (setq data right)
        ;; R-ize
        (setq buff (xem-r-vectorize identifier data))
        ;; add to kill ring
        (kill-new buff)                                                    ; add 'buff' as string to the kill ring
        (message "  xem: r-vector-line: '%s' added to kill ring" identifier))) ))

;; ---------------------------------
;;  one line plot
;; ---------------------------------

(defvar xem-oneplot-window t "'t' for plot window, 'nil' for PDF")

(defun xem-oneplot-window-toggle ()
  "Toggle boolean variable `xem-oneplot-window'."
  (interactive)
  (if xem-oneplot-window
      (progn
        (setq xem-oneplot-window nil)
        (message "  xem: oneplot-window-toggle: write plot to PDF and then view"))
    (setq xem-oneplot-window t)
    (message "  xem: oneplot-window-toggle: display plot on screen")) )

;; the 'xem-oneplot-line' function should ideally check it is
;; indeed operating on numbers

(defun xem-oneplot-line (trunc)
  "Plot the current XEM line using R.

Calls function `xem-oneplot' for the preparation of an R script and then
the R call.  Also produces a temporary PDF as a side-effect, which can be
saved under another name for future reference.

A typical data line might be:

    actual-productions [W] F    < +1.00e+08 +5.00e+07 +1.00e+08 +5.00e+07 +1.00e+08"
  (interactive "P")                          ; can take a prefix command argument
  (let* ((angles "<>"                    )   ; key/value separator: out-data and in-data
         (regex  (concat "[" angles "]") )
         (nanex  " +nan \\| -nan "       )   ; NaN matching regex
         (line   nil                     )   ; captured line (with trailing newline)
         (parts  ()                      )   ; for left/right split
         (left   nil                     )   ; left part
         (right  nil                     )   ; right part
         (orig   nil                     )   ; original right for reporting purposes
         (lparts ()                      )   ; left part split
         (fid    nil                     )   ; field id
         (rid    nil                     )   ; record id
         (title  ""                      )   ; plot title
         (units  ""                      )   ; plot units
         (type   ""                      )   ; field type, usually F
         (ret    nil                     )
         (nanmsg ""                      ))  ; used in final reporting
    ;; initial reporting
    (message "  xem-oneplot-line: commencing with prefix argument: %s" trunc)     ; used to truncate
    ;; capture
    (setq line (thing-at-point 'line))
    ;; integrity check for in-data or out-data
    (if (not (string-match regex line))
        (message "  xem: oneplot-line: no '%s' trip in this line, giving up." angles)
      (progn
        ;; process record id first
        (setq rid (xem-get-current-record-id))                             ; "entity.teas-windfarm-1"
        (message "  xem-oneplot-line: here 1: rid: %s" rid)
        (if rid
            (setq rid (replace-regexp-in-string "^entity\\." "" rid)))     ; strip record kind
        (message "  xem-oneplot-line: here 2: rid: %s" rid)
        ;; split field
        (setq parts (split-string line regex nil))                         ; 'nil' is accept null substrings
        (setq left  (nth 0 parts))
        (setq right (nth 1 parts))                                         ; could be nil
        (setq right (xem-string-trim right))                               ; could be an empty string
        (setq orig right)                                                  ; for completion reporting
        (setq right (replace-regexp-in-string "\\.\\.$" "" right))         ; trim any trailing double dots
        (setq right (xem-string-trim right))
        ;; process left : "actual-productions [W] F"
        (setq lparts (split-string left))
        (setq fid (nth 0 lparts))                                          ; "actual-productions"
        (if rid
            (setq title (concat rid " . " fid))                            ; create full plot title
          (setq title fid))                                                ; create partial plot title
;;;     (setq title (replace-regexp-in-string "-" " " title))              ; substitute any dashes
        (setq units (nth 1 lparts))                                        ; "[W]"
        (setq type  (nth 2 lparts))                                        ; "F" etc
        ;; process right : "+1.00e+08 +5.00e+07 +1.00e+08 +5.00e+07 +1.00e+08"
        (setq data right)
        ;; check for NaNs and overwrite 'nanmsg' string
        (if (string-match nanex data)
            (setq nanmsg " but NaNs detected (check stderr in *Shell Command Output*)"))
        ;; integrity check
        (if (= (length right) 0)                                           ; no data, no call
            (message "  xem: oneplot-line: failed as no data: %s." orig )
          ;; truncate data
          (if trunc                                                        ; 'trunc' can only be integral
              (setq data (xem-truncate-timeseries-string data trunc)))
          ;; plot call and completion reporting
          (xem-oneplot title units data nil)                               ; key call
;;;       (message "  xem-oneplot-line: using: %s" orig)                   ; often too long for normal reporting
          (message "  xem: oneplot-line complete%s." nanmsg)))) ))

(defun xem-truncate-timeseries-string (tsstring trunk)
  "Truncate space-separated timeseries TSSTRING to length TRUNK if possible."
  (let* ((splif    " "                             )   ; split regex (yes it is a regular expression)
         (sep      " "                             )
         (buffer   (split-string tsstring splif t) )   ; split to list, 't' means omit nulls
         (buflen   (length buffer)                 )
         (remove   (- buflen trunk)                )
         (trunked  nil                             ))
    (if (> buflen trunk)
        (progn
          (nbutlast buffer remove)                          ; chop list tail
          (setq trunked (mapconcat 'identity buffer sep))   ; serialize, no reversal needed
          (message "  xem-truncate-timeseries-string: complete, truncated by %d to %d" remove trunk)
          trunked)                           ; expose modified string
      (message "  xem-truncate-timeseries-string: complete, nothing done")
      tsstring) ))                           ; expose original string

(defun xem-oneplot (title                    ; string without dashes
                    units                    ; complete with brackets
                    data                     ; space-separated string of numbers
                    &optional pdftag)
  "Plot a oneline plot to support public function `xem-oneplot-line'.

The x-axis is zero-based, meaning the first step is notated zero."
  ;; odd bug in that 'xem-svn' became 'nil' during invocation
  (let* ((plot-window  xem-oneplot-window      )  ; controls plot type
         (rutil        "Rscript --vanilla"     )  ; 'Rscript' utility with options
         (ext          "r"                     )  ; batch script extension for R
         (ptype        "o"                     )  ; plot type, "o" = over-plot
         (ff           "%+.4g"                 )  ; float format specifier
         (svn          xem-svn                 )  ; copy over global variable, treat as integer
         (pdfviewer    "evince"                )  ; PDF viewer
         (PDFNAME      "Rplots.pdf"            )  ; CAUTION: default PDF name from R, do not change
         (short        ()                      )  ; possibly truncated 'data' for reporting
         (scriptname   nil                     )  ; name of generated script
         (csvdata      nil                     )  ; 'data' but now with commas
         (ftime        nil                     )  ; time format string
         (pargs        nil                     )  ; plot arguments string
         (targs        nil                     )  ; title arguments string
         (sargs        nil                     )  ; sprintf string
         (lines        ()                      )  ; script in list form
         (script       ""                      )  ; script in string form
         (call         nil                     )
         (msg          nil                     )
         (msg-1        nil                     )
         (msg-2        nil                     )
         (msg-3        nil                     )
         (bfname      (buffer-name nil)        )) ; just for reporting
    ;; initial reporting
    (if plot-window
        (message "  xem-oneplot: will open plot window")
      (message "  xem-oneplot: will produce a PDF and then view that"))
    (message "  xem-oneplot: title: '%s'" title)
    (message "  xem-oneplot: units: '%s'" units)
    (message "  xem-oneplot: data length %d" (length data))
    (if (> (length data) 59)
        (setq short (substring data 0 59))        ; from (includes) to (exclusive)
      (setq short data))
    (if data
        (message "  xem-oneplot: shortened data: %s" short)
      (message "  xem-oneplot: 'data' is null"))
    ;; generate a script name
    (setq scriptname (concat "temp" "." ext))     ; TOFIX: 18-Feb-2011: work up filenaming in due course
    ;; compose meta-data message
    (setq msg-1 (concat "\"model : \", model"))
    (if xem-svn
        (setq msg-2 (format "%s %d" "\" | svn : \"," xem-svn))
      (setq msg-2 (format "%s" "\" | svn : NA\"")))    ; "NA" if 'xem-svn' not set (that seems reasonable)
    (setq msg-3 (concat "\" | date : \", utcstamp"))
    (setq msg   (concat msg-1 ", " msg-2 ", " msg-3))
    (message "  xem-oneplot: msg: %s" msg)
    ;; preliminary matters
    (setq csvdata (replace-regexp-in-string " " ", " data))
    (setq ftime "format(as.POSIXlt(Sys.time(), tz = \"GMT\"), \"%d-%b-%Y %H:%M UTC\")")
    (setq pargs (concat "base, data, ylim = ylim, ann = FALSE, type = \"" ptype "\""))
    (setq targs "main = main, xlab = xlab, ylab = ylab")
    (setq sargs (concat "\"mean = " ff " / min = " ff " / max = " ff " / length = %d\"" ", mean, min, max, length"))
    ;; assemble the script
    (push (concat "# automatically generated R batch script"                ) lines)
    (push (concat "# stand-alone run: $ " rutil " " scriptname              ) lines)
    (push (concat "# preamble"                                              ) lines)
    (push (concat "script   <- \"" scriptname "\""                          ) lines)
    (push (concat "utcstamp <- " ftime                                      ) lines)
    (push (concat "model    <- \"" bfname "\""                              ) lines)
    (push (concat "msg  <- paste(" msg ")"                                  ) lines)
    (push (concat "main <- \""  title         "\""                          ) lines)
    (push (concat "xlab <- \"" "interval"     "\""                          ) lines)
    (push (concat "ylab <- \"" "units " units "\""                          ) lines)
    (push (concat "# data"                                                  ) lines)
    (push (concat "data <- c(" csvdata   ")"                                ) lines)

    (push (concat "mean   <- mean(data)"                                    ) lines)
    (push (concat "min    <- min(data)"                                     ) lines)
    (push (concat "max    <- max(data)"                                     ) lines)
    (push (concat "length <- length(data)"                                  ) lines)
    (push (concat "sum    <- sprintf(" sargs ")"                            ) lines)
    (if plot-window
        (progn
          (push "# hold-open functionality"                                   lines)
          (push "library(tcltk)  # for message box"                           lines)
          (push "x11()           # disable line to produce a PDF instead"     lines)))
    (push (concat "# plotting commands"                                     ) lines)
    (push (concat "par(col.main = \"darkslategray\")"                       ) lines)
    (push (concat "ymin <- min"                                             ) lines)
    (push (concat "ymax <- max"                                             ) lines)
    (push (concat "if ( ymin > 0.0 ) ymin <- 0.0"                           ) lines)
    (push (concat "ylim <- c(ymin, ymax)"                                   ) lines)
    (push (concat "base <- 0:(length(data) - 1)"                            ) lines)
    (push (concat "plot(" pargs ")"                                         ) lines)
    (push (concat "grid()  # disable to remove grid"                        ) lines)
    (push (concat "title(" targs ", line = 2.0)"                            ) lines)
    (push (concat "abline(h = mean, col = \"gray\")"                        ) lines)
    (push (concat "mtext(msg, side = 4, adj = 0.05, cex = 0.75)"            ) lines)
    (push (concat "mtext(sum, side = 3, line = 0.5, cex = 0.85)"            ) lines)
    (if plot-window
        (progn
          (push "# hold-open functionality"                                   lines)
          (push "prompt  <- \"hit spacebar to close plot\""                   lines)
          (push (concat "extra   <- \"R source is in file '" scriptname "'\"")lines)
          (push "capture <- tk_messageBox(message = prompt, detail = extra)"  lines)))
    (push (concat "# end of file"                                           ) lines)
    ;; stringify the script
    (setq script (xem-stringify-list lines "\n"))
    ; make and save buffer
    (set-buffer (generate-new-buffer (generate-new-buffer-name scriptname)))
    (insert script)                          ; simple insertion, no multi-byte issues
    (write-file scriptname nil)              ; 'nil' is do not prompt for overwrite
    (kill-buffer scriptname)                 ; returns 't' if killed
    ;; invoke the script
    (setq call (concat rutil " " scriptname))
    (message "  xem-oneplot: R call: %s" call)
    (shell-command call)         ;; no capture with this variant, no ampersand so run synchronously
    ;; process PDF
    (if (not plot-window)
        (progn
          (setq newname (xem-oneplot-format-pdfname bfname       ; visited file too
                                                    title        ; function argument
                                                    svn          ; global variable set in 'xem.el', copied over at beginning
                                                    pdftag))     ; optional function argument
          (rename-file PDFNAME newname 1)                        ; a number as third arg means confirm overwrite
          (set-file-modes newname #o440)                         ; CAUTION: note the unusual octal syntax
          (message "  xem-oneplot: PDF rename '%s' was '%s'" newname PDFNAME)
          (setq PDFNAME newname)
          (setq call (concat pdfviewer " " PDFNAME))
          (message "  xem-oneplot: PDF call: %s" call)
          (shell-command call)))
    ;; completion reporting
    (message "  xem-oneplot: Rscript ran") ))

(defun xem-oneplot-format-pdfname (xemfile
                                   title
                                   svn       ; 'nil' to omit component, else treat a integer
                                   &optional tag)
  "Generate and return a PDF name using XEMFILE, TITLE, SVN and optional TAG.

A typical result would be: 'test-00_asop-type-a00_demands.0000_tag.pdf'.
If the SVN is 'nil' then that component is omitted."
  (let* ((pdfext     ".pdf" )
         (xemstub    nil    )                ; say: "test-00"
         (plottitle  nil    )                ; say: "asop-type-a00_demands"
         (tags       nil    )                ; say: "0000_tag" or "0000" or "tag" or ""
         (dot        nil    )
         (usc        nil    )
         (pdfname    nil    ))               ; return value
    (if svn
        (if (not (integerp svn))
            (error "  xem-oneplot-format-pdfname: 'svn' argument failed integer test")))
    (message
     "  xem-oneplot-format-pdfname: commencing: xemfile '%s' title '%s' svn '%d' tag '%s'"
     xemfile title svn tag)
    (setq xemstub (file-name-sans-extension xemfile))
    (setq plottitle (replace-regexp-in-string " \\. " "_" title))
    (setq plottitle (concat "_" plottitle))
    (if (or  svn tag) (setq dot "."))
    (if (and svn tag) (setq usc "_"))
    (setq tags (concat dot (format "%d" svn) usc tag))
    (setq pdfname (concat xemstub plottitle tags ".pdf"))
    (message "  xem-oneplot-format-pdfname: complete, PDF name '%s'" pdfname)
    pdfname ))

;; ---------------------------------
;;  time step toggle
;; ---------------------------------

;; hours : 168 = one week, 8760 = one year

(defconst xem-toggle-steps-lo    6 "Xem toggle steps lo value.")      ; as per the 'TimeHorizon' XEDOC
(defconst xem-toggle-steps-hi 8760 "Xem toggle steps hi value.")      ; changes as required

(defun xem-toggle-steps (arg)
  "Semi-intelligent toggle for TimeHorizon 'steps' value.\n
Uses `xem-toggle-data' and predefined values for lo (6) and hi (8760).
Check the constant `xem-toggle-steps-hi' because it may change during testing.
A prefix argument will change the value of 'hi' to the value given."
  (interactive "P")
  (save-excursion
    (let* ((lo                xem-toggle-steps-lo     )     ; low toggle
           (hi                xem-toggle-steps-hi     )     ; high toggle
           (entity-id         "entity.time-horizon"   )     ; hardcoded in 'xeona'
           (field-id          "steps"                 )     ; hardcoded in 'xeona'
           (lineno-max        20                      )     ; restrict to n following lines (usually 6 or 9 basic)
           (regex-1           nil                     )     ; entity identifier regex
           (regex-2           nil                     )     ; field key regex
           (regex-3           nil                     )     ; field key regex WITH leading #
           (lineno-1          nil                     )     ; 'TimeHorizon' identifier line number
           (word              nil                     )
           (number            nil                     )
           (prompt            "(not overwritten)"     )
           (input             nil                     )
           (hxlist '("2" "6" "24" "48" "168" "672" "4380" "8760" "17520" "43800")))
      ;; report prefix arg
      (if arg
          (message "  xem: toggle-steps: prefix arg %d" arg)
        (message "  xem: toggle-steps: no prefix arg given"))
      ;; change 'hi' accordingly
      (if arg
          (progn
            (setq hi arg)
            (message "  xem: toggle-steps: because of prefix arg, hi value reset to %d" hi)))
      ;; go to beginning of accessible portion of buffer
      (goto-char (point-min))
      ;; search for 'TimeHorizon' entity
      (setq regex-1 (concat "^" entity-id "$"))   ; sole term on line
      ;; [1] set point at end of occurrence and also return value, 'nil' is search bound, non-nil means return 'nil' not error
      (if (setq start (re-search-forward regex-1 nil t))                             ; [1]
          (progn
            (setq lineno-1 (line-number-at-pos nil))
            ;; FIRST: search for active "steps" field id
            (setq regex-2 (concat "^[[:blank:]]+" field-id ".*" ">"))                     ; active in-data
            (if (re-search-forward regex-2 nil t)
                (progn
                  (if (<= (- (line-number-at-pos nil) lineno-1) lineno-max)
                      (progn
                        (beginning-of-line)
                        (forward-line +1)
                        (setq regex-3 (concat "^[[:blank:]]+" "# " field-id ".*" ">"))    ; disabled in-data
                        (setq line (thing-at-point 'line))
                        (if (string-match regex-3 line)
                            (progn
                              ;; SECOND: simply toggle up existing disabled data
                              (xem-toggle-data)
                              (forward-char -1)
                              (forward-word -1)
                              (setq word (thing-at-point 'word))
                              (setq number (string-to-number word))                           ; returns zero on problem
                              ;; prompted save if required (current buffer assumed here)
                              (if (buffer-modified-p)
                                  (if (y-or-n-p (format "Save this model because the steps have now toggled up to %d? " number))
                                      (save-buffer)))
                              (message "  xem: toggle-steps: complete, simply toggled up existing disabled data, now %d." number))
                          ;; THIRD: invoke toggle and change value as required (this code is macro-like)
                          (forward-line -1)                                               ; retreat
                          (xem-toggle-data)
                          (forward-char -1)
                          (forward-word -1)
                          (setq word (thing-at-point 'word))
                          (setq number (string-to-number word))                           ; returns zero on problem
                          (message "  xem-toggle-steps: recovered number %d" number)
                          (cond
                           ((= number hi) (setq number lo) (message "  xem-toggle-steps: toggled to lo %d" number))
                           ((= number lo) (setq number hi) (message "  xem-toggle-steps: toggled to hi %d" number))
                           ((= number 0)                   (message "  xem-toggle-steps: problem zero number %d" number))
                           (t
                            ;; FOURTH: seek user input
                            (message "  xem-toggle-steps: no overwrite for %d" number)
                            (setq prompt (format "current value %d cannot be toggled, insert a new value (or scroll up): " number))
                            (setq input
                                  (read-string
                                   prompt                             ; prompt
                                   (pop hxlist)                       ; initial
                                   'hxlist                            ; history list
                                   (format "%d" number)))             ; set default
                            (setq number (string-to-number input))))
                          (kill-line)                                                     ; delete remainder of line
                          (insert (format "%d" number))                                   ; insert altered number
                          ;; prompted save if required (current buffer assumed here)
                          (if (buffer-modified-p)
                              (if (y-or-n-p (format "Save this model because the steps have now changed to %d? " number))
                                  (save-buffer)))
                          ;;final reporting
                          (message "  xem: toggle-steps: complete, toggled sole data field and activated %d." number) ))
                    ;; non-alteration cases
                    (message "xem: toggle-steps: no action because TimeHorizon field '%s' not within %d lines." field-id lineno-max)))
              (message "xem: toggle-steps: no action because TimeHorizon field '%s' not found." field-id)))
        (message "xem: toggle-steps: no action because TimeHorizon identifier '%s' not found." entity-id)) )))

;; ---------------------------------
;;  xrefs
;; ---------------------------------

(defun xem-open-xref ()
  "Open a subset of org-mode style external links, namely: file: http:.\n
The web bowser is determined by the variable `browse-url-browser-function'"
  (interactive)
  (let* ((pdfviewer   "evince"   )           ; PDF-viewer
         (line        nil        )
         (parts       ()         )
         (left        nil        )
         (right       nil        )
         (target      nil        )
         (extn        nil        )
         (call        nil        ))
    (setq line (thing-at-point 'line))
    (setq line (substring-no-properties line))         ; remove text properties including coloration
    (setq parts (split-string line ":" nil))           ; 'nil' is accept null substrings
    (setq left  (xem-string-trim (nth 0 parts)))
    (setq right (xem-string-trim (nth 1 parts)))       ; could be nil
    (setq extn  (file-name-extension right nil))       ; 'nil' means exclude period
    (message "  xem-open-xref: left:right: %s:%s" left right)
    ;; seek an identifying string: http file
    (cond
     ((string-equal left "http")                       ; web document
      (setq target (concat left  ":" right))
      (browse-url target)
      (message "  xem: open-xref: complete, using default browser on '%s'." target))
     ((string-equal left "file")                       ; file of some sort
      (cond
       ((string-equal extn "pdf")                      ; PDF file
        (setq call (concat pdfviewer " " right)))
       (t (message "  xem: open-xref: complete, file extension '%s' not currently supported." extn))))
      (t (message "  xem: open-xref: complete, no identifying string matched for '%s'." left)))
    (if call
        ;; invoke the call
        (progn
          (setq call (concat call " " "&"))            ; make asynchronous
          (message "  xem-open-xref: call: %s" call)
          (shell-command call)                         ; no capture with this variant
          (delete-other-windows)                       ; close the '*Async Shell Command*' buffer
          (message "  xem: open-xref: complete with call: %s." call))) ))

;; ---------------------------------
;;  GLPK solver resets
;; ---------------------------------

(defconst xem-glpk-params-list '("init-scale-problem"
                                 "init-use-advanced-initial-basis"
                                 "init-use-simplex-presolver"
                                 "init-use-mip-presolver"
                                 "init-apply-numerical-zero"
                                 "trip-kkt-report-level"
                                 "trip-coeff-span-level"))

(defun xem-glpk-params (input)
  "Update the GLPK parameters in the domain controllers using 0000000-style INPUT."
  (interactive "sEnter the seven GLPK params (example 1100112): ")
  (let* ((vals     (split-string input "" t)  )   ; CAUTION: the 't' is essential
         (lenvals  (length vals)              )   ; adaptive
         (keys     xem-glpk-params-list       )
         (val      nil                        )   ; new value
         (count    0                          )   ; replacement count, should be multiple of input
         (domcons  0.0                        )   ; number of domain controllers changed
         (plural   ""                         )   ; can contain a plural 's'
         (wmsg     ""                         ))  ; warning message
    ;; initial reporting
    (message "  xem: glpk-params: %s" input)
    ;; integrity check
    (if (not (= (length vals) (length keys)))
        (error "  xem-glpk-params: error: input '%s' is not %d long" input (length keys)))
    ;; active code
    (dolist (key keys)
      (setq val (pop vals))
      (incf count (xem-field-reset key ">" val)))
    (if (not (= (mod count lenvals) 0))      ; CAUTION: 'vals' is now fully popped
        (setq wmsg " / WARNING : NOT MULTIPLE OF INPUT"))
    (setq domcons (/ count lenvals))
    (if (not (= domcons 1)) (setq plural "s"))
    (message
     "  xem: glpk-params: complete with %g domain controller%s updated%s."
     domcons plural wmsg) ))                 ; "%g" is good

(defun xem-field-reset (key angle value)
  "Basic data reset for KEY using ANGLE and VALUE as string.
This function does NOT CHECK the enclosing entity identifier so be careful."
  (save-excursion
    (let* ((regex  (concat "^" " +" key " .*" angle " *"))
           (count  0))
      ;; initial reporting
      (message "  xem-data-reset: beginning with key angle value: %s %s %s" key angle value)
      ;; integrity check
      (if (null value)
          (error "  xem-data-reset: error: null value supplied for key '%s'" key))
      ;; active code, go to beginning of accessible portion of buffer
      (goto-char (point-min))
      ;; search for 'regex'
      (while (re-search-forward regex nil t) ; 't' is do not error
          (progn
            (kill-line)                      ; should not kill thru newline
            (insert value)
            (incf count)))                   ; increment
      (message "  xem-data-reset: complete with count: %d" count)
      count )))                              ; expose return

;; ---------------------------------
;;  HV transmission update
;; ---------------------------------

(defun xem-hv-params (input)
  "Update the discretization-steps parameter for the HV transmission entity using 0-style INPUT."
  (interactive "sEnter the one param (example 2): ")
  (let* ((vals     (split-string input "" t))     ; CAUTION: the 't' is essential
         (lenvals  (length vals))
         (keys     '("discretization-steps"))
         (val      nil)                           ; new value
         (count    0)                             ; replacement count, should be multiple of input
         (wmsg     ""))                           ; warning message
    ;; initial reporting
    (message "  xem: hv-params: %s" input)
    ;; integrity check
    (if (not (= (length vals) (length keys)))
        (error "  xem-hv-params: error: input '%s' is not %d long" input (length keys)))
    ;; active code
    (dolist (key keys)
      (setq val (pop vals))
      (incf count (xem-field-reset key ">" val)))
    (if (not (= (mod count lenvals) 0))       ; CAUTION: 'vals' is now fully popped
        (setq wmsg " / WARNING : NOT MULTIPLE OF INPUT"))
    (message "  xem: hv-params: compete with %d modifications%s." count wmsg) ))

;; ---------------------------------
;;  bidsets display
;; ---------------------------------

;; not the most elegant code but it provides well formatted output

(defconst xem-buffer-bidsets-display
  "*XEM-bidset-timeseries*"
  "Results buffer for `xem-bidsets-display'.")

(defun xem-bidsets-display-line ()
  "Display the bidsets on the current line in a new buffer."
  (interactive)
  (let* ((angles  "<>"                    )  ; key/value separator: out-data and in-data
         (regex   (concat "[" angles "]") )
         (bsssep  " / "                   )  ; used to separate multiple bidsets in one double-quoted string
         (line    nil                     )  ; captured line (with trailing newline)
         (parts   ()                      )  ; for left/right split
         (left    nil                     )  ; left part
         (right   nil                     )  ; right part
         (orig    nil                     )  ; original right for reporting purposes
         (lparts  ()                      )  ; left part split
         (title   ""                      )  ; plot title
         (units   ""                      )  ; plot units
         (type    ""                      )  ; field type, usually F
         (ret     nil                     )
         (count   nil                     )) ; number of bidsets processed
    ;; initial reporting
    (message "  xem-bidsets-display-line: commencing")
    ;; capture
    (setq line (thing-at-point 'line))
    ;; integrity check for in-data or out-data
    (if (not (string-match regex line))
        (message "  xem: bidsets-display-line: no '%s' trip in this line, giving up." angles)
      ;; split field
      (setq parts (split-string line regex nil))     ; 'nil' is accept null substrings
        (setq left  (nth 0 parts))
        (setq right (nth 1 parts))                                  ; could be nil
        (setq right (xem-string-trim right))                        ; could be an empty string
        (setq orig right)                                           ; for completion reporting
        (setq right (replace-regexp-in-string "\\.\\.$" "" right))  ; trim any trailing double dots
        (setq right (xem-string-trim right))
        ;; process left viz : "actual-productions [W] F"
        (setq lparts (split-string left))
        (setq title (nth 0 lparts))                             ; "actual-productions"
        (setq title (replace-regexp-in-string "-" " " title))   ; substitute any dashes
        (setq units (nth 1 lparts))                             ; "[W]"
        (setq type  (nth 2 lparts))                             ; "X" in this case
        (if (null type) (setq units "(not given)"))             ; not fully robust but normally X is always present but units maybe not
        ;; process right viz : "\"bidset\" \"bidset\" \"bidset\""
        (setq data right)
        ;; integrity check
        (if (= (length right) 0)                                ; no data, no call
            (message "  xem: bidsets-display-line:: failed as no data: %s." orig )
          ;; plot call and completion reporting
          (setq count (xem-bidsets-display title
                                           units
                                           data))
          (message "  xem-bidsets-display: using: %s" orig)        ; often too long for normal reporting
          (message "  xem: bidsets-display-line complete using %d bidsets." count))) ))

(defun xem-bidsets-count-in-string (string regex)
  "Return count of REGEX matches in STRING."
  ;; CAUTION: must use index tracking with 'string-match'
  ;; search: count substrings count in string count matches count regexes count regexps
  (let* ((hits   0 )                         ; count to return
         (index  0 ))                        ; tracking index
    (while (string-match regex string index)
      (setq index (match-end 0))             ; update tracking
      (incf hits ))
    (message "  xem: bidsets-count-in-string: complete using regex %s matching %d times." regex hits )
    hits ))                                  ; expose return value

(defun xem-bidsets-display (title                      ; string without dashes
                            units                      ; complete with brackets
                            data)                      ; original string
  "Show a bidset timeseries to support public function `xem-bidsets-display-line'."
  (let* ((buffer-name  xem-buffer-bidsets-display )    ; results buffer
         (temp         data                       )
         (cnt          0                          )    ; number of current bids
         (bssCnt       0                          )    ; number bidsets for that interval
         (bumplen      0                          )    ; for finding the longest line
         (bidsets      ()                         )    ; list of formatted bidsets
         (entrys       ()                         )
         (count        0                          )
         (bfname       (buffer-name nil)          ))   ; originating buffer, just for reporting
    ;; initial reporting
    (message "  xem-bidsets-display: commencing")
    ;; modification for multiple bidsets
    (if (not (string-match "\" \"" temp))                             ; test for single string
        (setq temp (replace-regexp-in-string bsssep "\" \"" temp)))   ; then chop up, 'bsssep' is defined by caller
    ;; process data
    (setq temp (replace-regexp-in-string                              ; makes the splitting easier
                "\" \"" "\"\""                                        ; <" "> to <"">
                temp))
    (setq bidsets (split-string temp "\"" t))                         ; 't' is reject empty splits
    ;; obtain formatting info : 'bumplen' and 'bssCnt'
    (dolist (bidset (reverse bidsets))
      (if (> (length bidset) bumplen)
          (setq bumplen (length bidset)))
      (setq cnt 1)                                                    ; at least one bid is present
      (incf cnt (xem-bidsets-count-in-string bidset "\\*"))           ; call returns number of '*'
      (if (> cnt bssCnt)
          (setq bssCnt cnt)))
    (incf bumplen (* bssCnt 2))                                       ; assumes pos (+ or -) not present, fails gracefully
    ;; workhorse code
    (dolist (bidset (reverse bidsets))
      (push (xem-bidset-reformat bidset bumplen) entrys))             ; external call
    ;; new buffer
    (switch-to-buffer buffer-name)
    (text-mode)
    (xumber-mode)                                                     ; 'xeona' minor mode for numbers
    (setq truncate-lines t)
    ;; clear buffer contents, if present
    (erase-buffer)                                                    ; overrides narrowing too
    ;; insert results
    (insert (format "model  : %s\n" bfname))
    (insert (format "title  : %s\n" title))
    (insert (format "final  : %s\n" "total bid capacity, weighted-average unit price"))
    (insert (format "units  : %s\n" units))
    (insert (format "raw    : %s\n" data))
    (insert "\n")
    (dolist (entry entrys)
      (insert (format "%4d : %s\n" count entry))
      (incf count))
    ;; return to top
    (goto-char (point-min))
    (forward-line 5)
    ;; completion report
    (message "  xem-bidsets-display: complete")
    count))                                  ; expose return value

(defun xem-bidset-reformat (data             ; raw data
                            tabset)          ; alignment
  "Reformat a bidset to support private function `xem-bidsets-display'.

Note that an IEEE 754'inf' for elisp appears to be 1.0e+300 whereas for
C++ it is (with suppressed precision) 1.79769e+308"
  (let* ((bids            ()         )       ; list of bids
         (temp            ()         )       ; used to hold band, price pair
         (band            0.0        )
         (price           0.0        )
         (capacity        0.0        )       ; sum of bands
         (weighted-price  0.0        )       ; weighted sum of prices
         (buff            ()         )
         (sbuf            nil        )
         (formatstr       nil        )       ; formatting string for 'format'
         (output          ""         ))      ; output
    ;; obtain individual bids
    (message "  xem-bidset-reformat: input data: %s" data)
    (setq bids (split-string data "\\*" t))                 ; CAUTION: literal asterisk is "\\*" or [*]
    (message "  xem-bidset-reformat: bids: %s" bids)
    ;; calculate some ancillary information
    (dolist (bid bids)
      (setq temp (split-string bid " " t))                  ; 't' is reject empty splits
      (when (= (length temp) 2)                             ; essential skip the fixed value
        (setq band  (string-to-number (nth 0 temp)))        ; zero-based indexing
        (setq price (string-to-number (nth 1 temp)))
        (incf capacity band)
        (incf weighted-price (* band price)))               ; intermediate value
      (push (format "%11s %11s" (nth 0 temp) (nth 1 temp)) buff))
    (if (= capacity 0.0)                                    ; div-by-zero protection
        (setq weighted-price 0.0)                           ; should be zero anyway
      (setq weighted-price (/ weighted-price capacity)))    ; divide by capacity
    ;; create output
    (setq sbuf (mapconcat 'identity (nreverse buff) "  *  "))
    (setq sbuf (replace-regexp-in-string " $" "" sbuf))     ; trim final space is there is one
    (setq formatstr (concat "%-" (format "%d" tabset) "s : %+.6e %+.6e"))
    (setq output (format formatstr sbuf capacity weighted-price))
    ;; completion reporting
    (message "  xem: bidset-reformat: complete.")
    output ))                                               ; expose return value

;; ---------------------------------
;;  print a filtered xem file
;; ---------------------------------

(defconst xem-print-nocom-buffer-name        ; could add to buffer cycle list
  "*xem-print-nocom*"
  "Buffer for a full or part xem file, duly stripped of comments")

(defun xem-print-nocom-region (region-start region-end)
  "Print region while omitting comment lines (may include hidden rules too).\n
As programmed, expects a `robbie-print-ps' function to print a region."
  ;; CAUTION: the 'regex-2' screens and the run-time reporting may not match
  (interactive "r")
  (save-excursion
    (let* ((maxchar  173                                   )     ; long line truncation (based on 'across' printing)
           (reptag   " >>"                                 )     ; repetition tag, can be ""
           (replen   (length reptag)                       )     ; repetition tag length
           (regex-a  "^[[:blank:]]\\{6\\}"                 )     ; screen six leading blanks (spaces or tabs)
           (regex-b  "^[[:blank:]]\\{6\\}\\|^  ----------" )     ; screen ditto plus hidden rules (2 spaces and 10 dashes)
           (regex-w  "\\<<\\>"                             )     ; screen out-data (should not hit an email address), BUGGY
           (regex-x  "[[:blank:]]<"                        )     ; screen out-data (should not hit an email address)
           (regex-y  "[[:blank:]]>"                        )     ; screen in-data (should not hit an email address)
           (regex-z  "data-screen-disabled"                )
           (regex-1  regex-b                               )     ; CAUTION: swap regex here
           (regex-2  regex-x                               )     ; CAUTION: swap regex here
           (buffer   xem-print-nocom-buffer-name           )     ; output capture  buffer name
           (bm       (current-buffer)                      )     ; xem file buffer
           (bo       (get-buffer-create buffer)            )     ; output capture buffer
           (meta     nil                                   )     ; meta information
           (linebuf  nil                                   )     ; current line
           (graball  ()                                    )     ; main list of lines
           (grabtmp  ()                                    )     ; temporary list of lines
           (grabstr  nil                                   )     ; stringified list of lines
           (flag     t                                     )     ; 't' means last insert was not a blank line, else it was
           (commcnt  0                                     )     ; skipped comments count, includes hidden rules
           (harvcnt  0                                     )     ; harvested lines count
           (linecnt  0                                     )     ; to printer count
           (odatcnt  0                                     )     ; skipped out-data count
           (origcnt  0                                     )     ; original buffer count, observes narrowing
           (sqezcnt  0                                     )     ; skipped squeezed lines count
           (truncnt  0                                     )     ; truncated lines count
           (msg0     nil                                   )     ; message holder
           (msg1     nil                                   )     ; message holder
           (msg2     nil                                   )     ; message holder
           (msg3     nil                                   )     ; message holder
           (msg4     nil                                   )     ; message holder
           (msg5     nil                                   ))    ; message holder
      ;; preamble
      (message "  xem-print-nocom-region: commencing with maxchar %d, screening regexes '%s' and '%s'" maxchar regex-1 regex-2)
      ;; useful info
      (setq origcnt (count-lines (point-min) (point-max)))            ; observes narrowing
      (setq meta (format "source: %s" (buffer-name bm)))
      ;; harvest region and add to list#
      (push "" graball)                                               ; add blank line for good luck
      (goto-char region-start)                                        ; goto start region
      (while (< (point) region-end)
        (setq linebuf (thing-at-point 'line))                         ; grab line, includes new line
        (setq linebuf (replace-regexp-in-string "\n$" "" linebuf))    ; trim trailing newline
        (push linebuf graball)
        (incf harvcnt)
        (forward-line 1))
      (push "" graball)                                               ; add blank line for good luck
      ;; load buffer list
      (setq grabbuf graball)
      ;; filter comments and such using 'regex-1'
      (setq grabtmp ())
      (dolist (grab grabbuf)
        (if (string-match regex-1 grab)                               ; SCREENING STEP
            (incf commcnt)                                            ; skip current line
          (push grab grabtmp)))
      (setq grabbuf grabtmp)
      ;; filter out-data using 'regex-2'
      (setq grabtmp ())
      (dolist (grab grabbuf)
        (if (string-match regex-2 grab)                               ; SCREENING STEP
            (incf odatcnt)                                            ; skip current line
          (push grab grabtmp)))
      (setq grabbuf grabtmp)
      ;; truncate long lines for cleaner printing
      (setq grabtmp ())
      (dolist (grab grabbuf)
        (if (> (length grab) maxchar)                                 ; short string protection
            (progn
              (setq grab (substring grab 0 (- maxchar replen)))       ; TRUNCATION STEP
              (setq grab (concat grab reptag))
              (incf truncnt)))
        (push grab grabtmp))
      (setq grabbuf grabtmp)
      ;; squeeze list (suppress repeated empty lines)
      (setq grabtmp ())
      (setq flag t)                                                   ; 't' means last insert was not a blank line
      (dolist (grab grabbuf)
        (if (string-match "^$" grab)                                  ; BLANK LINE SCREENING STEP
            (if (null flag)
                (incf sqezcnt)                                        ; skip current line
              (push grab grabtmp)
              (incf linecnt)
              (setq flag nil))
          (push grab grabtmp)
          (incf linecnt)
          (setq flag t)))
      (setq grabbuf grabtmp)
      ;; messages
      (setq msg0 (format
                  "screens: '%s' '%s'"
                  regex-1 regex-2))
      (setq msg1 (format
                  "lines: original  %d (observes narrowing)"
                  origcnt))
      (setq msg2 (format
                  "lines: harvested %d, skipped comments %d, skipped out-data %d, squeezed %d, truncated %d"
                  harvcnt commcnt odatcnt sqezcnt truncnt))
      (setq msg3 (format
                  "lines: harvested %d, skipped comments %d, skipped out-data %d, squeezed %d"
                  harvcnt commcnt odatcnt sqezcnt))
      (setq msg4 (format
                  "trunc: truncated %d, cutoff %d chars"
                  truncnt maxchar))
      ;; stringify buffer
      (setq grabstr (xem-stringify-list grabbuf "\n"))                ; list to string conversion
      ;; print to behind-the-scenes buffer
      (set-buffer bo)
      (erase-buffer)
      (insert "\n")
      (insert (format "[%s]\n" meta))
      (insert (format "[%s]\n" msg0))
      (insert (format "[%s]\n" msg4))
      (insert (format "[%s]\n" msg1))
      (insert (format "[%s]\n" msg3))
      (insert grabstr)                                                ; main insert call
      (insert "\n")
      (insert (format "[%s]\n" meta))
      (insert (format "[%s]\n" msg0))
      (insert (format "[%s]\n" msg4))
      (insert (format "[%s]\n" msg1))
      (insert (format "[%s]\n" msg3))
      (xem-mode)
      ;; development
      (if t                                                           ; CAUTION: 'nil' = look, 't' = print
          (robbie-print-ps)                                           ; custom function defined in my '.emacs' file
        (switch-to-buffer bo))
      ;; finish
      (message "  xem-print-nocom-region: %s" msg0)
      (message "  xem-print-nocom-region: %s" msg1)
      (message "  xem: print-nocom-region: %s." msg2)                 ; note fullstop
      linecnt )))

(defun xem-print-nocom-buffer ()
  "Print buffer while omitting comment lines.\n
Wrapper to `xem-print-nocom-region'"
  (interactive)
  (save-excursion
    (let* ((linecnt  (xem-print-nocom-region (point-min) (point-max)) ))     ; beginning and end of buffer
      (message "  xem: print-nocom-buffer: transferred %d lines." linecnt) )))

;; ---------------------------------
;;  key selected entity ids
;; ---------------------------------

;;; (setq buff (sort buff '<)) ; CAUTION: is '< correct
;;; todo: add to menu

(defun xem-entity-id-mod-helper (input)
  "Reformat INPUT and return.\n
\"gate-stated-tariff-cseq-c01\" -> \"c.gate.01    gate-stated-tariff-cseq-c01\""
  (let* ((common  "_"                          )  ; when no letter
         (regex   "\\([[:alpha:]]*\\)\\(.*\\)" )  ; looking to split c|01
         (buff   ""                            )  ; return buffer
         (split  ()                            )  ; split id
         (first  nil                           )  ; first segment
         (last   nil                           )  ; last segment
         (leta   nil                           )
         (numa   nil                           )
         (stag   nil                           )) ; sort tag
    (message "  xem-entity-id-mod-helper: commencing: input '%s'" input)
    ;; identifier splitting
    (setq split (split-string input "-" t))       ; 't' is omit nulls
    (setq first (car split))
    (setq last (car (last split)))                ; 'car'ing is required
    ;; tag formation
    (if (not (string-match regex last))
        (setq stag "FAULTY")
      (setq leta (match-string 1 last))           ; one-based
      (if (= (length leta) 0) (setq leta common))
      (setq numa (match-string 2 last))
      (setq stag (concat leta "." first))
      (if (> (length numa) 0) (setq stag (concat stag "." numa))))
    ;; special overwrites
    (if (string-match "time-horizon" input) (setq stag (concat common common)))
    (if (string-match "overseer"     input) (setq stag (concat common common)))
    ;; create return
    (setq buff (format "%-10s %s" stag input))
    (message
     "  xem-entity-id-mod-helper: first '%s' last '%s' leta '%s' numa '%s' stag '%s'"
     first last leta numa stag)
    ;; complete reporting
    (message "  xem-entity-id-mod-helper: returning '%s'" buff)
    buff))                                        ; expose value

(defun xem-entity-id-mod (start end)
  "Generate keyed entity ids in region bounded START and END and add to kill ring.\n
Key generation is undertaken by private function `xem-entity-id-mod-helper'"
  (interactive "r")
  (save-excursion
    (let* ((regex  "^entity\\.\\(.*\\)$"   )
           (count  0              )            ; hit counter
           (all    nil            )
           (hit    nil            )
           (buff   ()             )
           (sbuf   nil            )
           )
      ;; initial message
      (message "  xem-entity-id-mod: commencing: regex '%s'" regex)
      ;; scan region
      (goto-char start)
      (while (re-search-forward regex end t) ; 't' means return 'nil', not error, on fail
        (incf count)
        (setq all (match-string 0))                              ; for debugging
        (setq hit (match-string 1))                              ; what we want
        (message "  xem-entity-id-mod: hit '%s'" hit)
        (push (xem-entity-id-mod-helper hit) buff))              ; reformatted
      ;; serialize 'buff'
      (setq sbuf (mapconcat 'identity (nreverse buff) "\n"))     ; stringify with newlines
      (setq sbuf (concat sbuf "\n"))                             ; add final newline
      ;; display (temporary hack)
      (message "\n")
      (message "%s" sbuf)
      (message "\n")
      ;; add 'sbuf' to kill ring
      (kill-new sbuf)                                            ; add 'sbuf' as string to kill ring
      ;; completion reporting
      (message "  xem: entity-id-mod: compete with %d hits" count) )))

;; ---------------------------------
;;  seek fully-qualified field id
;; ---------------------------------

(defun xem-get-full-field-identifier ()
  "Obtain fully-qualified identifier for field on current line.  Else return nil."
  ;; public point of entry
  (interactive)
  (message "  xem-get-full-field-identifier: commencing search")
  (let* ((splif  "[<>]"                      )    ; covers in-data and out-data
         (sep    "."                         )
         (key    (xem-get-field-name splif)  )
         (rec    (xem-get-current-record-id) )
         (fqid   nil                         ))   ; fully-qualified field identifier
    (message "  xem-get-full-field-identifier: rec '%s' key '%s'" rec key)
    (if (and key rec)
        (progn
          (setq fqid (concat rec sep key))
          (kill-new fqid)                         ; add 'id' as string to the kill ring
          (message "  xem: get-full-field-identifier: complete with '%s'." fqid))
      (message "  xem: get-full-field-identifier: failure to locate." ))
    fqid ))                                       ; expose return value

(defun xem-get-field-name (splif)
  "Return the field name on the current line using regex SPLIF.  Else return nil.

The SPLIF regex is usually, but not limited to, one of \"<\" \">\" \"[<>]\"."
  (message "  xem-get-field-name: commencing with splif '%s'" splif)
  (let* ((grab  (thing-at-point 'line)        )   ; grab current line
         (line  (xem-string-trim grab)        )
         (key   (xem-extract-key line splif)  )
         (key   (substring-no-properties key) ))  ; remove text properties including coloration
    (message "  xem-get-field-name: located key '%s' from trimmed line '%s'" key line)
    key ))                                        ; expose return value

(defun xem-extract-key (line                      ; input (no newline)
                        splif)                    ; split string regex, usually " < " or " > "
  "Extract field key from the given LINE and SPLIF regex.  Else return nil.

The SPLIF regex is usually, but not limited to, one of \"<\" \">\" \"[<>]\".
The call returns nil if nothing like a KEY is located."
  (message "  xem-extract-key: commencing with splif '%s'" splif)
  (let* ((elements  (split-string line splif) )
         (left      nil                       )
         (lefts     nil                       )
         (key       nil                       ))
    (if elements                                  ; also (< 0 (length elements))
        (setq left (xem-string-trim (pop elements))))
    (if left
        (setq lefts (split-string left)))
    (if lefts
        (setq key (xem-string-trim (pop lefts))))
    (if key
        (setq key (substring-no-properties key))) ; remove text properties including coloration
    (message "  xem-extract-key: located '%s'" key)
    key ))                                        ; expose the return value

;  $Id: xem-3.el 8356 2011-12-06 18:24:51Z robbie $
;  end of file

