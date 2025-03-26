;;; xem-4.el --- additional code 4 for 'xem.el'

;  file-purpose     : emacs additional code 4 for 'xem.el'
;  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
;  file-create-date : Tue 24-Aug-2010 09:52 UTC
;  file-status      : ongoing
;  file-keywords    : emacs xeona

;  $Revision: 8087 $
;  $Date: 2011-11-23 10:00:29 +0100 (Wed, 23 Nov 2011) $
;  $Author: robbie $
;  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/elisp/xem-4.el $

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
;;  summarize values
;; ---------------------------------

;; program.post-processing
;;
;;     summarize-list > "entity.teas-a07.productions entity.teas-a07.carbon-captures"
;;
;;                                         timeseries (2) : count       sum       min      mean       max  non0s  zeros     elems
;;     entity.teas-ccgt-capture-a07.productions           :     2         0         0         0         0      0      2         0         0
;;     entity.teas-ccgt-capture-a07.carbon-captures       :     2  3.28e-03  1.64e-03  1.64e-03  1.64e-03      2      0  1.64e-03  1.64e-03

(defun xem-summarize-line ()                 ; working
  "Summarize current line in form:

  count  integral       min      mean      max    opMean non0s zeros     elems
     12  7.33e-04  5.57e-09  3.50e-08  1.70e-08  1.70e-08    12     0  2.80e-08  3.50e-08  3.05e-08"
  (interactive)
  (message "  xem-summarize-line: commencing")
  (let* ((interval nil )                     ; horizon interval
         (target   nil )                     ; fully-qualified field identifier
         (head     nil )                     ; header (see 'tmp3' below)
         (summary  nil )                     ; formatted summary
         (buffer   nil ))                    ; final output
      ;; get the 'interval'
    (setq interval (xem-get-interval))
    (if (null interval)
        (error "  xem: summarize: horizon interval not located for some reason")
      (message "  xem-summarize-line: interval %d" interval))
    ;; get the 'target'
    (setq target (xem-get-full-field-identifier))
    (message "  xem-summarize-line: %s" target)
    ;; get the 'summary'
    (setq summary (xem-summarize-target target 0 3600))     ; "0" is no trunctation
    ;; define the 'head'
    (setq head "count  integral       min      mean       max    opMean non0s zeros     elems         2         3")
    ;; create 'buffer' and add to kill ring
    (setq buffer (format "  %s\n  %s\n  %s" target head summary))     ; no trailing newline
    (kill-new buffer)                                                 ; add 'buffer' as string to the kill ring
    ;; completion
    (message "  xem: summarize-line: complete (with reporting next) ..")
    (message "%s" buffer) ))

(defconst xem-summarize-info
  (list "program.post-processing" "summarize-list")
  "Two element list containing the record and field names which,
in turn, holds the numerical targets for `xem-summarize' and
`xem-summarize-raw-info'.  These targets need to be
space-separated and set in double-quotes.")

(defun xem-summarize-wrapper (truncate)      ; wrapper function
  "Wrapper to function `xem-summarize' which also seeks a value for TRUNCATE from the user.

TRUNCATE may be zero to not truncate or a non-negative integer to
truncate.  If too high, the value is ignored."
  (interactive "nEnter statistical summary truncation length (0 for all): ")
  (save-excursion
    (let* ((summary nil ))
      ;; initial reporting
      (message "  xem-summarize-wrapper: commencing with truncate %d" truncate)
      (message "  xem-summarize-wrapper: about to call 'xem-summarize'")
      ;; wrapper call
      (setq summary (xem-summarize truncate))
      ;; insert at top of buffer
      (goto-char (point-min))
      (insert "\n")
      (insert "note\n")                      ; else 'xeona' warns about encountering homeless lines
      (insert "\n")
      (insert summary)
      (insert "\n")
      ;; add to kill ring too
      (kill-new summary) )))

(defun xem-summarize (truncate)
  "Summarize nominated list and cut timeseries to TRUNCATE.

If the point is on a blank line, the summary is inserted there,
otherwise it is added to the kill ring.  The nominated list is
set by constant `xem-summarize-data'.

The 'opMean' operational mean is divided by the number of non-zeros."
  ;; later bind to <s-menu>
  (save-excursion
    (let* ((indent     2   )                 ; margin indent (was 1 and 4 but 2 works will with 'xem-cycle-squish')
           (rawtargs   nil )                 ; targets in raw form
           (targets    ()  )                 ; list of targets as strings: entity.identifier.fieldname
           (short      nil )                 ; target stripped of leading "entity."
           (shorts     ()  )                 ; list of shorts
           (summaries  ()  )                 ; list of summaries (value, count, mean, etc)
           (formatstr  ""  )                 ; line format string
           (buff       ()  )                 ; formatted output buffer
           (sbuf       nil )                 ; 'buff' as string
           (bumplen    0   )                 ; ratcheted target string length
           (count      0   )                 ; number of targets processed
           (tmp1       nil )
           (tmp2       nil )
           (tmp3       nil )
           (tmp4       nil )
           (headl      nil )                 ; left heading string
           (headr      nil )                 ; right heading string
           (hline      nil )                 ; headings line
           (interval   nil ))
      ;; initial reporting
      (message "  xem-summarize: commencing: truncate %g" truncate)
      ;; integrity check on 'truncate'
      (if (not (integerp truncate))
          (error "  xem: summarize: truncate must be an integer: %g" truncate))
      ;; error if buffer currently narrowed
      (if (not (= (point-min) 1))                                     ; not a foolproof test, first char could be narrowed
          (error "  xem: summarize: buffer currently narrowed (point-min %d)" (point-min)))
      ;; collect targets
      (setq rawtargs (xem-summarize-raw-info))
      ;; error if no raw info
      (if (null rawtargs)
          (error "  xem-summarize: field not found: %s" xem-summarize-data))
      ;; split raw arguments
      (setq rawtargs (replace-regexp-in-string "\"" "" rawtargs))     ; remove the double-quotes from each end
      (setq targets (split-string rawtargs))                          ; space-separated
      ;; error if empty
      (if (null targets)
          (error "  xem: summarize: no entries to summarize (the field is empty)"))
      ;; get the interval
      (setq interval (xem-get-interval))
      (if (null interval)
          (error "  xem: summarize: horizon interval not located for some reason"))
      ;; processing
      (dolist (target (reverse targets))                              ; CAUTION: 'reverse' is related to 'push'
        (incf count)
        (setq short (replace-regexp-in-string "^entity\\." "" target))
        (if (> (length short) bumplen) (setq bumplen (length short))) ; ratchet the length
        (push short shorts)
        (push (xem-summarize-target target truncate interval) summaries))
      ;; create and add header line first
      (setq tmp1 (format "timeseries (%d)" truncate))
      (setq tmp2 (make-string (+ indent bumplen (- (length tmp1))) 32))    ; using ASCII 32 space characters
      (setq headl (concat tmp2 tmp1))
      (setq tmp3 "count  integral       min      mean       max    opMean non0s zeros     elems")
      (dotimes (i (- (min (if (zerop truncate) 3 truncate) 3) 1))     ; work out how many cols to label
        (setq tmp4 (concat tmp4 (format "%10d" (+ i 1)))))            ; create labels
      (setq headr (concat tmp3 tmp4))
      (setq hline (concat headl " : " headr))
      (push hline buff)
      ;; create format string here because elisp format strings cannot take variables
      (setq formatstr (concat (make-string indent 32) "%-" (format "%d" bumplen) "s" " : " "%s"))
      ;; create formatted output as list
      (while shorts
        (push (format formatstr (pop shorts) (pop summaries)) buff))
      ;; serialize list adding newlines
      (setq sbuf (mapconcat 'identity (nreverse buff) "\n"))
      ;; completion reporting
      (message "  xem: summarize: complete with %d targets and truncate %d." count truncate)
      sbuf)))                                                         ; expose return value

(defun xem-summarize-raw-info ()
  "Recover information from the field that constant `xem-summarize-info' specifies."
  ;; required because 'xem-narrow-to-entity' refuses to narrow to non-entity records
  (save-excursion
    (let* ((limit   200                             )  ; search limit
           (data    xem-summarize-info              )
           (record  (pop data)                      )
           (field   (pop data)                      )
           (regex1  (concat "^" record "$")         )
           (regex2  (concat "^[[:blank:]]*" field)  )
           (raw     nil                             ))
      (message "  xem-summarize-raw-info: using %s" xem-summarize-info)
      (goto-char (point-min))
      (if (re-search-forward regex1)
          (if (re-search-forward regex2 (+ (point) limit))
              (progn
                (setq line (thing-at-point 'line))
                (setq line (xem-string-trim line))
                (setq raw (xem-extract-data line field ">"))
                (message "  xem: summarize-raw-info: complete and successful with '%s'." raw)
                raw))
        (message "  xem: summarize-raw-info: complete but failed to located information.")
        raw) )))                             ; expose return value

(defun xem-summarize-raw-target-value (target)
  "Get raw value for TARGET in form 'prefix.record.field'.
Assumes either \"<\" or \">\" separates the value."
  ;; does not assume that 'field' must resides in 'prefix.record'
  (save-excursion
    (let* ((splif1   "\\."  )
           (splif2   "[<>]" )
           (splits   ()     )                ; split 'target'
           (prefix   nil    )
           (entity   nil    )
           (field    nil    )
           (record   nil    )                ; "prefix.entity"
           (regex    nil    )
           (raw      nil    ))               ; raw target value, to be returned
      ;; initial reporting
      (message "  xem-summarize-raw-target-value: commencing on target '%s'" target)
      ;; dissect target
      (setq splits (split-string target splif1))
      (setq prefix (pop splits))
      (setq entity (pop splits))
      (setq field  (pop splits))
      (setq record (concat prefix "." entity))
      (message "  xem-summarize-raw-target-value: reconstructed target '%s.%s.%s'" prefix entity field)
      ;; search
      (setq regex (concat "^" record "$"))
      (goto-char (point-min))
      (if (re-search-forward regex)
          (progn
            (goto-char (match-beginning 0))                 ; required by 'xem-narrow-to-entity'
            (xem-narrow-to-entity)                          ; file 'xem.el'
            (setq raw (xem-get-field-value field splif2))   ; file 'xem.el' [1]
            (widen)                                         ; un-narrow
            (customize-set-variable 'cursor-type 'box)      ; return cursor to its normal state, was 'bar
            (if raw
                (message "  xem: summarize-raw-target-value: complete and successful with '%s'." raw)
              (message "  xem: summarize-raw-target-value: complete but field '%s' faulty." field))
            raw)
        (message "  xem: summarize-raw-target-value: complete but record '%s' not found." record)
      raw))))                                ; expose return value, could be 'nil'

;; CAUTION: [1] could be a subtle bug here if search-forward does no respect narrowing .. but it seems to
;; TOFIX: BUG: [1] bad bug if function 'xem-get-field-value' fails, function stalls and buffer remains narrowed

(defun xem-summarize-target (target truncate interval)
  "Interpret the TARGET and, if a timeseries, consider TRUNCATE elements only.

INTERVAL is the horizon interval length in seconds and is used to calculate the integral.
Checks are undertaken and the processing duly adjusted.  Raw
values containing double-quotes are rejected.  Raw values
containing the repetition string \"..\" have this removed before
continuing.  A TRUNCATE value of zero, means to not truncate.
The code to summarize timeseries is contained in function
`xem-summarize-timeseries'"
  (let* ((splif  "[[:blank:]]+"                          )
         (buff   "(xem-get-field-value failed)"          )
         (raw    (xem-summarize-raw-target-value target) )
         (nist   ()                                      )) ; list of numbers (but not a vector)
    (message "  xem-summarize-target: commencing: truncate %d and target '%s'" truncate target)
    (message "  xem-summarize-target: raw '%s'" raw)
    ;; process raw value
    (if raw
        (cond
         ((string-equal "" raw)              ; no data
          (setq buff "(no data)"))
         ((string-match "\\.class$" target)  ; special case field
          (setq buff raw))                   ; return class value as is
         ((string-match "\"" raw)            ; contains double-quotes
          (setq buff raw))                   ; return string value as is
         (t                                  ; else carry on, should have recoverable numbers
          (setq split (split-string raw splif))             ; 'reverse' is related to 'push'
          (dolist (strnum (reverse split))
            (if (not (string-match "\\.\\." strnum))        ; avoid double dots
                (push (string-to-number strnum) nist)))     ; cast to number
          (setq cnt (length nist))
          (cond
           ((= cnt 0)
            (setq buff "(empty vector)"))
           ((= cnt 1)
            (setq buff (format "%g" (car nist))))
           (t
            (setq buff (xem-summarize-timeseries nist truncate interval)))))
         (message "  xem: summarize-target: complete and successful with %s." buff))
      buff
      (message "  xem: summarize-target: complete but unsuccessful."))
    buff))                                   ; expose return value

; http://stackoverflow.com/questions/590579/how-to-sum-a-list-of-numbers-in-emacs-lisp
; can use (reduce '+ (list 1 2 3))
; but this is better (apply '+ (list 1 2 3)) and (apply 'max (list 1 10 3))

(defun xem-summarize-timeseries (numlist trunc interval)
  "Calculate and report statistics for the first TRUNC elements of NUMLIST.

INTERVAL is the horizon interval length in seconds and is used to calculate the integral.
If TRUNC is zero or excessive, then use the original length."
  ;; should ideally separate the numerical code and parsing and formatting code
  (let* ((sep    " "              )          ; entry separator, also ","
         (orlen  (length numlist) )          ; simply zero if 'nil' or ()
         (count  nil              )
         (integ  nil              )
         (sum    nil              )
         (min    nil              )
         (max    nil              )
         (mean   nil              )          ; normal mean
         (opme   nil              )          ; operational mean, divided by 'nonzs'
         (zeros  nil              )
         (nonzs  nil              )
         (buff   ()               )
         (sbuf   nil              ))
    ;; initial reporting
    (message "  xem-summarize-timeseries: commencing: truncate %d" trunc)
    ;; confirm 'numlist'
    (if (null numlist)
        (error "  xem: summarize-timeseries: numlist is null: %s" numlist))
    ;; check 'trunc' is integer-valued
    (if (not (integerp trunc))
        (error "  xem: summarize-timeseries: trunc not integer: %g" trunc))
    ;; truncate 'numlist' if appropriate
    (if (and (not (zerop trunc)) (< trunc orlen))
        (nbutlast numlist (- orlen trunc)))       ; last 'n' elements removed, note also 'butlast'
    ;; active code
    (setq count (length numlist))
    (setq sum   (apply '+   numlist))             ; the 'apply' is powerful, note also 'reduce'
    (setq integ (* sum interval))
    (setq min   (apply 'min numlist))
    (setq max   (apply 'max numlist))
    (setq mean  (/ sum count))                    ; 'count' cannot be zero
    (setq zeros 0)
    (dolist (num numlist)
      (if (zerop num)                             ; test for exact zero
          (incf zeros)))
    (setq nonzs  (- count zeros))
    (setq mean  (/ sum count))                    ; 'count' cannot be zero
    (if (zerop nonzs)                             ; but 'nonzs' can be zero
        (setq opme 0)
      (setq opme (/ sum nonzs)))
    (message
     "  xem-summarize-timeseries: original length %d  count %d  sum %g  integral %g min %g  mean %g  max %g  nonz %d  zero %d"
     orlen count sum integ min mean max nonzs zeros)
    ;; formatting
    (push (format "%5d"   count) buff)
;;  (push (format "% .2e" sum)   buff)
    (push (format "% .2e" integ) buff)
    (push (format "% .2e" min)   buff)
    (push (format "% .2e" mean)  buff)
    (push (format "% .2e" max)   buff)
    (push (format "% .2e" opme)  buff)
    (push (format "%5d"   nonzs) buff)
    (push (format "%5d"   zeros) buff)
    ;; append the first three or so elements
    (dotimes (i (min orlen (length numlist) 3))   ; choose the lowest value, also 'i' is 0 inclusive .. value exclusive
      (push (format "% .2e" (pop numlist)) buff))  ; destructive on 'numlist', else use 'nth'
    ;; serialize
    (setq sbuf (mapconcat 'identity (nreverse buff) sep))
    ;; reduce the "0.00e+00" zeros with "       0"
    (setq sbuf (replace-regexp-in-string "0\\.00e\\+00" "       0" sbuf))
    ;; completion reporting
    (message "  xem: summarize-timeseries: complete using %d truncate." trunc)
    sbuf))                                        ; expose return value

;; ---------------------------------
;;  create a selections list
;; ---------------------------------

;; originally written to generate in-data values for 'AsopLmpBidsParam::selections-list'
;;
;; steps  : 12
;; input  : selections-list : 1 : 0 = 3 4 : 2 = 9 10
;; output : 1 1 1 0 0 1 1 1 1 2 2 1 1

(defun xem-make-selections-list-wrapper (length)
  "Generate a LENGTH long selections-list from simplified information.

If LENGTH is zero, locate and use the current value of
'entity.time-horizon.steps' by invoking function `xem-get-steps'.
Function `xem-make-selections-list' does the bulk of the work and
describes the input format."
  (interactive "nEnter desired length (0 to use current time horizon steps): ")
  (let* ((steps  nil ))                      ; value to use
    ;; initial reporting
    (message "  xem-make-selections-list-wrapper: commencing with entered length %g" length)
    ;; active code
    (cond                                    ; case statement
     ((not (integerp length))                ; non-integral test
      (error "  xem-make-selections-list-wrapper: error: non-integral value %g entered" length))
     ((< length 0)
      (error "  xem-make-selections-list-wrapper: error: negative value %d entered" length))
     ((= length 0)                           ; locate and use the current time horizon value
      (setq steps (xem-get-steps))           ; returns 'nil' on failure to locate
      (if steps
          (message "  xem-make-selections-list-wrapper: using current value of steps %d" steps)
        (error "  xem-make-selections-list-wrapper: error: unable to locate time horizon steps (check the model)")))
     (t                                      ; else
      (setq steps length)
      (message "  xem-make-selections-list-wrapper: using prompted value of steps %d" steps) ))
    (xem-make-selections-list steps) ))      ; workhorse call

(defun xem-make-selections-list (steps)
  "Generate a STEPS long selections-list from simplified information.

Given 12 steps and the line:  selections-list : 1 : 0 = 3 4 : 2 = 9 10
This should yield:            1 1 1 0 0 1 1 1 1 2 2 1

Based on the : field separator, field one is discarded, field two
sets the default, and the remaining optional fields define the
alterations.  These latter fields use the = subfield separator,
in which the left side sets the value and the right side provides
a space-separated list the zero-based indexes."
  ;; uses a fixed-length vector, which, like a string, is a type of elisp array
  (let* ((sepchar  ":"                                            )
         (equchar  "="                                            )
         (capture  (thing-at-point 'line)                         )   ; complete with newline
         (line     (replace-regexp-in-string "\n$" "" capture)    )   ; remove the newline
         (line     (substring-no-properties line)                 )   ; remove text properties including coloration
         (line     (xem-string-trim line)                         )   ; trim junk
         (sepex    (concat "[[:blank:]]*" sepchar "[[:blank:]]*") )
         (equex    (concat "[[:blank:]]*" equchar "[[:blank:]]*") )
         (splits   ()                                             )
         (tag      nil                                            )
         (meta     nil                                            )
         (defstr   nil                                            )   ; default value as string
         (default  nil                                            )   ; default value as integer
         (vec      nil                                            )   ; vector-type array
         (value    nil                                            )
         (indexes  nil                                            )
         (buf      ()                                             )
         (str      nil                                            ))  ; final string
    ;; initial reporting
    (message "  xem-make-selections-list: commencing with '%s'" line)
    ;; confirm 'steps'
    (if (or (null steps)
            (zerop steps))
        (error "  xem-make-selections-list: error: unsuitable time horizon steps: %s" steps)
      (message "  xem-make-selections-list: using steps %d" steps))
    ;; check line
    (if (zerop (length line))
        (error "  xem-make-selections-list: error: current line empty '%s'" line))
    ;; harvest the specification with integrity checks
    (setq splits (split-string line sepex nil))             ; 'nil' means include zero-length substrings
    (setq tag    (pop splits))
    (setq defstr (pop splits))
    (if (not defstr)
        (error "  xem-make-selections-list: error: ill-formed input (check current line)"))
    (setq default (string-to-number defstr))
    (if (not (integerp default))
        (error "  xem-make-selections-list: error: ill-formed input: non-integral default value: %g" default))
    (message "  xem-make-selections-list: default %d" default)
    ;; create fixed length vector-type array filled with 'default'
    (setq vec (make-vector steps default))
    (message "  xem-make-selections-list: 'vec' created %s" vec)
    ;; modify 'vec' -- whilst noting the indexes need not be ordered
    (dolist (split splits)
      (setq two (split-string split equex))
      (setq value (pop two))                                ; left-side hold value as string
      (setq value (string-to-number value))
      (setq indexes (split-string (pop two)))               ; split right-side
      (dolist (index indexes)
        (setq index (string-to-number index))
        (aset vec index value)))                            ; modify 'vec' using 'aset'
    (message "  xem-make-selections-list: 'vec' altered %s" vec)
    ;; create a list from 'vec' using 'aref'
    (dotimes (i steps)
      (push (format "%d" (aref vec i)) buf))
    (message "  xem-make-selections-list: 'buf' created %s" buf)
    ;; serialize
    (setq str (mapconcat 'identity (nreverse buf) " "))     ; stringify [1]
    ;; [1] 'identity' is built-in function and 'mapconcat' returns an empty string if 'buff' is '()'
    (message "  xem-make-selections-list: 'str' created '%s'" str)
    ;; add to kill ring
    (kill-new str)
    ;; completion reporting
    (message " xem: make-selections-list: complete and added to kill ring from tag '%s' with length %d." tag steps) ))

;; ---------------------------------
;;  get 'steps' and 'interval'
;; ---------------------------------

(defun xem-get-steps ()
  "Locate and return the value of 'entity.time-horizon.steps'.

This function expects to encounter the following lines (or similar):

  entity.time-horizon
      steps [-] i    > 6

On failure, the function returns nil."
  ;; It would have been eminently better to have written a
  ;; comprehensive record.field search once and well!
  (save-excursion
    (let* ((entity-id  "entity.time-horizon"      )    ; hardcoded in 'xeona'
           (field-id   "steps"                    )    ; hardcoded in 'xeona'
           (regex-1    (concat "^" entity-id "$") )    ; sole term on line
           (regex-2    (concat "^[[:blank:]]*" field-id "[[:blank:]]+.*[[:blank:]]>[[:blank:]]*\\([[:digit:]]*\\)$") )
           (numstr     nil                        )
           (steps      nil                        ))   ; error value too
      ;; initial reporting
      (message "  xem-get-steps: commencing search for '%s.%s'" entity-id field-id)
      ;; go to beginning of accessible portion of buffer
      (goto-char (point-min))
      ;; 're-search-forward' sets point at end of occurrence and returns point,
      ;; 'nil' means no search bound, non-nil means return 'nil' not error
      (if (re-search-forward regex-1 nil t)            ; entity search
          (if (re-search-forward regex-2 nil t)        ; field search
              (progn
                (setq numstr (match-string 1))
                (setq steps (string-to-number numstr))
                (message "  xem-get-steps: complete with %d" steps) steps )
            (message "  xem-get-steps: complete but field '%s' not found" field-id) steps )
        (message "  xem-get-steps: complete but record '%s' not found" entity-id) steps ) )))

(defun xem-get-interval ()
  "Locate and return the value of 'entity.time-horizon.interval'.

This function expects to encounter the following lines (or similar):

  entity.time-horizon
      interval [-] i    > 3600

On failure, the function returns nil."
  ;; It would have been eminently better to have written a
  ;; comprehensive record.field search once and well!
  (save-excursion
    (let* ((entity-id  "entity.time-horizon"      )    ; hardcoded in 'xeona'
           (field-id   "interval"                 )    ; hardcoded in 'xeona'
           (regex-1    (concat "^" entity-id "$") )    ; sole term on line
           (regex-2    (concat "^[[:blank:]]*" field-id "[[:blank:]]+.*[[:blank:]]>[[:blank:]]*\\([[:digit:]]*\\)$") )
           (numstr     nil                        )
           (interval   nil                        ))   ; error value too
      ;; initial reporting
      (message "  xem-get-interval: commencing search for '%s.%s'" entity-id field-id)
      ;; go to beginning of accessible portion of buffer
      (goto-char (point-min))
      ;; 're-search-forward' sets point at end of occurrence and returns point,
      ;; 'nil' means no search bound, non-nil means return 'nil' not error
      (if (re-search-forward regex-1 nil t)            ; entity search
          (if (re-search-forward regex-2 nil t)        ; field search
              (progn
                (setq numstr (match-string 1))
                (setq interval (string-to-number numstr))
                (message "  xem-get-interval: complete with %d" interval) interval )
            (message "  xem-get-interval: complete but field '%s' not found" field-id) interval )
        (message "  xem-get-interval: complete but record '%s' not found" entity-id) interval ) )))

;; ---------------------------------
;;  occur navigation
;; ---------------------------------

(defun xem-occur-next ()
  "Skip thru `occur' occurrences.

Continues to work fine if the main window has been maximized using `delete-other-windows'."
  ;; gracefully reports "No buffer named *Occur*" if that is the case
  ;; this is probably key bound too, perhaps to <f6>
  (interactive)
  (let* ((ob  "*Occur*" ))
    ;; (select-window ob)                    ; never tested
    ;; (switch-to-buffer ob)                 ; kinda works but flip-flops the windows
    (set-buffer ob)
    (occur-next 1)
    (occur-mode-goto-occurrence)
    (message "  xem: occur-next: complete.") ))

(defun xem-occur-prev ()
  "Reverse skip thru `occur' occurrences.

Continues to work fine if the main window has been maximized using `delete-other-windows'."
  ;; gracefully reports "No buffer named *Occur*" if that is the case
  ;; this is probably key bound too, perhaps to <S-f6>
  (interactive)
  (let* ((ob  "*Occur*" ))
    ;; (select-window ob)                    ; never tested
    ;; (switch-to-buffer ob)                 ; kinda works but flip-flops the windows
    (set-buffer ob)
    (occur-prev 1)
    (occur-mode-goto-occurrence)
    (message "  xem: occur-next: complete.") ))

;; ---------------------------------
;;  toggle default and release build
;; ---------------------------------

(defun xem-toggle-xeona-binary ()
  "Toggle between 'mach' and 'release' builds of xeona."
  (interactive)
  (message "  xem-toggle-xeona-binary: commencing")
  (let* ((xbin   xem-binary                      )     ; current binary (fully-qualified)
         (xleaf  (file-name-nondirectory xbin)   )     ; leaf name
         (xext   (file-name-extension xleaf nil) )     ; 'nil' is omit the dot
         (dbin   xem-binary-name                 )     ; default binary (leaf only) = 'xeona.mach' at time of writing
         (dleaf  (file-name-nondirectory dbin)   )
         (dext   (file-name-extension dleaf nil) )
         (temp1  nil                             )
         (temp2  nil                             ))
    (message "  xem-toggle-xeona-binary: xeona current '%s' default '%s'" xleaf dleaf)
    ;; switch on 'xext'
    (cond                                                        ; process 'xext', the current binary extension
     ;; default binary is current
     ((string-equal dext xext)
      (setq temp1 (xem-locate-release-build))                    ; hunt for the latest release build
      (if temp1
          (setq temp2 (xem-locate-binary temp1)))                ; reconfirm (not strictly necessary)
      (if temp2
          (progn
            (setq xem-binary temp2)                               ; toggle now
            (setq xem-svn (xem-binary-svn xem-binary))))
      (if temp2
          (message "  xem: toggle-xeona-binary: toggled to '%s'" (file-name-nondirectory xem-binary))
        (message "  xem: toggle-xeona-binary: toggle failed, staying with default '%s'" (file-name-nondirectory xem-binary))))
     ;; release binary is current
     ((string-match "^r[[:digit:]]+$" xext)                      ; pattern 'r0000'
      (setq temp1 (xem-locate-binary xem-binary-name))           ; default
      (if temp1
          (setq xem-binary temp1))                               ; toggle now
      (if temp1
          (message "  xem: toggle-xeona-binary: togged to '%s'" (file-name-nondirectory xem-binary))
        (message " xem: toggle-xeona-binary: toggle failed, staying with release '%s'" (file-name-nondirectory xem-binary))))
     ;; neither condition occurred
     (t
      (error "  xem-toggle-xeona-binary: xeona binary extension not supported: %s" xext))) ))

(defun xem-locate-release-build ()
  "Locate the latest release build of 'xeona'."
  ;; CAUTION: careful with any svn transition to "0000" across
  ;; "00000" as the "0000" sort lower!
  (message "  xem-locate-release-build: commencing")
  (let* ((paths   (list "." ".." "../.." "../xeona1") )
         (regex   "^xeona\\.r[[:digit:]]+$"           )     ; typically 'xeona.r0000'
         (hits    ()                                  )
         (latest  nil                                 ))
    ;; search for release build files
    (setq paths (nreverse paths))                           ; work backwards
    (dolist (path paths)
      (if (null hits)
          (setq hits (directory-files path t regex nil))))  ; [1] elisp 'find' call
    ;; [1] 't' is use fully-qualified name, 'nil' is reject
    ;; nosort, meaning return in alphabetical order -- see
    ;; caution above about when 'subversion' roll over to five
    ;; digits
    ;;
    ;; respond if found
    (if hits
        (progn
          (setq latest (car (last hits)))
          (message "  xem-locate-release-build: %s" latest))
      (message "  xem-locate-release-build: nothing found"))
      latest ))

;  $Id: xem-4.el 8087 2011-11-23 09:00:29Z robbie $
;  end of file

