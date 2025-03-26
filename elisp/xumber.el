;;; xumber.el --- extract and manipulate plain text numbers within the editor

;  file-purpose     : emacs extract and manipulate plain text numbers within the editor
;  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
;  file-create-date : Sat 31-Jul-2010 10:10 UTC
;  file-status      : working
;  file-keywords    : emacs xeona

;  $Revision: 7835 $
;  $Date: 2011-10-31 09:40:15 +0100 (Mon, 31 Oct 2011) $
;  $Author: robbie $
;  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/elisp/xumber.el $

;  XEONA CODEBASE COPY
;
;  * this code originated as part of the xeona codebase
;
;  * the GLPv2 license included here derives from Emacs Lisp
;    coding practice and from not the 'xeona' project --
;    users can optionally chose to adopt the 'xeona' norm
;    instead

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
;;      > add "[+- ]inf" and "nan" searching
;;
;;      * check current buffer logic (although no problems have
;;        been detected), while noting the warning in section 27.2
;;        of the elisp manual
;;
;;      * check `xumber-reformat-number'
;;
;;      * run `checkdoc' periodically
;;
;;  Known issues
;;
;;      * see commit r7193 for the`xumber-mode-syntax-table' fix,
;;        which should be settled now
;;

;;; History:

;;      * see the subversion log messages

;;; Code:

;; ---------------------------------
;;  user-modifiable global constants
;; ---------------------------------

(defconst xumber-sum-buffer     "*xumber-tally*"   "Buffer for notifying last tally.")
(defconst xumber-history-buffer "*xumber-history*" "Buffer for recording tally history.")

;; ---------------------------------
;;  custom key maps
;; ---------------------------------

(defvar xumber-mode-map nil "Local keymap for Xumber mode buffers.")

(if xumber-mode-map
    ()                                       ; protection against double loading

  ;; 'super' is the Windows/Penguin key

  ;; keyboard shortcuts
  (setq xumber-mode-map (make-sparse-keymap))
  (define-key xumber-mode-map [(super         right)] 'xumber-show-number)
  (define-key xumber-mode-map [(super          left)] 'xumber-insert-number)
  (define-key xumber-mode-map [(super            up)] 'xumber-move-number-prior)
  (define-key xumber-mode-map [(super          down)] 'xumber-move-number-next)
  (define-key xumber-mode-map [(super        delete)] 'xumber-sum-zero)
  (define-key xumber-mode-map [(super          home)] 'xumber-switch-history-buffer)

  ;; custom menu
  (defvar menuXumber0 (make-sparse-keymap "Xumber Menu"))
  (defvar menuXumber1 (make-sparse-keymap "Xumber1"))

  ;; top-level entry (visible in menu bar, more to the right of "Tools")
  (define-key xumber-mode-map [menu-bar xumber]           (cons "Xumber" menuXumber0))    ; menu-bar label

  ;; main menu bottom (reverse order)
  (define-key menuXumber0 [about]       '(menu-item "About"                       xumber-menu-about    :help "One-line report on Xumber minor mode (\"s-\" indicates the Windows/Penguin key)"))
  (define-key menuXumber0 [message]     '(menu-item "Open small *Message* buffer" xumber-open-messages :help "Open a small message buffer and watch the messages"))
  (define-key menuXumber0 [separator-1] '("--"))

  ;; sub-menu entries (reverse order)
  (define-key xumber-mode-map [menu-bar xumber submenu1]  (cons "Find"   menuXumber1))

  ;; main menu top (reverse order)
  (define-key menuXumber0 [separator-2] '("--"))
  (define-key menuXumber0 [number-1]    '(menu-item "Smart insert formatted number"  xumber-insert-number         :help "Insert and reformat prompted number, while respecting the current data-type hint in {f i}"))
  (define-key menuXumber0 [reformat]    '(menu-item "Smart reformat current number"  xumber-reformat-number       :help "Reformat current number."))
  (define-key menuXumber0 [separator-3] '("--"))
  (define-key menuXumber0 [sum-buffer]  '(menu-item "Switch to tally history buffer" xumber-switch-history-buffer :help "Switch to tally history buffer"))
  (define-key menuXumber0 [zero-num]    '(menu-item "Reset number tally"             xumber-sum-zero              :help "Reset the number count and tally to zero"))
  (define-key menuXumber0 [find-prior]  '(menu-item "Previous number"                xumber-move-number-prior     :help "Find previous number using standard emacs symbol motion and number testing"))
  (define-key menuXumber0 [find-next]   '(menu-item "Next number"                    xumber-move-number-next      :help "Find next number using standard emacs symbol motion and number testing"))
  (define-key menuXumber0 [show-num]    '(menu-item "Show and tally current number"  xumber-show-number           :help "Show current number in a potentially more readable format and also tally it"))

  ;; ---

  ;; submenu 1 (reverse order)
  (define-key menuXumber1 [find-noeng]  '(menu-item "Find next non-engineering notation"  xumber-find-float-noneng  :help "Find next number in scientific notation, but not standard engineering notation"))
  (define-key menuXumber1 [find-sci]    '(menu-item "Find next 000.0 scientific notation" xumber-find-float-000     :help "Find next number in scientific notation and also using '000.0'"))
  (define-key menuXumber1 [find-dec]    '(menu-item "Find next decimal notation"          xumber-find-float-decimal :help "Find next number in decimal notation"))
  (define-key menuXumber1 [find-dec]    '(menu-item "Find next integer notation"          xumber-find-integer       :help "Find next number in integer notation"))

  ) ; final parenthesis

;; ---------------------------------
;;  menu support
;; ---------------------------------

(defun xumber-menu-about ()
  "About message for `xumber-mode'."
  (interactive)
  (let* ((file    (car (load-history-filename-element "xumber")) )  ; locate name of this elisp file
         (sedstr  "s/^[[:space:]]*;\\+[[:space:]]*\\$Revision: \\([[:digit:]]*\\) \\$/\\1/p")
         (call    (concat "sed --quiet '" sedstr "' " file)      )
         (svnver  "(not set)"                                    ))
    (message "  xumber: about: file: '%s'" file)
    (message "  xumber: about: sed: '%s'" sedstr)
    (setq svnver (shell-command-to-string call))
    (if (string-match "\n$" svnver)                    ; trim trailing newline
        (setq svnver (replace-match "" t t svnver)))   ; fixed case and literal
    (message "  xumber: about: svnver: '%s'" svnver)
    (message "Xumber minor mode %s to extract and manipulate plain text and numbers (\"s-\" is the Windows/Penguin key)." svnver)))

;; ---------------------------------
;;  symbol table
;; ---------------------------------

;; modify the syntax table for this mode
;; http://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Syntax-Table-Functions
;; http://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Example-Major-Modes
;; http://www.emacswiki.org/emacs/EmacsSyntaxTable
;; show current: C-h s

(defvar xumber-mode-syntax-table
  (let ((st  (make-syntax-table) ))          ; make new syntax table by MODIFYING the existing table
    (modify-syntax-entry ?. "_" st)          ; put the dot char (.) in class symbol (_)
    ;; the '[' and ']' mean that the number search functions do
    ;; not get hung up on, for example, [999] -- remove the next
    ;; two statements if problems arise because maintaining this
    ;; navigation behavior is not important
    (modify-syntax-entry 91 "_" st)          ; put '[' in class symbol (_)
    (modify-syntax-entry 93 "_" st)          ; put ']' in class symbol (_)
    ;; ditto for '(' and ')' for jumping text like (6)
    (modify-syntax-entry 40 "_" st)          ; put '(' in class symbol (_)
    (modify-syntax-entry 41 "_" st)          ; put ')' in class symbol (_)
    st)                                      ; reveal
  "Syntax table used while in `xumber-mode'.")

;; ---------------------------------
;;  development only
;; ---------------------------------

;;; TOFIX: 01-Aug-2010: make message non-centered

(defun xumber-open-messages ()
  "Convenient way of opening *Messages* buffer."
  (interactive)
  (let* ((buffer-name  "*Messages*"                  )
         (w1           (selected-window)             )
         (w2           (split-window-vertically -10) ))
    (other-window 1)
    (switch-to-buffer buffer-name)
    (other-window 1)
    (message "  xumber: open message complete: %S and %S." w1 w2)))

(defun xumber-display-status-buffer (lines)
  "Display and update the status buffer for LINES."
  ;; inspiration: the 'checkdoc' function `checkdoc-display-status-buffer'
  (let ((buffer-tally       xumber-sum-buffer     )
        (buffer-hx          xumber-history-buffer )
        (wmh-prior          window-min-height     )
        (window-min-height  (1+ (length lines))   ))   ; the modeline, if present, counts as one line
    ;; record history buffer
    (setq buffer-hx (get-buffer-create buffer-hx))     ; CAUTION: okay for buffer to exist
;;; (princ (car (last lines)) buffer-hx)
    (princ (car (nth 0 lines)) buffer-hx)              ; zero-based counting
    (princ "\n" buffer-hx)
    ;; display small buffer
    (with-output-to-temp-buffer buffer-tally
      (dolist (line (reverse lines))
        (princ (car line))
        (princ "\n")))
    (message nil)
    (sit-for 0)
    (shrink-window-if-larger-than-buffer          ; CAUTION: statement must be at or near the end
     (get-buffer-window buffer-tally))
    (setq window-min-height wmh-prior) ))

;; ---------------------------------
;;  number navigate functions
;; ---------------------------------

(defun xumber-move-number-next ()
  "Search for next number using symbol motion and the standard `numberp' predicate."
  (interactive)
  (with-syntax-table xumber-mode-syntax-table
    (let ((symbol    nil )
          (number    nil )
          (count     0   ))
      ;; search
      (forward-symbol +1)                      ; nudge
      (while (and (not (eobp))
                  (not (number-at-point)))
        (forward-symbol +1)
        (incf count))
      ;; completion reporting
      (if (eobp)
          (message "  xumber: find number reached end of buffer.")
        (setq symbol (thing-at-point 'symbol))
        (setq number (number-at-point))
        (message "  xumber: find number complete with %s and %f" symbol number)) )))

(defun xumber-move-number-prior ()
  "Search for previous number using symbol motion and the standard `numberp' predicate."
  (interactive)
  (with-syntax-table xumber-mode-syntax-table
    (let ((symbol    nil )
          (number    nil )
          (count     0   ))
      ;; search
      (forward-symbol -2)                      ; nudge
      (while (and (not (bobp))
                  (not (number-at-point)))
        (forward-symbol -1)
        (incf count))
      (forward-symbol +1)
      ;; completion reporting
      (if (bobp)
          (message "  xumber: find number reached start of buffer.")
        (setq symbol (thing-at-point 'symbol))
        (setq number (number-at-point))
        (message "  xumber: find number complete with %s and %f" symbol number)) )))

;; ---------------------------------
;;  number search functions
;; ---------------------------------

(defun xumber-find-float-000 ()
  "Search for -000.0e-00, that is with three leading digits."
  (interactive)
  (let ((regex  "\\<[[:digit:]]\\{3\\}\\.0e[-+][[:digit:]]+\\>" ))
    (if (re-search-forward regex)
        (forward-char -0))                   ; backup 0 chars
    (message "  xumber: find-float complete using regex '%s' and match '%s'."
             regex (match-string 0))))

;; see also 'calculator.el' and (calculator-eng-display)

(defun xumber-find-float-noneng ()
  "Search for -0.0e-02 and similar non-engineering exponents."
  (interactive)
  (let ((regex  "\\<[-+]?[[:digit:]]+\\.e[-+]0[124578]\\|e[-+]1[0134679]\\>" ))
    (if (re-search-forward regex)
        (forward-char -0))
    (message "  xumber: find-float complete using regex '%s' and match '%s'."
             regex (match-string 0))))

(defun xumber-find-float-decimal ()
  "Search for decimal notion with decimal point."
  (interactive)
  (let ((regex  "\\<[-+]?[[:digit:]]+\\.[[:digit:]]+\\>" ))
    (if (re-search-forward regex)
        (forward-char -0))
    (message "  xumber: find-float complete using regex '%s' and match '%s'."
             regex (match-string 0))))

(defun xumber-find-integer ()
  "Search for integer."
  (interactive)
  (let ((regex  "\\<[-+]?[[:digit:]]+\\>" ))
    (if (re-search-forward regex)
        (forward-char 0))
    (message "  xumber: find-integer complete using regex '%s' and match '%s'."
             regex (match-string 0))))

;; ---------------------------------
;;  number display and format functions
;; ---------------------------------

;; CAUTION: setting the sum to "0.0" provokes non-integer status
;; from the outset

(defvar xumber-number-sum   0 "Xumber tally sum.")
(defvar xumber-number-cnt   0 "Xumber tally count.")

(defun xumber-sum-zero ()
  "Reset tally."
  (interactive)
  (let ((msg1  nil )
        (msgs  ()  ))
    (setq xumber-number-sum  0)
    (setq xumber-number-cnt  0)
    (setq msg1 (format"  xumber: sum-zero (tally %d) : %g" xumber-number-cnt xumber-number-sum))
    (message "%s" msg1)
    (push (list msg1) msgs)
    (xumber-display-status-buffer msgs) ))

(defun xumber-capture-current-number (delete)
  "Attempt to capture current number.  And also delete it if DELETE is true.

Returns a string if something sensible found, otherwise nil."
  ;; NOTE: see commit r4944 for previous "nibble" code which hunted
  ;; for space and newline chars and was platform-specific
  (with-syntax-table xumber-mode-syntax-table
    (let ((capture  nil )                      ; type string
          (number   nil )                      ; type number
          (bnds     ()  )                      ; beginning and end positions of symbol
          (beg          )                      ; beginning of symbol
          (end          )                      ; end of symbol
          (size         ))                     ; size of symbol
      ;; CAUTION: requires variable `xumber-mode-syntax-table' which
      ;; puts the dot (.) in the current class symbol (_)
      (setq capture (thing-at-point 'symbol))
      (setq bnds (bounds-of-thing-at-point 'symbol))     ; returns cons cell (START . END)
      (setq beg (car bnds))
      (setq end (cdr bnds))
      (setq size (- end beg))
      (message "  xumber: capture-current-number: size %d" size)
      (setq capture (buffer-substring-no-properties beg end))
      (message "  xumber: capture-current-number: capture '%s'" capture)
      (if (> (length capture) 0)
          (progn
            (setq number (string-to-number capture))
            (message "  xumber: capture-current-number: number %d" number)
            (if delete
                (progn
                  (delete-region beg end)
                  (message "  xumber: capture-current-number: complete including deletion"))
              (message "  xumber: capture-current-number: complete excluding deletion")))
        (message "  xumber: capture-current-number: complete but capture failed"))
      capture)))

(defun xumber-show-number ()
  "Show current number if potentially more readable format."
  (interactive)
  (save-excursion
    (let* ((capture  (xumber-capture-current-number nil) )  ; returns a string or null,
           (output   ""                                  )  ; .. 'nil' means do not delete
           (msg      ()                                  )
           (msgs     ()                                  )
           (msg1     nil                                 )
           (msg2     nil                                 ))
      (if capture
          (progn
            (setq number (string-to-number capture))
            (cond
             ;; integer
             ((integerp number)
              (cond
               ;; big integer
               ((> (abs number) 9999)
                (setq output (format "%s = %s ~ %s"
                                     (format "%i" number)
                                     (xumber-comma-ize number)
                                     (xumber-kmg-ize number))))
               ;; remaining integer
               (t
                (setq output (format "%i" number)))))
             ;; float
             (t
              (cond
               ;; big float
               ((> (abs number) 999999.0)
                (setq output (format "%s = %s ~ %s"
                                     (format "%e" number)
                                     (xumber-comma-ize number)
                                     (xumber-kmg-ize number))))
               ;; medium float
               ((> (abs number)      9.0)
                (setq output (format "%s = %s ~ %s"
                                     (replace-regexp-in-string
                                      "\\.$" ""
                                      (replace-regexp-in-string "0+$" "" (format "%f" number)))
                                     (xumber-comma-ize number)
                                     (xumber-kmg-ize number))))
               ;; zero
               ((= (abs number)      0.0)
                (setq output "zero"))

               ;; tiny float
               ((< (abs number)      0.00001)
                (setq output (format "%s (perhaps %s $/GJ or %s c/kWh)"
                                     (format "%e" number)
                                     (format "%.0f" (/ number 1.0e-9))          ; $/GJ
                                     (format "%.0f" (/ number 0.2778e-8)))))    ; c/kWh (3.6^-1)
               ;; small float
               ((< (abs number)      0.01)
                (setq output (format "%s"
                                     (format "%f" number))))
               ;; remaining (1% to 100%)
               (t
                (setq output (format "%s = %s"
                                     (replace-regexp-in-string
                                      "\\.$" ""
                                      (replace-regexp-in-string "0+$" "" (format "%f" number)))
                                     (format "%.0f%%" (* number 100))))))))
            ;; update tally
            (setq xumber-number-sum (+ xumber-number-sum number))
            (setq xumber-number-cnt (1+ xumber-number-cnt))
            ;; show tally
            (setq sum xumber-number-sum)
            (setq cnt xumber-number-cnt)
            (if (<= number 1.0e-12)          ; loss of precision on comma-ize under e-12
                (setq msg1 (format "  xumber: show number (precision loss) : %s = %s" capture output))
              (setq msg1 (format "  xumber: show number                  : %s = %s" capture output)))
            (if (integerp sum)               ; elisp automatically switches to float when encountered
                (setq msg2 (format "  xumber: show number (tally %02d)       : %d"
                                   cnt sum))
              (setq msg2 (format "  xumber: show number (tally %02d)       : %+.2e" cnt
                                 sum)))
            (message "%s\n%s" msg1 msg2)          ; duplicate as message
            (push (list msg2) msgs)
            (push (list msg1) msgs)
            (xumber-display-status-buffer msgs))  ; CAUTION: must come late
        ;; completion reporting
        (message "  xumber: show number: nothing sensible to show (check cursor location).")) )))

(defun xumber-reformat-number ()
  "Capture, reformat, and reinsert current number."
  (interactive)
  (let ((capture    nil )                    ; return from `xumber-capture-current-number'
        (number     nil )                    ; type number
        (size           )                    ; size of number as string
        (reformat       )                    ; return from `xumber-insert-number'
        (recapture      ))                   ; converted return for validation against 'capture'
    (setq capture (xumber-capture-current-number t))
    (if (not (null capture))
        (progn
          (setq size (length capture))
          (setq number (string-to-number capture))
          (setq reformat (xumber-insert-number number))     ; unprompted version of call
          (setq recapture (string-to-number reformat))
          (if (= number recapture)                          ; numerically equal
              (if (string-equal capture reformat)           ; string-wise identical
                  (message "  xumber: reformat number DUPLICATES '%s' as '%s' with original size %d."
                           capture reformat size)
                (message "  xumber: reformat number REFORMATS '%s' as '%s' with original size %d."
                         capture reformat size))
            (message "  xumber: reformat number WARNS ABOUT '%s' as '%s' with original size %d."
                     capture reformat size)))
      (message "  xumber: reformat number quit without action")) ))

(defun xumber-insert-number (number)
  "Ask for and reformat NUMBER according to current XEM input requirements.\n
Makes use of the type prompt, if present."
  (interactive "nEnter number (any format): ")
  (let ((reformat  "(faulty)" )              ; desired reformatting
        (line                 )              ; current line
        (leftright            )              ; as split on <>
        (left                 )              ; left part
        (bits                 )              ; as split on multiple blanks
        (type                 )              ; field type prompt in { f, i } or something else
        (temp nil             ))
    ;; confirm space
    (if (not (or (= (char-before) 32)        ; peek backwards and check for space
                 (= (char-after)  32)))
        (error "  xumber: insert-number: refusing to insert in current position"))
    ;; obtain field type prompt
    (setq line (thing-at-point 'line))       ; includes trailing newline
    (setq leftright (split-string line "[<>]"))
    (setq left (pop leftright))              ; left-side
    (setq bits (split-string left))          ; use default separator "[ \f\t\n\r\v]+"
    (setq type (car (last bits)))
    (setq type (downcase type))
    (message "  xumber: insert-number recovered type '%s'" type)
    ;; reformat number
    (cond ((and (equal type "i") (integerp number))                   ; integer
           (setq reformat (format "%d" number)))
          ((and (equal type "f") (zerop number))                      ; zero float
           (setq reformat "0.0"))
          ((and (equal type "f") (integerp number) (< number 100))    ; small "integer" float
           (setq reformat (concat (format "%d" number) ".0")))
          ((equal type "f")                                           ; float
           (setq reformat (format "%+e" number)))                     ; remove "+" to remove leading +
          (t                                                          ; fall-thru
           (setq reformat "")))
    ;; attempt to suppress some zeros
;;; (while (string-match "000" reformat)
;;;   (setq reformat (replace-match "00" t t reformat)))
    (setq temp reformat)
    (while (string-match "00" temp)
      (setq temp (replace-match "0" t t temp)))
    (if (= (string-to-number temp) number)   ; numerically equal
        (setq reformat temp))                ; accept the suppression
    ;; insert
    (if (not (= (char-before) 32)) (insert " "))
    (insert reformat)
    (if (not (= (char-after) 32)) (insert " "))
    ;; completion reporting
    (message "  xumber: insert-number complete using '%f' and '%s'." number reformat)
    reformat))

(defun xumber-comma-ize (number)
  "Comma-ise the (float or integral) NUMBER and return it as a string.\n
Does not truncate 1.2345e-12, but will with slightly smaller numbers.
Large numbers are not a problem."
  ;; comma-ization code base on: http://www.emacswiki.org/emacs/sunrise-commander.el
  (let* ((buffer  (format "%.16f" (abs number))                      ) ; stringify without sign [1]
         (parts   (split-string buffer "\\." t)                      ) ; split decimal
         (intg    (pop parts)                                        ) ; integral part left of decimal
         (frac    (pop parts)                                        ) ; fractional part right of
         (frac    (if (zerop (string-to-number frac)) nil frac)      ) ; ..  decimal, else null
         (frac    (if frac (replace-regexp-in-string "0+$" "" frac)) ) ; trim trailing zeros
         (sign    (if (< number 0) "-" "")                           ) ; set sign (could also add "+")
         (digits  (reverse (split-string intg "" t))                 )
         (result   nil                                               ))
    (message "\nbuffer : %s\nintg   : %s\nfrac   : %s\nsign   : %s" buffer intg frac sign)
    (dotimes (n (length digits) result)                ; comma-ize code
      (if (and (< 0 n) (zerop (% n 3)))
          (setq result (concat "," result)))
      (setq result (concat (pop digits) result)))
    (if frac (setq result (concat result "." frac)))   ; reinstate fractional part
    (setq result (concat sign result)) ))              ; reinstate sign
    ;; [1] was `number-to-string'

(defun xumber-kmg-ize (number)
  "kMG-ise the (float or integral) NUMBER and return it as a string."
  ;; some of this code may be redundant because this function is
  ;; only called on largish numbers -- but the effort of
  ;; refactoring was not really justified
  (let* ((buffer    (format "%.16f" (abs number))                      )   ; stringify without sign [1]
         (parts     (split-string buffer "\\." t)                      )   ; split decimal
         (intg      (pop parts)                                        )   ; integral part left of decimal
         (frac      (pop parts)                                        )   ; fractional part right of
         (frac      (if (zerop (string-to-number frac)) nil frac)      )   ; ..  decimal, else null
         (frac      (if frac (replace-regexp-in-string "0+$" "" frac)) )   ; trim trailing zeros
         (sign      (if (< number 0) "-" "")                           )   ; set sign (could also add "+")
         (digits    (reverse (split-string intg "" t))                 )
         (result     nil                                               )   ; return string
         (commacnt  0                                                  )   ; number of commas if comma-ized
         (prefs    (list "" "k" "M" "G" "T" "P" "E" "Z")               )   ; SI prefixes, starting 10^0
         (pref      nil                                                ))  ; selected prefix
    (message "\nbuffer : %s\nintg   : %s\nfrac   : %s\nsign   : %s" buffer intg frac sign)
    ;; determine prefix
    (dotimes (n (length digits) result)                ; kMG-ize code
      (if (and (< 0 n) (zerop (% n 3)))
          (incf commacnt)))
    (setq pref (nth commacnt prefs))
    ;; arithmetic
    (setq reduce (/ number (expt 10 (* commacnt 3))))  ; sign is present here in any case
    (setq trunci  (round reduce))
    ;; assemble
    (setq result (format "%d" trunci))
    (setq result (concat result pref)) ))

(defun xumber-comma-ize-original (number)    ; CAUTION: not used
  "Comma-ise the (float or integral) NUMBER and return it as a string."
  ;; source: http://www.emacswiki.org/emacs/sunrise-commander.el
  ;; CAUTION: takes floats but chops the fractional part and fails on some negatives
  (let* ((num     (replace-regexp-in-string "\\..*$" "" (number-to-string number)) )
         (digits  (reverse (split-string num "" t))                                )
         (result   nil                                                             ))
    (dotimes (n (length digits) result)
      (if (and (< 0 n) (zerop (% n 3)))
          (setq result (concat "," result)))
      (setq result (concat (pop digits) result))) ))

(defun xumber-switch-history-buffer ()
  "Switch to tally history buffer and back."
  (interactive)
  (let ((target  (get-buffer xumber-history-buffer) ))
    (if (not (null target))
        (if (not (eq (current-buffer) target))    ; 'eq' for same lisp object
            (progn
              (switch-to-buffer target)
              (xumber-mode t))                    ; enable this function and associated key map
          (switch-to-buffer (other-buffer)))
      (message "  xumber: switch history buffer cannot switch as buffer '%s' has not been created."
               xumber-history-buffer)) ))

;; ---------------------------------
;;  minor mode
;; ---------------------------------

;; CAUTION: I believe the `xumber-mode-map' definitions
;; need to precede `xumber-mode' definition

;;;###autoload(defvar xumber-mode nil)
(define-minor-mode xumber-mode
  "Minor mode for processing numbers in plain text files.

Xumber mode provides functions to extract and manipulate numbers
in plain text.  Xumber mode is designed to compliment the Xem and
Xeona major modes.

With no argument, this command toggles the mode.  A non-null
prefix argument turns on the mode.  A null prefix argument turns
off the mode."

  ;; common items

  :init-value nil
  :lighter " Xumber"                         ; modeline lighter
  :keymap nil
  :global nil                                ; nil means buffer-local minor mode
  :group 'xumber
  :version nil                               ; emacs version (and not mode version)

  ;; code

  (message "  xumber: xumber-mode: commencing")
  (if xumber-mode
      (message "  xumber: toggle on mode")
    (message "  xumber: toggle off mode"))

  ) ; final parenthesis

;; ---------------------------------
;;  provide
;; ---------------------------------

;; put the mode symbol into the list "features", so that users can
;; invoke (require 'xumber-mode) and load your code only when needed

(provide 'xumber-mode)

;;; xumber.el ends here

;  $Id: xumber.el 7835 2011-10-31 08:40:15Z robbie $
;  end of file

