;;; xrstat.el --- create and call R stub programs for visualization

;  file-purpose     : emacs create and call R stub programs for visualization
;  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
;  file-create-date : Thu 12-Aug-2010 22:07 UTC
;  file-status      : ongoing
;  file-keywords    : emacs xeona

;  $Revision: 7071 $
;  $Date: 2011-09-06 14:46:21 +0200 (Tue, 06 Sep 2011) $
;  $Author: robbie $
;  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/elisp/xrstat.el $

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

;;  TODOs
;;
;;      > hold open displays (they flash for an instant) [1]
;;      > xrstat-mode with line highlights for "^>" and "^xeona"
;;
;;      [1] either set "par(ask = TRUE)" or solved elsewhere by
;;      (1) displaying a PDF output file instead, (2) using a
;;      Tcl/Tk event button, (3) embedding the plot in a Tcl/Tk
;;      window (requires a non-standard R package), or (4) adding
;;      a 'readLines' call for user input (not as convenient as
;;      the user has to find the terminal)
;;
;;  Known issues
;;
;;  Background
;;
;;      reference: An Introduction to R / B.4 Scripting with R
;;      reference: An Introduction to R / B.1 Invoking R from the command line
;;      web: http://cran.r-project.org/doc/manuals/R-intro.html
;;
;;  Approaches
;;
;;      there appear to be several approaches, whereby 'file.R'
;;      contains <expressions>
;;
;;          $ R -e <expression> -e <expression>
;;          $ echo <expressions> | R --file -
;;          $ R --file temp.R
;;          $ R CMD BATCH temp.R
;;          $ Rscript temp.R
;;
;;  Typical consolidated command
;;
;;      R --no-save --no-restore --quiet --verbose
;;        -e 'source("~/synk/xeona/svn2/futz/trunk/xeonar/xem.R")'
;;        -e 'xem.job("~/synk/xeona/svn2/futz/trunk/models/test-16.xem")'

;;; History:

;;      * see the subversion log messages

;;; Code:

;; =================================
;;  xem support
;; =================================

(defconst xem-r-dir (if (getenv "XEONAR")
                        (getenv "XEONAR")
                      (concat "/home/robbie/synk/xeona/svn2/futz/trunk/xeonar/")))

(defconst xem-buffer-r-capture "*r-capture*" "Buffer for R output capture.")

;; ---------------------------------
;;  switcher function
;; ---------------------------------

(defun xem-r-run-switch (start end)
  "Switch between entire buffer and region-bounded action."
  (interactive "r")
  (message "  xem-r-run-switch: entering function: start %s end %s" start end)
  (if (use-region-p)
      (xem-r-run-reg start end)
    (xem-r-run)) )

;; ---------------------------------
;;  R program call
;; ---------------------------------

(defun xem-r-run ()
  "Run the 'xem.R' program on the current buffer."
  (interactive)
  (let* ((bm        (current-buffer)                         )   ; xem file buffer
         (bc        (get-buffer-create xem-buffer-r-capture) )   ; output capture buffer
         (target    (buffer-file-name nil)                   )   ; file associated with current buffer
         (binary    xem-binary                               )   ; xeona binary
         (xeona     nil                                      )   ; xeona binary with quotes or NULL
         (rbin      "R"                                      )
         (rdir      xem-r-dir                                )
         (rscript   "xem.R"                                  )
         (dbug      1                                        )   ; debug reporting in { 0 1 2 3 4 }
         (rfile     nil                                      )   ; rdir + rscript
         (jargs     ()                                       )
         (jarg      nil                                      )   ; xem.job arguments (concatenated)
         (rcmds     ()                                       )
         (rcmd      nil                                      )   ; R program (concatenated)
         (ropts     ()                                       )
         (ropt      nil                                      )   ; R options (concatenated)
         (rcall     nil                                      )
         (code      nil                                      ))
    ;; outset reporting (because this is a long set of procedures)
    (message "  xem: r-run: commencing with target '%s'" target)
    ;; check if run -- otherwise the R code errors out as follows:
    ;;       info : xem.job : plot list job starts
    ;;     Error in xy.coords(x, y, xlabel, ylabel, log) :
    ;;       'x' and 'y' lengths differ
    ;;     Calls: xem.job -> xem.plotValue -> plot -> plot.default -> xy.coords
    (if (xem-have-i-run)                     ; active call
        (message "  xem: r-run: buffer identified as having run")
      (error "  xem: r-run: error: buffer identified as NOT having run"))
    ;; identify model or guard and set 'xeona' appropriately
    (setq binary (abbreviate-file-name binary))
    (if (string-match "\\.guard\\.xem$" target)
        (setq xeona (concat "\"" binary "\""))
      (setq xeona "NULL"))                                   ; CAUTION: no quoting
    ;; assemble R script name
    (if (not (string-equal (substring rdir -1 nil) "/"))     ; cannot guarantee a trailing slash
        (setq rdir (concat rdir "/")))
    (setq rfile (concat rdir rscript))
    (setq rfile (abbreviate-file-name rfile))
    ;; create 'job' call
    (setq target (abbreviate-file-name target))
    (push (concat "xemfile=\"" target "\""              ) jargs)
    (push (concat "xeona=" xeona                        ) jargs)
    (push (concat "dbug=\"" (number-to-string dbug) "\"") jargs)
    (setq jarg (xem-stringify-list jargs ", "))
    ;; create "program"
    (push (concat "-e " "'source(\"" rfile "\")'" ) rcmds)  ; load script command
    (push (concat "-e " "'xem.job(" jarg ")'"     ) rcmds)  ; run function command
    (setq rcmd (xem-stringify-list rcmds))
    (message "  xem: r run: rcmd '%s'" rcmd)
    ;; R and R options
    (push "--no-save"     ropts) ; do NOT save workspace at the end of the session
    (push "--no-restore"  ropts) ; do NOT restore previously saved objects, the R history file, et al
;;; (push "--vanilla"     ropts) ; --no-save, --no-restore, --no-site-file, --no-init-file --no-environ
    (push "--quiet"       ropts) ; omit startup message
;;; (push "--slave"       ropts) ; make R run as quietly as possible
;;; (push "--verbose"     ropts) ; print more information about progress
;;; (push "--interactive" ropts) ; CAUTION: not required
;;; (setq ropt (combine-and-quote-strings (reverse ropts)))
    (setq ropt (xem-stringify-list ropts))
    (message "  xem: r run: ropt '%s'" ropt)
    ;; assemble call
    (setq rcall (concat rbin " " ropt " " rcmd))
    (message "  xem: r run: rcall '%s'" rcall)
    ;; confirm R
    (setq code (shell-command "R --version"))
    (sit-for 1)
    (delete-other-windows)
    (cond
     ((= code   0) (message "  xem: r-run: R command found"))
     ((= code 127) (error "  xem: r-run: R command not found, expected exit code %d" code))
     (t            (error "  xem: r-run: R command unexpected exit code %d" code)))
    ;; confirm R script
    (access-file rfile "  xem: r-call: abandoning task without action")
    (message "  xem: r run: access file passed '%s'" rfile)
    ;; prompted save if required ('bm' assumed here)
    (if (buffer-modified-p)
        (if (y-or-n-p "Save this model in order to continue? ")
            (save-buffer)
          (error "  xem: r run call abandoning '%s' R call without action" rcall)))
    (save-buffer)                                       ; defensive programming, not really needed
    ;; prepare output buffer
    (set-buffer bc)                                     ; 'bc' is the capture buffer
    (xrstat-mode t)                                     ; minor mode, 't' is activate
    (erase-buffer)                                      ; delete entire contents, ignores any narrowing
    (message "  xem: r run: buffer '%s' erased" (buffer-name bc))
;;; (error "  xem: r-call: deliberate exit")
    ;; run command
    (setq code (shell-command rcall bc nil))            ; 'nil' means mingle stdout and stderr
    (message "  xem: r-call: R ran synchronously and returned %s" code)
    (xrstat-summarize-buffer bc)                        ; create a summary
    (message "  xem: r call = '%s'\n       r exit = %d" rcall code) ))

;; ---------------------------------
;;  utilities
;; ---------------------------------

(defun xem-stringify-list (slist &optional sep)
  "Stringify string SLIST using optional SEP if provided, else \" \"."
  (let* ((buffer  "" ))                           ; returns an empty string if 'slist' is nil
    (if (not sep) (setq sep " "))
    (message "  xem: stringify-list: sep '%s'" sep)
    (message "  xem: stringify-list: length %d" (length sep))
;   (message "  xem: stringify-list: slist '%s'" slist)
    (message "  xem: stringify-list: length %d" (length slist))
    (when slist
      (dolist (s (reverse slist))
        (setq buffer (concat buffer sep s)))  ; CAUTION: 'car' not need on 's'
      (setq buffer (substring buffer (length sep) nil)))
    buffer ))                                     ; expose return value

;; ---------------------------------
;;  open plot window files
;; ---------------------------------

;; typical viewers (bad results)
;;
;;   png : display emacs eog gthumb
;;   pdf : acroread emacs evince
;;   svg : (display) eog evince firefox gthumb inkscape
;;
;;   notes - eog gets confused with multiple files

(defun xem-r-pngs ()
  "Display any PNG files associated with the current model."
  (interactive)
  (let* ((bc    (current-buffer)       )
         (call  "gthumb --fullscreen"  )     ; works well for dedicated viewing
         (extn  "png"                  ))
    (message "  xem: r-pngs: extn = %s  buffer = %s  call = %s" extn (buffer-name bc) call)
    (xrstat-files bc call extn)
    (message "  xem: r-pngs: complete using extension '%s' and call '%s'" extn call) ))

(defun xem-r-pdfs ()
  "Display any PDF files associated with the current model."
  (interactive)
  (let* ((bc    (current-buffer)       )
         (call  "evince --fullscreen"  )
         (extn  "pdf"                  ))
    (xrstat-files bc call extn)
    (message "  xem: r-pdfs: complete using extension '%s' and call '%s'" extn call) ))

(defun xem-r-svgs ()
  "Display any SVG files associated with the current model."
  (interactive)
  (let* ((bc    (current-buffer)       )
         (call  "gthumb --fullscreen"  )
         (extn  "svg"                  ))
    (xrstat-files bc call extn)
    (message "  xem: r-svgs: complete using extension '%s' and call '%s'" extn call) ))

(defun xrstat-files (buffer                   ; lisp buffer object
                     util                     ; viewer utility call string
                     ext)                     ; file extension without dot
  "View files associated with BUFFER using call UTIL and filtering on extension EXT."
  ;; CAUTION: this code breaks if matched filenames contain
  ;; whitespace (which should not be the case)
  (let* ((model    (buffer-file-name buffer)                      )
         (stub     (xrstat-stub-me model)                         )   ; grab stub name
         (pattern  (concat stub ".*." ext)                        )
         (files    (file-expand-wildcards pattern t)              )   ; used to confirm some files
         (call     (concat util " " "$(ls -tr " pattern ") " "&") )   ; NOTE: "&" for asynchronous call
         (code     nil                                            ))
    (if files
        (progn
          (message "  xrstat: files: hits = %d" (length files))
          (message "  xrstat: files: matching files:")
          (dolist (file files)
            (message "    %s" (file-name-nondirectory file)))
          (message "  xrstat: files: call = '%s'" call)
          (shell-command call nil nil)      ; CAUTION: no return status for asynchronous call
          (message "  xrstat: files: complete with base call '%s'." call))
      (message "  xrstat: files: no files match wildcard pattern '%s'." pattern)) ))

(defun xrstat-stub-me (filename)                ; normal leaf, relative, or absolute
  "Stub the FILENAME."
  (let* ((filename  (abbreviate-file-name     filename) )
         (buf-1     (file-name-sans-extension filename) )
         (ext-1     (file-name-extension      filename) )   ; should be "xem"
         (buf-2     (file-name-sans-extension buf-1)    )
         (ext-2     (file-name-extension      buf-1)    )   ; could possibly be "guard"
         (ret       nil                                 ))
    (cond
     ((string-equal "guard" ext-2) (setq ret buf-2))
     ((string-equal "xem"   ext-1) (setq ret buf-1))
     (t                            (setq ret filename)))
    (message "  xrstat: stub-me: processing '%s' to '%s'" filename ret)
    ret ))                                   ; stubbed filename

;; ---------------------------------
;;  provide
;; ---------------------------------

(provide 'xrstat)

;; =================================
;;  xrstat-mode minor mode
;; =================================

;; ---------------------------------
;;  requirements
;; ---------------------------------

(require 'hi-lock)                           ; "minor mode for interactive automatic highlighting"

;; ---------------------------------
;;  key map
;; ---------------------------------

;; CAUTION: `xrstat-mode-map' should precede `define-minor-mode'

(defvar xrstat-mode-map nil "Local keymap for Rifs mode buffers.")

(if xrstat-mode-map
    ()                                       ; protection against double loading
  ;; key bindings
  (setq xrstat-mode-map (make-sparse-keymap))
  (define-key xrstat-mode-map [(shift           up)] 'xrstat-move-regex-prior)
  (define-key xrstat-mode-map [(shift         down)] 'xrstat-move-regex-next))

;; ---------------------------------
;;  minor mode
;; ---------------------------------

;;;###autoload(defvar xrstat-mode nil)
(define-minor-mode xrstat-mode
  "Minor mode for displaying R output capture.

This mode highlights R calls and highlights warnings and errors,
enables navigation, and offers a summarize function.
Key-bindings include:

  next     : \\[xrstat-move-regex-next]
  previous : \\[xrstat-move-regex-prior]

With no argument, this command toggles the mode.  A non-null
prefix argument turns on the mode.  A null prefix argument turns
off the mode."

  ;; common items

  :init-value nil
  :lighter " Xrstat"                         ; modeline lighter
  :keymap nil
  :global nil                                ; nil means buffer-local minor mode
  :group 'xrstat
  :version nil                               ; emacs version (and not mode version)

  ;; code

  (message "  xrstat: xrstat-mode: commencing")
  (if xrstat-mode
      (progn
        (visual-line-mode t)                 ; employ visual not logical lines, activate `word-wrap'
        (toggle-truncate-lines 1)
        (xrstat-colorize)
        (message "  xrstat: toggle on mode"))
    (progn
      (visual-line-mode nil)
      (toggle-truncate-lines -1)
      (hi-lock-mode nil)
      (message "  xrstat: toggle off mode")))

  ) ; final parenthesis

;; ---------------------------------
;;  navigation
;; ---------------------------------

(defconst xrstat-move-regex
  "Warning message\\|warn\\|error\\|Error\\|ERROR"
  "Xrstat move regex.")

(defconst xrstat-recenter
  nil                                        ; useful values: 0, 1, nil (to center)
  "Xrstat navigation recenter behavior.")

(defun xrstat-move-regex-next ()
  "Navigate to next regex."
  (interactive)
  (let ((regex  xrstat-move-regex ))
    (end-of-line)                            ; need to "kick-start" the search

    (if (re-search-forward regex
                           nil t)            ; limit (no), no error (true)
        (progn
          (beginning-of-line)
          (recenter xrstat-recenter)
          (beginning-of-line)
          (message "  xrstat: move to next regex complete."))
      (goto-char (point-max))
      (message "  xrstat: move to next regex FAILED."))
    (match-beginning 0) ))                   ; nil if no match

(defun xrstat-move-regex-prior ()
  "Navigate to previous regex."
  (interactive)
  (let ((regex  xrstat-move-regex ))
    (if (re-search-backward regex
                            nil t)           ; limit (no), no error (true)
        (progn
          (beginning-of-line)
          (recenter xrstat-recenter)
          (beginning-of-line)
          (message "  xrstat: move to previous regex complete."))
      (goto-char (point-min))
      (message "  xrstat: move to previous regex FAILED."))
    (match-beginning 0) ))                   ; nil if no match

;; ---------------------------------
;;  highlight and summarize
;; ---------------------------------

(defface xrstat-color-B01
  '((((background dark)) (:background "sienna1" :foreground "black"))
    (t (:background "sienna1")))
  "Private face.")

(defface xrstat-color-B02
  '((((background dark)) (:background "DarkGoldenrod1" :foreground "black"))
    (t (:background "DarkGoldenrod1")))
  "Private face.")

(defface xrstat-color-B03
  '((((background dark)) (:background "DarkGoldenrod1" :foreground "black"))
    (t (:background "DarkGoldenrod1")))
  "Private face.")

(defface xrstat-color-B04
  '((((background dark)) (:background "red" :foreground "black"))
    (t (:background "red")))
  "Private face.")

(defun xrstat-colorize ()
  "Highlight key lines and phrases."
  (interactive)
  (let* ((regex-1  "^> ."                  )
         (regex-2  "Warning message"       )
         (regex-3  "warn"                  )
         (regex-4  "error\\|Error\\|ERROR" ))
    (highlight-lines-matching-regexp regex-1 'xrstat-color-B01)
    (highlight-phrase                regex-2 'xrstat-color-B02)
    (highlight-phrase                regex-3 'xrstat-color-B03)
    (highlight-lines-matching-regexp regex-4 'xrstat-color-B04)
    (message "  xrstat: colorize: complete") ))

(defun xrstat-summarize-buffer (buffer)
  "Summarize r capture in given BUFFER."
  (interactive)
  (let* ((bname (buffer-name buffer)        )
         (regex-2  "Warning message"        )
         (regex-3  "warn"                   )
         (regex-4  "error\\|Error\\|ERROR"  )
         (regex-5  "invalid graphics state" ))
    (goto-char (point-min))
    (insert "              capture status\n")
    (insert (xrstat-format-occurs "R  w a r n"            regex-2))
    (insert (xrstat-format-occurs "xeona  w a r n"        regex-3))
    (insert (xrstat-format-occurs "all  e r r o r"        regex-4))
    (insert (xrstat-format-occurs "invalid graphic state" regex-5))
    (insert "\n")
    (message "  xrstat: summarize-buffer: complete with '%s'." bname) ))

(defun xrstat-format-occurs (tag regex)
  "Report TAG described occurrences of REGEX in current buffer."
  (save-excursion
    (let* ((line (format "  %22s : %d\n" tag (xrstat-count-occurs regex)) ))
      line )))

(defun xrstat-count-occurs (regex)
  "Count occurrences of REGEX in current buffer."
  (let* ((prior             case-fold-search )
         (case-fold-search  nil              )    ; non-nil means ignore case
         (count             0                ))
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (incf count))
    (setq case-fold-search prior)
    (message "  xrstat: count-occurs: regex '%s' occurred %d times" regex count)
    count ))

;; ---------------------------------
;;  part-model network plots
;; ---------------------------------

(defun xem-r-run-reg (start end)             ; values seem to stick after region is canceled
  "Network plot model in region bounded START and END."
  (interactive "r")
  (save-excursion
    (let* ((xem-file     (buffer-file-name nil)              )   ; model name
           (xem-stub     (file-name-sans-extension xem-file) )
           (buffer-tag   "part"                              )   ; additional tag
           (buffer-stub  (concat xem-file "." buffer-tag)    )   ; create stub
           (buffer-extn  "xem"                               )
           (buffername   nil                                 )
           (text-1       nil                                 )
           (text-2       nil                                 )
           (text-3       nil                                 ))
      ;; preamble
      (message "  xem-r-run-reg ------------------------------------------------------------------")
      (if (not (use-region-p))               ; note also 'region-active-p'
          (message "  xem-r-run-reg: use-region predicate returned nil"))
      (message "  xem-r-run-reg: start %d end %d" start end)
      ;; grab texts
      (setq text-0 "\nnote\n\n    generated file\n")
      (setq text-1 (xem-stringify-list (xem-rrunreg-opening) "\n"))
      (setq text-2 (buffer-substring start end))
      (setq text-3 (xem-stringify-list (xem-rrunreg-closing) "\n"))
      ;; open temporary buffer to hold the truncated model
      (setq buffer-name (generate-new-buffer-name (concat buffer-stub "." buffer-extn)))
      (message "  xem-r-run-reg: buffer-name: %s" buffer-name)
      (set-buffer (generate-new-buffer buffer-name))    ; CAUTION: any <2> tag only applies to the buffer and not the file
      (xem-mode)
      ;; reassemble
      (insert text-0)
      (insert text-1)
      (insert text-2)
      (insert text-3)
      ;; write entire buffer
      (write-file buffer-name)
      (message "  xem-r-run-reg: about to call 'xem-r-run'")
      (xem-r-run)
      ;; housekeeping
      (message "  xem: r-run-reg: complete using buffer '%s'." buffer-name) )))

(defun xem-rrunreg-opening ()
  "Capture and modify a necessary subset of the 'program admin' records."
  (let* ((policy       24          )           ; 'program.r-processing.r-policy' value (normally 24)
         (svn-value    -1          )           ; 'program.last-run.used-svn' value (both zero and -1 is okay)
         (extra        " (subset)" )           ; appended to 'r-title' (can be empty string)
         (policy-line  nil         )
         (line         nil         )           ; captured line
         (lines        ()          )           ; list of lines to be returned
         (left         nil         )
         (right        nil         )
         (regex-1      nil         )
         (regex-2      nil         )
         (regex-3      nil         )
         (regex-4      nil         )
         (pos-1        nil         )
         (pos-2        nil         )
         (pos-3        nil         )
         (pos-4        nil         ))
    ;; create r-policy line
    (setq policy-line (format "    r-policy i                                    > %d" policy))
    ;; seek 'program.last-run.used-svn' value
    (setq regex-1 "^program.last-run$")
    (setq regex-2 "^[[:blank:]]+used-svn.*<")
    (goto-char (point-min))                                           ; go to beginning of accessible portion
    (if (setq pos-1 (re-search-forward regex-1 nil t))
        (if (setq pos-2 (re-search-forward regex-2 nil t))
            (progn
              (setq line (thing-at-point 'line))
              ;; split field
              (setq parts (split-string line "<" nil))                ; 'nil' is accept null substrings
              (setq left  (nth 0 parts))
              (setq line (concat left "<" " " (format "%d" svn-value)))
              (push ""                                     lines)
              (push "program.last-run"                     lines)
              (push ""                                     lines)
              (push line                                   lines)
              (push ""                                     lines))))
    (message "  xem-rrunreg-opening: positions : %s %s" pos-1 pos-2)  ; "%s" is okay
    (message "  xem-rrunreg-opening: line: %s" line)
    ;; seek 'program.r-processing.r-title' value
    (setq regex-3 "^program.r-processing$")
    (setq regex-4 "^[[:blank:]]+r-title.*>")
    (goto-char (point-min))                                           ; go to beginning of accessible portion
    (if (setq pos-3 (re-search-forward regex-3 nil t))
        (if (setq pos-4 (re-search-forward regex-4 nil t))
            (progn
              (setq line (thing-at-point 'line))
              (setq line (replace-regexp-in-string "\n$" "" line))    ; trim trailing newline
              (setq line (replace-regexp-in-string "\"$" "" line))    ; trim trailing double-quote
              (setq line (concat line extra "\""))                    ; append 'extra' material
              (push "program.r-processing"                 lines)
              (push ""                                     lines)
              (push policy-line                            lines)
              (push line                                   lines)
              (push ""                                     lines))))
    (message "  xem-rrunreg-opening: positions : %s %s" pos-3 pos-4)  ; "%s" is okay
    (message "  xem-rrunreg-opening: line: %s" line)
    (push "" lines)
    ;; housekeeping
    (message "  xem: rrunreg-opening: complete")
    lines ))                                 ; expose return value

(defun xem-rrunreg-closing ()
  "Create a necessary subset of the 'tail' records."
  (let* ((lines ()  ))                       ; list of lines to be returned
    (push "model-end"                                      lines)
    (push ""                                               lines)
    (message "  xem: rrunreg-closing: complete")
    lines ))                                 ; expose return value

(defun xem-rrunreg-opening-test ()
  "Wrapper function to test function `xem-rrunreg-opening'."
  (interactive)
  (let* ()
    (message "%s" (xem-stringify-list (xem-rrunreg-opening) "\n")) ))

;; ---------------------------------
;;  provide
;; ---------------------------------

;; put the mode symbol into the list "features", so that users can
;; invoke (require 'xrstat-mode) and load your code only when needed

(provide 'xrstat-mode)

;;; xrstat.el ends here

;  $Id: xrstat.el 7071 2011-09-06 12:46:21Z robbie $
;  end of file

