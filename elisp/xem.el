;;; xem.el --- editing support for xeona model files

;  file-purpose     : emacs editing support for xeona model files
;  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
;  file-create-date : Fri 16-Jul-2010 15:19 UTC
;  file-status      : ongoing
;  file-keywords    : emacs xeona

;  $Revision: 9269 $
;  $Date: 2012-12-10 14:45:38 +0100 (Mon, 10 Dec 2012) $
;  $Author: robbie $
;  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/elisp/xem.el $

;  XEONA CODEBASE COPY
;
;  * this code was originally imported from RCS 1.48 of
;    '/home/robbie/synk/xeona/xem-processing/xem.el'
;
;  * the GLPv2 license included here derives from Emacs Lisp
;    coding practice and from not the 'xeona' project --
;    users can optionally chose to adopt the 'xeona' norm
;    instead

;; Copyright (C) 2009-2011 Robbie Morrison

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
;;      * check current buffer logic (although no problems have
;;        been detected), while noting the warning in section 27.2
;;        of the elisp manual
;;
;;      * remove redundant `message' development calls
;;
;;      * add `abbreviate-file-name' to messages where appropriate
;;
;;      * run `checkdoc' periodically
;;
;;  Known issues
;;
;;      * IGNORE: xeona reorders its XEM file on first writing, which means
;;        that the 'vhold' and 'hhold' placeholders becomes scrambled -- any
;;        fix would require an semantic understanding of location and that is
;;        not a priority at this stage of development
;;
;;  Major mode programming resources
;;
;;      * Cameron et al (2004 p381+) for an introduction
;;      * "info elisp" and in particular "Major Mode Examples"
;;      * Xah's website: http://xahlee.org/emacs
;;      * Emacs wiki: www.emacswiki.org/emacs/MajorMode
;;
;;  Note on `message' calls
;;
;;      Any `message' call expected to be read (meaning, should
;;      remain in the mini-buffer long enough to be seen), should
;;      end with a '.' dot.
;;
;;  References
;;
;;     Cameron, Debra, James Elliott, Marc Loy, Eric Raymond, and Bill
;;         Rosenblatt.  2005.  Learning GNU Emacs -- Third edition.
;;         O'Reilly and Associates, Sebastopol, CA, USA.  ISBN:
;;         0-596-00648-9.  [For GNU Emacs 21.3.5]

;;; History:

;;      * see the subversion log messages

;;; Code:

;; ---------------------------------
;;  user-modifiable global constants
;; ---------------------------------

(defconst xem-website
  "http://www.iet.tu-berlin.de/deeco"        ; currently set to 'deeco'!
  "Xeona website URL.")

(defconst xem-dchar
  "#"
  "Xeona disable char (as set in 'common.cc').")

(defconst xem-model-extension
  ".xem"
  "Xeona model extension (as set in 'common.cc').")

(defconst xem-backup-tag
  "~"
  "Xeona backup tag (as set in 'common.cc').")

(defconst xem-model-guard-tag
  ".guard"
  "Xeona guard file tag (as set in 'common.cc').")

(defconst xem-binary-name
  "xeona.mach"
  "Xeona binary name (located semi-intelligently).")

(defconst xem-buffer-xeona-output
  "*xeona-output*"
  "Buffer for xeona output.")

(defconst xem-buffer-xeona-data-rules
  "*xeona-data-rules*"
  "Buffer for xeona data rules.")

(defconst xem-buffer-xeona-usage
  "*xeona-usage*"
  "Buffer for xeona usage.")

(defconst xem-buffer-xem-diff
  "*xem-diff*"
  "Buffer for xem diff results.")

(defconst xem-file-stem
  xeona-file-stem
  "Current directory.")

(defconst xem-xumber-file
  (concat xem-file-stem "xumber.el")
  "Xumber mode file to drag in.")

;; ---------------------------------
;;  requirements
;; ---------------------------------

(require 'ediff)                             ; "a comprehensive visual interface to diff & patch"
(require 'hi-lock)                           ; "minor mode for interactive automatic highlighting"
(require 'thingatpt)                         ; "get the `thing' at point"
(require 'highlight-current-line)            ; "highlight line where the cursor is"

;; ---------------------------------
;;  mode function
;; ---------------------------------

(defun xem-mode ()
  "Major mode for editing xeona XEM model files.

All interactive functions start \"xem-\" for easy identification.

If the menu bar is hidden, a background Xem menu can be brought
up with: \"Ctrl-Shift-leftmouse\".

Dedicated key binding are indicated in the Xem menu.  For a
complete listing, use \\[describe-bindings].

The cursor changes from black to turquoise under the selective
display.  The cursor changes from a box to a line under narrowing.

Much of the functionality of this mode depends on access to a
suitable 'xeona' binary.  This access is tested on loading and
any failure in noted.

The `xem-toggle-narrow' function requires `narrow-to-region' to
be enabled.  In which case you may need to the following in your
emacs configuration file:

  (put 'narrow-to-region 'disabled nil)"
  (interactive)

  (message "  xem: xem-mode setup commencing")

  ;; recommended
  (kill-all-local-variables)

  ;; information
  (message "  xem: default-face-height %d" xem-default-face-height)

  ;; emacs stuff
  (setq major-mode 'xem-mode)                ; used by C-h m aka `describe-mode'
  (setq mode-name  "Xem")
  (use-local-map    xem-mode-map)
  (run-hooks       'xem-mode-hook)           ; optionally defined in the user's configuration file

  ;; use our custom menu, "Xem", located to the right of "Tools"
  (menu-bar-mode 1)

  ;; visual bell (particularly as the Ubuntu system bell is broken as of mid-2010)
  (setq visible-bell t)

  ;; enable word-based wrapping under `toggle-truncate-lines', this is part of `visual-line-mode'
  (setq word-wrap t)                         ; non-nil means word wrap continuation lines

  ;; background (can be redefined within 'xem-mode-hook')
  (setq fill-column 63)                      ; auto-fill wrap (should strictly speaking be 65 - 4)
  (setq selective-display-ellipses nil)      ; omit "..."

  ;; global variables
  (setq xem-binary   (xem-locate-binary xem-binary-name))
  (setq xem-svn      (xem-binary-svn xem-binary)) ; CAUTION: integer not string
  (setq xem-have-run (xem-have-i-run))

  ;; make your comment command use the same shortcut for 'comment-dwim' (dwim = do what I mean),
  ;; the user may have changed their default
  ;; (define-key xem-mode-map [remap comment-dwim] 'xem-comment-dwim)

  ;; miscellaneous
  (setq message-log-max t)                   ; 'true' means do not truncate "*Messages*"
  (global-auto-revert-mode 1)                ; experimental, note also `auto-revert-mode'
  (message "  xem: xem-mode: Global-Auto-Revert mode activated")
  (xem-toggle-SITFOR +1)

  ;; line wrapping
  (toggle-truncate-lines 1)                  ; "1" is never fold (wrap) long lines

  ;; general actions
  (xem-colorize)                             ; automatically colorize buffer on opening
; (xem-maxscreen-me)
; (xem-fullscreen-me)

  ;; if-run actions
  (if (xem-have-i-run)                       ; 'true' if XEM file has run
      (message "  xem: xem-mode: XEM file deemed to have run")
    (message "  xem: xem-mode: XEM file deemed NOT to have run"))

  ;; Xumber mode
  ; both work the same
  (autoload 'xumber-mode
    xem-xumber-file
    "Toggle xeona number mode."
    t)                                       ; 't' for interactive
  (xumber-mode t)
  (if xumber-mode
      (message "  xem: xem-mode: Xumber mode now active")
    (message "  xem: xem-mode: Xumber mode INACTIVE"))

  ;; completion reporting
  (if (not xem-binary) (ding))               ; acknowledge visual bell setting
  (message "  xem: xem-mode setup complete (binary %s revision %s)." xem-binary xem-svn))

;; ---------------------------------
;;  custom key maps
;; ---------------------------------

(defvar xem-mode-map nil "Local keymap for Xem mode buffers.")
;; CAUTION: must NOT be (make-local-variable 'xem-mode-map)

;; see additional file 'keys-and-keymaps.txt' for more
;; information on this topic

(if xem-mode-map
    ()                                       ; protection against double loading

  ;; keyboard shortcuts
  (setq xem-mode-map (make-sparse-keymap))   ; CAUTION: resets the above
  (define-key xem-mode-map [(shift         right)] 'xem-cycle-squish)
  (define-key xem-mode-map [(shift          left)] 'xem-toggle-rules)
  (define-key xem-mode-map [(            backtab)] 'xem-toggle-data)            ; type (shift tab)
  (define-key xem-mode-map [(control shift iso-lefttab)] 'xem-double-dot-one)   ; experimental
  (define-key xem-mode-map [(shift            up)] 'xem-move-entity-prior)
  (define-key xem-mode-map [(shift          down)] 'xem-move-entity-next)
  (define-key xem-mode-map [(shift          home)] 'xem-move-hash-prior)
  (define-key xem-mode-map [(shift           end)] 'xem-move-hash-next)
  (define-key xem-mode-map [(shift         prior)] 'xem-move-field-in-prior)
  (define-key xem-mode-map [(shift          next)] 'xem-move-field-in-next)
  (define-key xem-mode-map [(shift         pause)] 'xem-toggle-highlight-output)
  (define-key xem-mode-map [(control shift   end)] 'xem-move-hash-ring)
  (define-key xem-mode-map [(shift         print)] 'xem-toggle-narrow)

  (define-key xem-mode-map [(               menu)] 'xem-run-me)
  (define-key xem-mode-map [(shift          menu)] 'xem-r-run-switch)
  (define-key xem-mode-map [(control        menu)] 'xem-bidsets-display-line)
  (define-key xem-mode-map [(control shift  menu)] 'xem-toggle-xeona-binary)
  (define-key xem-mode-map [(meta           menu)] 'xem-oneplot-line)
  (define-key xem-mode-map [(control meta   menu)] 'xem-toggle-steps)
  (define-key xem-mode-map [(super          menu)] 'xem-summarize-wrapper)
  (define-key xem-mode-map [(super           end)] 'xem-get-full-field-identifier)
  (define-key xem-mode-map [(meta shift     menu)] 'xem-oneplot-window-toggle)
  (define-key xem-mode-map [(meta super     menu)] 'xem-summarize-line)
; (define-key xem-mode-map [(control super  menu)] ')

  (define-key xem-mode-map [(                 f6)] 'xem-occur-next)
  (define-key xem-mode-map [(shift            f6)] 'xem-occur-prev)
  (define-key xem-mode-map [(control x)        ?p] 'xem-print-nocom-region)

  ;; temporary key maps for convenience during development

  (define-key xem-mode-map [(                 f2)] 'xem-reset-me)

  ;; custom menu (does not make use of the 'easymenu' elisp package)
  (defvar menuXem00 (make-sparse-keymap "Xem Menu"))    ; mouse label
  (defvar menuXem01 (make-sparse-keymap "Xem 01"))
  (defvar menuXem02 (make-sparse-keymap "Xem 02"))
  (defvar menuXem03 (make-sparse-keymap "Xem 03"))
  (defvar menuXem04 (make-sparse-keymap "Xem 04"))
  (defvar menuXem05 (make-sparse-keymap "Xem 05"))
  (defvar menuXem06 (make-sparse-keymap "Xem 06"))
  (defvar menuXem07 (make-sparse-keymap "Xem 07"))
  (defvar menuXem08 (make-sparse-keymap "Xem 08"))
  (defvar menuXem09 (make-sparse-keymap "Xem 09"))
  (defvar menuXem10 (make-sparse-keymap "Xem 10"))
  (defvar menuXem11 (make-sparse-keymap "Xem 11"))
  (defvar menuXem12 (make-sparse-keymap "Xem 12"))
  (defvar menuXem13 (make-sparse-keymap "Xem 13"))

  ;; mouse usage
  (define-key xem-mode-map [C-S-down-mouse-1] menuXem00)

  ;; top-level entry (visible in menu bar, right of "Tools")
  (define-key xem-mode-map [menu-bar xem]          (cons "Xem"                 menuXem00)) ; menu-bar label

  ;; main menu bottom (reverse order)
  (define-key menuXem00 [about]       '(menu-item "About"                   xem-menu-about           :help "One-line report on Xem mode"))
  (define-key menuXem00 [help]        '(menu-item "Describe mode"           xem-menu-describe        :help "Standard emacs 'describe-mode' call"))
  (define-key menuXem00 [bindings]    '(menu-item "Describe key bindings *" describe-bindings        :help "Standard emacs 'describe-bindings' call"))
  (define-key menuXem00 [usage-filt]  '(menu-item "Show xeona usage"        xem-usage-filtered       :help "Show regex filtered xeona usage using a call to \"xeona --usage\""))
  (define-key menuXem00 [help-data]   '(menu-item "Show xeona data rules"   xem-menu-show-data-rules :help "Show xeona data rules using a call to \"xeona --data\""))
  (define-key menuXem00 [separator-1] '("--"))

  ;; sub-menu entries (reverse order)
  (define-key xem-mode-map [menu-bar xem submenu13] (cons "Miscellaneous"                         menuXem13))
  (define-key xem-mode-map [menu-bar xem submenu12] (cons "File"                                  menuXem12))
  (define-key xem-mode-map [menu-bar xem submenu11] (cons "Print"                                 menuXem11))
  (define-key xem-mode-map [menu-bar xem submenu10] (cons "Model dumps"                           menuXem10))
  (define-key xem-mode-map [menu-bar xem submenu09] (cons "Visual post-processing using R"        menuXem09))
  (define-key xem-mode-map [menu-bar xem submenu08] (cons "Numerical post-processing using elisp" menuXem08))
  (define-key xem-mode-map [menu-bar xem submenu07] (cons "Run / diff / reset model"              menuXem07))
  (define-key xem-mode-map [menu-bar xem submenu06] (cons "Hi-light"                              menuXem06))
  (define-key xem-mode-map [menu-bar xem submenu05] (cons "Tidy code"                             menuXem05))
  (define-key xem-mode-map [menu-bar xem submenu04] (cons "Report environment"                    menuXem04))
  (define-key xem-mode-map [menu-bar xem submenu03] (cons "Summarize model"                       menuXem03))
  (define-key xem-mode-map [menu-bar xem submenu02] (cons "Insert"                                menuXem02))
  (define-key xem-mode-map [menu-bar xem submenu01] (cons "Navigate"                              menuXem01))

  ;; main menu top (reverse order)
  (define-key menuXem00 [separator-2] '("--"))
  (define-key menuXem00 [tog-screen]  '(menu-item "Full-screen (toggle)"        xem-fullscreen-me-toggle    :help "Toggle full screen (Linux only)"))
  (define-key menuXem00 [tog-max]     '(menu-item "Maximize screen (toggle)"    xem-maxscreen-me-toggle     :help "Toggle maximize screen (Linux only)"))
; (define-key menuXem00 [switch]      '(menu-item "Switch to useful buffer"     xem-switch-to-useful-buffer :help "Switch to useful buffer OR show buffer list"))
  (define-key menuXem00 [cycle-bufs]  '(menu-item "Cycle to next useful buffer" xeona-cycle-buffers         :help "Cycle to next useful buffer"))
  (define-key menuXem00 [narrow]      '(menu-item "Narrow to entity (toggle)"   xem-toggle-narrow           :help "Toggle narrow to entity function (a line cursor indicates this action) (requires 'narrow-to-region' be enabled)"))
  (define-key menuXem00 [enable]      '(menu-item "Enabled status (toggle)"     xem-toggle-data             :help "Toggle entity enabled status OR create duplicate disabled field or reinstate current disabled field (access via <S-tab>)"))
  (define-key menuXem00 [toggle-rule] '(menu-item "Rule indent (toggle)"        xem-toggle-rules            :help "Toggle rule indent between 1 and 2 cols (in order to change visibility under full collapse)"))
  (define-key menuXem00 [cycl-squish] '(menu-item "Cycle display collapse"      xem-cycle-squish            :help "Cycle thru 3-way display collapse (a turquoise cursor indicates an incomplete display)"))

  ;; ---

  ;; submenu 1 (reverse order)
  (define-key menuXem01 [hash-cycle]  '(menu-item "Cycle thru disabled entities and fields" xem-move-hash-ring       :help "Cycle to next disabled entity or field"))
  (define-key menuXem01 [hash-next]   '(menu-item "Next disabled entity or field"           xem-move-hash-next       :help "Move down to next disabled entity or field"))
  (define-key menuXem01 [hash-prior]  '(menu-item "Previous disabled entity or field"       xem-move-hash-prior      :help "Move up to previous disabled entity or field"))
  (define-key menuXem01 [outf-next]   '(menu-item "Next out-data field"                     xem-move-field-out-next  :help "Move down to next output data field"))
  (define-key menuXem01 [outf-prior]  '(menu-item "Previous out-data field"                 xem-move-field-out-prior :help "Move up to previous output data field"))
  (define-key menuXem01 [inf-next]    '(menu-item "Next in-data field"                      xem-move-field-in-next   :help "Move down to next input data field"))
  (define-key menuXem01 [inf-prior]   '(menu-item "Previous in-data field"                  xem-move-field-in-prior  :help "Move up to previous input data field"))
  (define-key menuXem01 [ent-next]    '(menu-item "Next entity"                             xem-move-entity-next     :help "Move down to next entity"))
  (define-key menuXem01 [ent-prior]   '(menu-item "Previous entity"                         xem-move-entity-prior    :help "Move up to previous entity"))

  ;; submenu 2 (reverse order)
  (define-key menuXem02 [sel-list]    '(menu-item "Generate a selections list"    xem-make-selections-list-wrapper  :help "Given 12 steps, turn 'selections-list : 1 : 0 = 3 4 : 2 = 9 10' into '1 1 1 0 0 1 1 1 1 2 2 1'"))
  (define-key menuXem02 [uncamel]     '(menu-item "Uncamel current symbol"        xem-uncamel                       :help "Uncamel current symbol (meaning convert \"myId\" to \"my-id\")"))
  (define-key menuXem02 [pink]        '(menu-item "Colorized comment"             xem-insert-strong-comment         :help "Insert pink colorized comment"))
  (define-key menuXem02 [rule-scroll] '(menu-item "Rule via scroll or entry"      xem-insert-rule-scroll            :help "Insert rule by scrolling or by direct entry"))
;;(define-key menuXem02 [rule-prompt] '(menu-item "Rule via prompt"               xem-insert-rule-prompt            :help "Insert rule after prompting"))
  (define-key menuXem02 [call-scroll] '(menu-item "Entity via scroll"             xem-insert-entity-scroll          :help "Insert new entity by scrolling an alphabetical list"))
  (define-key menuXem02 [call-regex]  '(menu-item "Entity via regex"              xem-insert-entity-regex           :help "Insert new entity based on prompted case-sensitive regex"))
  (define-key menuXem02 [new-model]   '(menu-item "New model"                     xem-insert-new-model              :help "Insert new model (an empty buffer is recommended)"))

  ;; submenu 3 (reverse order)
  (define-key menuXem03 [bidset-line] '(menu-item "Show bidsets on current line"       xem-bidsets-display-line    :help "Display bidsets timeseries on current line in new buffer"))
  (define-key menuXem03 [separator-4] '("--"))
  (define-key menuXem03 [scan-region] '(menu-item "List classes and remarks in region" xem-scan-class-region       :help "Collate and list selected classes and builtin remarks"))
  (define-key menuXem03 [scan-buffer] '(menu-item "List classes and remarks in buffer" xem-scan-class-buffer       :help "Collate and list all classes and builtin remarks"))
  (define-key menuXem03 [separator-3] '("--"))
  (define-key menuXem03 [occur-next]  '(menu-item "Navigate down thru summaries"       xem-occur-next              :help "Navigate down thru the summaries"))
  (define-key menuXem03 [asterisk]    '(menu-item "List \"**\" comments"               xem-occur-asterisks         :help "List \"**\" comments (usually pink)"))
  (define-key menuXem03 [identities]  '(menu-item "List field value identities"        xem-occur-identities        :help "List identities used as field values (the \"entity.\" is absent and any sub-entity identifiers may follow)"))
  (define-key menuXem03 [hashes]      '(menu-item "List hashes"                        xem-occur-hashes            :help "List hashes"))
  (define-key menuXem03 [rules]       '(menu-item "List rules"                         xem-occur-rules             :help "List rules"))
  (define-key menuXem03 [classes]     '(menu-item "List classes"                       xem-occur-classes           :help "List classes"))
  (define-key menuXem03 [domcons]     '(menu-item "List domain controllers"            xem-occur-domcons           :help "List domain controller entities"))
  (define-key menuXem03 [disabled]    '(menu-item "List disabled entities"             xem-occur-entities-disabled :help "List only disabled entities"))
  (define-key menuXem03 [entities]    '(menu-item "List entities"                      xem-occur-entities          :help "List all entities"))
  (define-key menuXem03 [remarks]     '(menu-item "List builtin remarks"               xem-occur-builtin-remarks   :help "List builtin remarks"))
  (define-key menuXem03 [separator-5] '("--"))
  (define-key menuXem03 [toggle-out]  '(menu-item "Show output highlights"             xem-toggle-highlight-output :help "Toggle show output highlights as defined under the in-data field 'program.r-processing.r-highlight-output'"))
  (define-key menuXem03 [status]      '(menu-item "Report on run status"               xem-menu-model-status       :help "Report on full file name and model run status"))

  ;; submenu 4 (reverse order)
  (define-key menuXem04 [about]       '(menu-item "Report on Xem mode"                         xem-menu-about         :help "Report on current Xem mode version number"))
  (define-key menuXem04 [svn-version] '(menu-item "Give xeona codebase"                        xem-menu-svnversion    :help "Display current xeona codebase svn version string"))
  (define-key menuXem04 [locate]      '(menu-item "Give current xeona binary name and version" xem-menu-report-binary :help "Give name of xeona binary in use by Xem mode and show version number"))

  ;; submenu 5 (reverse order)
  (define-key menuXem05 [clean]       '(menu-item "Clean \"some problem\" (obsolete)"                        xem-clean-model     :help "Clean any \"(some problem)\" in the current model"))
  (define-key menuXem05 [rerule]      '(menu-item "Realign current rule"                                     xem-rule            :help "Realign rule (will also prompt for text if no rule current)"))
  (define-key menuXem05 [revisit]     '(menu-item "Save and revisit buffer (to reset emacs local variables)" xem-revisit         :help "Save and revisit the current buffer to reset the emacs local variables"))
  (define-key menuXem05 [uncolorize]  '(menu-item "Uncolorize buffer *"                                      hi-lock-mode        :help "Remove color emphasis from buffer (by running 'hi-lock-mode')"))
  (define-key menuXem05 [colorize]    '(menu-item "Colorize buffer"                                          xem-colorize        :help "Add color emphasis to buffer"))
  (define-key menuXem05 [reset-tabs]  '(menu-item "Reset tab-stop-list, then save and revisit"               xem-reset-tabstops  :help "Reset emacs local variables tabstop list using the prevailing format"))
  (define-key menuXem05 [reindent]    '(menu-item "Reindent buffer using tab-stops-list"                     xem-reindent        :help "Reindent entire buffer using emacs local variables tabstop list (save and revisit file first if the tabstops have changed)"))
  (define-key menuXem05 [trim]        '(menu-item "Delete trailing whitespace"                               xem-trim-trailing   :help "Delete trailing white space"))
  (define-key menuXem05 [squeeze]     '(menu-item "Squeeze blank lines"                                      xem-squeeze-lines   :help "Remove multiple blank lines without prompting"))

  ;; submenu 6 (reverse order)
  (define-key menuXem06 [kil-buffer]  '(menu-item "Clear buffer"    xem-hilighter-clear-buffer :help "Clear all highlights"))
  (define-key menuXem06 [kil-region]  '(menu-item "Clear region"    xem-hilighter-clear-region :help "Clear intersecting highlights from current region"))
  (define-key menuXem06 [hil-region]  '(menu-item "Hi-light region" xem-hilighter-region       :help "Hilight current region"))
  (define-key menuXem06 [hil-entity]  '(menu-item "Hi-light entity" xem-hilighter-entity       :help "Hilight current entity (except last) (becomes temporarily scrambled under display collapse)"))

  ;; submenu 7 (reverse order)
  (define-key menuXem07 [rebin]       '(menu-item "Change xeona binary"                         xem-rebinary            :help "Change name of called binary"))
  (define-key menuXem07 [tidy]        '(menu-item "Tidy model using xeona"                      xem-run-tidy            :help "Run current model using \"xeona --tidy\""))
  (define-key menuXem07 [reset-me]    '(menu-item "Reset model (do both the above)"             xem-reset-me            :help "Intelligently zero and double-dot over entire buffer -- indicated whenever 'time-horizon.steps' is altered"))
  (define-key menuXem07 [zero-me]     '(menu-item "Zero the out-data"                           xem-zero-me             :help "Intelligently zero out-data over entire buffer"))
  (define-key menuXem07 [double-dot]  '(menu-item "Double dot all timeseries in-data"           xem-double-dot-me       :help "Intelligently add trailing double-dots to all in-data timeseries over entire buffer"))
  (define-key menuXem07 [double-one]  '(menu-item "Truncate timeseries in-data on current line" xem-double-dot-one      :help "Blindly truncate to first element and add trailing double-dots to in-data on current line"))
  (define-key menuXem07 [diff-me]     '(menu-item "Diff model (choose)"                         xem-diff-me             :help "Prompt for diff method and then diff current model against its ~ file (user is responsible for ensuring currency)"))
  (define-key menuXem07 [ediff-me]    '(menu-item "Ediff model"                                 xem-diff-me-ediff       :help "Ediff current model against its ~ file (user is responsible for ensuring currency)"))
  (define-key menuXem07 [run-1]       '(menu-item "Run model"                                   xem-run-me              :help "Run current model after prompted for values for report|mode|yeek"))
  (define-key menuXem07 [toggle-bin]  '(menu-item "Toggle between default and release binary"   xem-toggle-xeona-binary :help "Toggle between default binary and latest release build binary"))
  (define-key menuXem07 [hvtr-params] '(menu-item "Change HV transmission params en-masse"      xem-hv-params           :help "Change the discretization-steps param for the HV transmission entities using 0-style input."))
  (define-key menuXem07 [glpk-params] '(menu-item "Change GLPK params en-masse"                 xem-glpk-params         :help "Change GLPK params in domain controllers using 000000-style input."))
  (define-key menuXem07 [toggle-step] '(menu-item (format "Change horizon steps (perhaps toggle %d and %d)" xem-toggle-steps-lo xem-toggle-steps-hi) xem-toggle-steps :help "Semi-intelligent toggle for 'entity.time-horizon.steps' between either the default or, if present, the disabled values."))

  ;; submenu 8 (reverse order)
  (define-key menuXem08 [sumize-line] '(menu-item "Statistical summary for timeseries on current line" xem-summarize-line    :help "Display statistical summary for timeseries on current line."))
  (define-key menuXem08 [pp-sumize]   '(menu-item "Statistical summaries for nominated timeseries"     xem-summarize-wrapper :help "Provide statistical summaries of timeseries and scalars, normally post run (but not mandatory)."))

  ;; submenu 9 (reverse order)
  (define-key menuXem09 [r-svgs]      '(menu-item "Open associated SVGs"                      xem-r-svgs                :help "Open any associated SVG files"))
  (define-key menuXem09 [r-pdfs]      '(menu-item "Open associated PDFs"                      xem-r-pdfs                :help "Open any associated PDF files"))
  (define-key menuXem09 [r-pngs]      '(menu-item "Open associated PNGs"                      xem-r-pngs                :help "Open any associated PNG files"))
  (define-key menuXem09 [r-run-reg]   '(menu-item "Invoke 'xem.job' on current region"        xem-r-run-reg             :help "Invoke 'xem.job' call on current region using r-policy = 24, the model need not have run"))
  (define-key menuXem09 [r-run]       '(menu-item "Invoke 'xem.job' on current model"         xem-r-run                 :help "Invoke 'xem.job' call on current model without invoking 'xeona' (\"xeona = NULL\" is passed), the model must have run"))
  (define-key menuXem09 [r-run-swit]  '(menu-item "Invoke 'xem.job' using a switcher"         xem-r-run-switch          :help "Invoke 'xem.job' call on current model or region (either of the two calls below)"))
  (define-key menuXem09 [r-oneplot]   '(menu-item "Plot current data line"                    xem-oneplot-line          :help "Plot the current in-data or out-data line, produces a temporary PDF which can be renamed"))
  (define-key menuXem09 [one-toggle]  '(menu-item "Toggle between PDF export and window plot" xem-oneplot-window-toggle :help "Toggle between PDF export and view and plot in window"))

  ;; submenu 10 (reverse order)
  (define-key menuXem10 [dump-clean]  '(menu-item "Prompted en-masse clean up of current buffer" xem-dump-clean-buffer :help "Prompted en-masse clean based on current buffer, files containing \".xem\" remain"))
  (define-key menuXem10 [dump-unpack] '(menu-item "Select and unpack an existing tarball"        xem-dump-unpack       :help "Select and unpack tarball into current directory, prompted for high or low extraction (may also revert open files)"))
  (define-key menuXem10 [dump-sum]    '(menu-item "Summarize dumped models in current directory" xem-dump-summarize    :help "Summarized recognized dumps in current directory"))
  (define-key menuXem10 [dump-xem]    '(menu-item "Dump current model to tarball"                xem-dump              :help "Dump current model to tarball -- best with relevant R plots and HTML visualizations"))

  ;; submenu 11 (reverse order)
  (define-key menuXem11 [print-reg]   '(menu-item "Print buffer with screening" xem-print-nocom-buffer :help "Print buffer while omitting 6 indented comments, 2 indented rules, and out-data"))
  (define-key menuXem11 [print-buf]   '(menu-item "Print region with screening" xem-print-nocom-region :help "Print region while omitting 6 indented comments, 2 indented rules, and out-data"))

  ;; submenu 12 (reverse order)
  (define-key menuXem12 [ung-current] '(menu-item "Create model from current guard file"          xem-unguard-current   :help "Create a standard model file from the current guard file"))
  (define-key menuXem12 [ung-dialog]  '(menu-item "Create model from selected guard file"         xem-unguard-prompted  :help "Create a standard model file from a dialoged list of guard files"))
  (define-key menuXem12 [guard]       '(menu-item "Create guard file from current model"          xem-file-guardize     :help "Create a guard file from the current model"))
  (define-key menuXem12 [bf-visits]   '(menu-item "Save various log buffers"                      xem-buffers-visit-all :help "Save selected unvisited log buffers,the original buffer names are retained"))
  (define-key menuXem12 [peel]        '(menu-item "Peel off a backup file"                        xem-file-peel         :help "Peel off an incremental backup file from the current model (but consider using RCS version control instead)"))

  ;; submenu 13 (reverse order)
  (define-key menuXem13 [sitfor]      '(menu-item "Toggle SITFOR value"                   xem-toggle-SITFOR                 :help "Toggle and then report the new value of SITFOR used in emacs 'sit-for' calls"))
  (define-key menuXem13 [repeat]      '(menu-item "Repeat last command *"                 repeat                            :help "Standard emacs 'repeat' command"))
  (define-key menuXem13 [save-some]   '(menu-item "Save some buffers *"                   save-some-buffers                 :help "Standard emacs 'save-some-buffers' command"))
  (define-key menuXem13 [kbd-quit]    '(menu-item "Quit the current emacs code (safe) *"  keyboard-quit                     :help "Standard emacs 'keyboard-quit' command"))
  (define-key menuXem13 [line-hi]     '(menu-item "Toggle current line highlight (hcl) *" highlight-current-line-minor-mode :help "Toggle current line highlight (by running 'highlight-current-line-minor-mode')"))
  (define-key menuXem13 [r-stub]      '(menu-item "Display world time *"                  display-time-world                :help "Display world time"))
  (define-key menuXem13 [hi-phrase]   '(menu-item "Highlight a regex *"                   highlight-phrase                  :help "Highlight regex phrase (by running 'highlight-phrase')"))
  (define-key menuXem13 [open-xref]   '(menu-item "Open external link on current line"    xem-open-xref                     :help "Open an org-mode style external link on the current line"))
  (define-key menuXem13 [website]     '(menu-item "Visit xeona (deeco) website"           xem-menu-goto-website             :help "Visit the xeona website (currently the deeco site)"))
  (define-key menuXem13 [all-dec]     '(menu-item "Decrease text size everywhere"         xem-scale-decrease-everywhere     :help "Decrease text size everwhere"))
  (define-key menuXem13 [font-dec]    '(menu-item "Decrease text size here"               text-scale-decrease               :help "Decrease text size using a 'text-scale' function (also <S-down-mouse-1>)"))
  (define-key menuXem13 [fullqual]    '(menu-item "Grab fully-qualified field name"       xem-get-full-field-identifier     :help "Grab field name on current line, prepend record name, and add to kill ring"))
  (define-key menuXem13 [reapply]     '(menu-item "Reapply xem-mode"                      xem-mode                          :help "Reapply 'xem-mode', perhaps to correct a coloration problem"))

 ) ; final parenthesis

;; ---------------------------------
;;  aliases
;; ---------------------------------

;; CAUTION: `defalias' on 2007 GNU Emacs 21.4 does not support an optional DOCSTRING

;;---------------------------------
;; dot function
;;---------------------------------

(defun xem-double-dot-one ()
  "Truncate and double dot current line."
  (interactive)
  (let* ((angle  ">" )
         (line1  nil )
         (line2  nil )
         (parts  nil )
         (left   nil )
         (right  nil )
         (orig   nil )
         (vals   nil )
         (val    ""  ))
    ;; capture and delete
    (beginning-of-line)
    (setq line1 (thing-at-point 'line))
    (delete-region (line-beginning-position) (line-end-position))  ; the newline char remains tho
    ;; split field
    (setq parts (split-string line1 angle nil))
    (setq left  (nth 0 parts))
    (setq right (nth 1 parts))                                   ; could be nil
    (setq right (xem-string-trim right))                         ; could be empty string
    (setq org right)                                             ; for completion reporting
    (setq right (replace-regexp-in-string "\\.\\.$" "" right))   ; trim any trailing double dots
    (setq right (xem-string-trim right))
    ;; split and value
    (cond
     ;; { null }
     ((= (length right) 0) )                      ; do nothing
     ;; already dotted
     ((string-match "\\.\\.$" right) )            ; do nothing
     ;; { s S x X }
     ((string-equal (substring right 0 1) "\"")   ; leading double-quote
      (setq vals (split-string (substring right 1 -1) "\" +\"" nil))
      (setq val (nth 0 vals))
      (setq val (concat " " "\"" val "\"" " ..")))
      ;; { i I f F bB } noting that boolean is handled as integer
      (t                                          ; by elimination
       (setq vals (split-string right " " nil))
       (setq val (nth 0 vals))
       (setq val (concat " " val " .."))) )
    ;; reassemble
    (setq line2 (concat left angle val))
    ;; insert
    (insert line2)
    ;; completion reporting
    (message "  xem: double-dot-one complete using:\n  %s\n %s" org val) ))

;; ---------------------------------
;;  decrease text size everywhere
;; ---------------------------------

(defun xem-scale-decrease-everywhere ()
  "Decrease text size everywhere."
  (interactive)
  (let* ((xfont "-misc-fixed-medium-r-normal--13-*-*-*-*-80-iso8859-15"))
    (set-face-attribute                      ; loaded changes propagate to new frames
     'default                                ; so-called default face
     nil                                     ; CAUTION: 'nil' required and will propagate
     :font xfont)                            ; takes any supported "XLFD" (X logical font description)
    (message "  xem: scale-decrease-everywhere complete using '%s'." xfont) ))

;; ---------------------------------
;;  menu support files
;; ---------------------------------

(defun xem-menu-help ()
  "Wrapper to `describe-function' `xem-mode'."
  (interactive)
  (describe-function 'xem-mode))

(defun xem-menu-describe ()
  "Wrapper to `describe-mode'."
  (interactive)
  (describe-mode))

(defun xem-menu-goto-website ()
  "Goto `xem-website' value."
  (interactive)
  (message "  xem: 'xeona' website not yet written, providing 'deeco' website instead.")
  (browse-url xem-website))

(defun xem-menu-svnversion ()
  "States current svn version string."
  (interactive)
  (let* ((workingcopy  "/home/robbie/synk/xeona/svn2/futz"                          )
         (svnver       (shell-command-to-string (concat "svnversion " workingcopy)) ))
    (if (file-accessible-directory-p workingcopy)
        (progn
          (if (string-match "\n$" svnver)             ; trim trailing newline
              (setq svnver (replace-match "" t t svnver)))  ; fixed case and literal
          (message "Current svn version for '%s' is %s." workingcopy svnver))
      (message "Cannot read given working copy directory '%s'." workingcopy))))

(defun xem-menu-about ()
  "About message for `xem-mode'."
  (interactive)
  (let* ((file    (car (load-history-filename-element "xem")) )  ; name of this elisp file
         (sedstr  "s/^[[:space:]]*;\\+[[:space:]]*\\$Revision: \\([[:digit:]]*\\) \\$/\\1/p")
         (call    (concat "sed --quiet '" sedstr "' " file)   )
         (svnver  "(not set)"                                 ))
    (message "  xem: about: file: '%s'" file)
    (message "  xem: about: sed: '%s'" sedstr)
    (setq svnver (shell-command-to-string call))
    (if (string-match "\n$" svnver)                    ; trim trailing newline
        (setq svnver (replace-match "" t t svnver)))   ; fixed case and literal
    (message "  xem: about: svnver: '%s'" svnver)
    (message "%s major mode %s for editing xeona XEM model files." mode-name svnver)))

(defun xem-menu-model-status ()
  "Provide information about the current XEM file."
  (interactive)
  (let ((xem-file (buffer-file-name nil)))
    (setq xem-have-run (xem-have-i-run))     ; update status
    (if xem-have-run
        (message "Xem file '%s' has been run (but not necessarily in this session)." xem-file)
      (message "Xem file '%s' has not been run." xem-file))))

(defun xem-menu-report-binary ()
  "Provide information about the xeona binary in use."
  (interactive)
  (message "Using xeona binary '%s' with svn '%s'." xem-binary xem-svn))

(defun xem-menu-show-data-rules ()
  "Show data file rules."
  (interactive)
  (let ((call    (concat xem-binary " --data" " 2>/dev/null"))
        (capture ""))
    (switch-to-buffer xem-buffer-xeona-data-rules)
    (erase-buffer)
    (setq capture (shell-command-to-string call))
    (insert capture)
    (beginning-of-buffer)
    (message "  xem: show data rules using call '%s' complete." call)))

;; ---------------------------------
;;  SITFOR toggle
;; ---------------------------------

(defconst SITFOR 0.0)

(defun xem-toggle-SITFOR (&optional set)
  "Toggle and display `SITFOR' value, otherwise reset depending on SET.

Activate if SET is positive, deactivate if negative, and do nothing if zero"
  (interactive)
  (let* ((default  0.5    )
         (zero     0.0    )
         (prior    SITFOR ))
    (if set
        (cond
         ((< set 0)         (setq SITFOR zero   ))     ; deactivate
         ((= set 0)                              )     ; do nothing except return value
         ((> set 0)         (setq SITFOR default)))    ; activate
      (cond
       ((= prior default) (setq SITFOR zero   ))       ; toggle off
       (t                 (setq SITFOR default))))     ; toggle on
    (message "  xem: SITFOR = %f" SITFOR)
    SITFOR ))                                          ; expose return value

;; ---------------------------------
;;  file peel
;; ---------------------------------

(defun xem-file-peel ()
  "Peel off a backup file from the current model (but consider using RCS version control instead)."
  (interactive)
  (let* ((limit     98                    )  ; cycle from one to 'limit' plus one
         (peelname  (xem-file-peelname 1) ))
    (dotimes (v limit)                       ; the 'v' starts zero inclusive and ends 'limit' exclusive
      (if (file-exists-p (xem-file-peelname (+ v 1)))
          (setq peelname (xem-file-peelname (+ v 2)))))
    (if (file-exists-p peelname)
        (error "  xem: file-peel: cannot peel this file, version ceiling %d exceeded" (1+ limit)))
    (write-region nil nil peelname)          ; could also add "nil nil nil t"
    (message "  xem: file-peel: peel complete using '%s'." peelname) ))

(defun xem-file-peelname (version)
  "Create a peel filename using VERSION integer."
  (let* ((xem-file  (buffer-file-name nil)                         )  ; file with current buffer
         (xem-stub  (file-name-sans-extension xem-file)            )
         (xem-ext   (file-name-extension xem-file)                 )  ; without leading dot
         (peelname  (format "%s_%02d.%s" xem-stub version xem-ext) ))
    peelname ))                              ; expose the return value

;; ---------------------------------
;;  guard file functions
;; ---------------------------------

(defun xem-file-guardize ()
  "Create and save a guard file based on the current model.

This function only processes the out-data -- the in-data is left intact.

This function does NOT rely on the various field value data-type
prompts being present and correct."
  (interactive)
  (let* ((ext        xem-model-extension                  )      ; with leading dot
         (tag        xem-model-guard-tag                  )
         (modelname  (buffer-file-name nil)               )      ; this file
         (stub       (file-name-sans-extension modelname) )
         (guardname  (concat stub tag ext)                )
         (bg         (get-buffer-create guardname)        )      ; guardfile buffer
         (count      0                                    ))
    ;; initial reporting
    (message "  xem: file-guardize: modelname '%s'" modelname)
    (message "  xem: file-guardize: guardname '%s'" guardname)
    ;; delete current guard file on request or abandon
    (if (file-exists-p guardname)
        (if (y-or-n-p "Overwrite existing guard file? ")
            (delete-file guardname)    ; TOFIX: 05-Aug-2010: should rename and delete on completion
          (kill-buffer bg)
          (error "  xem: file-guardize: guardfile '%s' exists and cannot \
be overwritten, abandoning task without action" guardname)))
    ;; continue
    (set-buffer bg)
    (set-visited-file-name guardname)
    (xem-mode)
    (insert-file modelname)
    (setq count (xem-zero-buffer bg))
    (goto-char (point-min))
    (save-buffer 0)                               ; zero means never make a backup
    (message "  xem: guardize complete using '%s', stripped %d out-data fields." guardname count)
  (message "  xem: guardize complete using '%s'." guardname) ))

(defun xem-reset-me ()
  "Reset -- zero and double-dot -- the current buffer.

Identical to calling `xem-double-dot-me' and `xem-zero-me'.

This function does NOT rely on the various field value data-type
prompts being present and correct."
  (interactive)
  (save-excursion
    (let* ((bc      (current-buffer)      )  ; lisp buffer object
           (sitfor  (xem-toggle-SITFOR 0) )
           (count1  0                     )  ; out-data changes
           (count2  0                     )) ; in-data changes
      (xem-toggle-SITFOR -1)
      (setq count1 (xem-zero-buffer bc))
      (setq count2 (xem-dotdot-buffer bc))
      (if (> sitfor 0) (xem-toggle-SITFOR +1))
      (message "  xem: reset-me: complete \
resetting %d out-data fields and double-dotting %d in-data time-series." count1 count2) )))

(defun xem-double-dot-me ()
  "Double dot the current buffer.

This function does NOT rely on the various field value data-type
prompts being present and correct."
  (interactive)
  (save-excursion
    (let* ((bc     (current-buffer) )        ; lisp buffer object
           (count  0                ))       ; in-data changes
      (setq count (xem-dotdot-buffer bc))
      (message "  xem: double-dot-me: complete double-dotting %d in-data time-series." count) )))

(defun xem-zero-me ()
  "Zero the current buffer.

This function does NOT rely on the various field value data-type
prompts being present and correct."
  (interactive)
  (save-excursion
    (let* ((bc     (current-buffer) )        ; lisp buffer object
           (count  0                ))       ; in-data changes
      (setq count (xem-zero-buffer bc))
      (message "  xem: zero-me: complete resetting %d out-data fields." count) )))

(defun xem-dotdot-buffer (buffer)
  "Double-dot the in-data time-series in BUFFER.

This function does NOT rely on the various field value data-type
prompts being present and correct."
  (save-excursion
    (let* ((angle   ">"                  )   ; indicates in-data (input data)
           (value   ""                   )
           (count   0                    )   ; number of changes
           (line1   nil                  )   ; cut
           (line2   nil                  )   ; paste
           (parts   nil                  )   ; split 'line1'
           (left    nil                  )   ; left side
           (right   nil                  )   ; right side
           (type    "(not set)"          )   ; type in { null special timeseries }
           (vals    nil                  )   ; string values
           (len     nil                  )   ; length
           (dotdot  nil                  ))  ; trailing ellipses
      ;; test buffer
      (if (not (bufferp buffer))
          (error "  xem: dotdot-buffer: given buffer failed buffer test"))
      (set-buffer buffer)                    ; make 'buffer' current for editing purposes
      (goto-char (point-min))
      ;; add initial blank line if required because later logic assumes this to be so
      (when (xem-is-field angle) (goto-char (point-min)) (insert "\n") (goto-char (point-min)))
      ;; loop in-data fields
      (while (not (eobp))
        (while (progn (forward-line +1)      ; CAUTION: order is significant
                      (xem-is-field angle))  ; CAUTION: odd logic!
          (incf count)
          (beginning-of-line)
          (setq line1 (thing-at-point 'line))
          (delete-region (line-beginning-position) (line-end-position))  ; the newline char remains tho
          ;; split field
          (setq parts (split-string line1 angle nil))
          (setq left  (nth 0 parts))
          (setq right (nth 1 parts))                   ; could be nil
          (setq right (xem-string-trim right))         ; could be empty string
          ;; split and value
          (cond
           ;; { null }
           ((= (length right) 0)                       ; empty string
            (setq len 0)
            (setq type "null"))
           ;; already dotted
           ((string-match "\\.\\.$"  right) (setq type "dotted"))
           ;; { S X }
           ((string-equal (substring right 0 1) "\"")  ; leading double-quote
            (setq vals (split-string (substring right 1 -1) "\" +\"" nil))
            (setq len (length vals))
            (cond
             ((= len 1) (setq type "single"))
             (t         (setq type "undotted"))))
           ;; { I F B } noting that boolean is handled as integer
           (t                                          ; by elimination
            (setq vals (split-string right " " nil))
            (setq len (length vals))
            (cond
             ((= len 1) (setq type "single"))
             (t         (setq type "undotted")))))
          ;; reassemble
          (cond
           ((string-match type "null"    ) (setq dotdot ""   ))
           ((string-match type "special" ) (setq dotdot ""   ))
           ((string-match type "single"  ) (setq dotdot ""   ))
           ((string-match type "dotted"  ) (setq dotdot ""   ))
           ((string-match type "undotted") (setq dotdot " .."))
           (t (error "  xem: dotdot-buffer: error: type not properly determined: %s" type)))
          (setq line2 (concat left angle " " right dotdot))
          ;; insert
          (insert line2)
          (message "  xem: dotdot-buffer: line2 : %2d %-10s : %s" len type line2)
          (sit-for SITFOR)
          )) ; 'while' x 2
      ;; completion reporting
      (message "  xem: dotdot-buffer: complete, %d values removed or modified." count)
      count )))

(defconst xem-empty-out-fields
  (list "process-id" "run-kind" "used-svn" "simulate-return")
  "List of out-data field to remain strictly empty.")

(defun xem-zero-buffer (buffer)
  "Zero BUFFER.

This function does NOT rely on the various field value data-type
prompts being present and correct."
  (save-excursion
    (let* ((angle   "<"                  )   ; indicates out-data (output data)
           (emptys  xem-empty-out-fields )
           (value   ""                   )
           (count   0                    )   ; number of changes
           (line1   nil                  )   ; cut
           (line2   nil                  )   ; paste
           (parts   nil                  )   ; split 'line1'
           (left    nil                  )   ; left side
           (right   nil                  )   ; right side
           (type    nil                  )   ; type in { null special string int float }
           (vals    nil                  )   ; string values
           (val     nil                  )   ; first 'vals'
           (nums    nil                  )   ; number values
           (num     nil                  )   ; first 'nums'
           (len     nil                  ))  ; length
      ;; teste buffer
      (if (not (bufferp buffer))
          (error "  xem: zero-buffer:: given buffer failed buffer test"))
      (set-buffer buffer)                ; make 'buffer' current for editing purposes
      (goto-char (point-min))
      ;; add initial blank line if required because later logic assumes this to be so
      (when (xem-is-field angle) (goto-char (point-min)) (insert "\n") (goto-char (point-min)))
      ;; loop out-data fields
      (while (not (eobp))
        (while (progn (forward-line +1)      ; CAUTION: order is significant
                      (xem-is-field angle))  ; CAUTION: odd logic!
          (incf count)
          (beginning-of-line)
          (setq line1 (thing-at-point 'line))
          (delete-region (line-beginning-position) (line-end-position))  ; the newline char remains tho
          ;; split field
          (setq parts (split-string line1 angle nil))
          (setq left  (nth 0 parts))
          (setq right (nth 1 parts))                   ; could be nil
          (setq right (xem-string-trim right))         ; could be empty string
          ;; split and value
          (cond
           ;; { null }
           ((= (length right) 0)                       ; empty string
            (setq len 0)
            (setq value "")
            (setq type "null"))
           ;; { s S x X } with x, being a character stream, is essentially a string
           ((string-equal (substring right 0 1) "\"")  ; leading double-quote
            (setq vals (split-string (substring right 1 -1) "\" +\"" nil))
            (setq val (nth 0 vals))
            (setq len  (length vals))
            (setq value " \"_\"")                      ; was simply " \"\""
            (if (or (> len 1)
                    (equal (substring right -2 nil) ".."))
                (setq value (concat value " ..")))
            (setq type "string"))
           ;; { i I f F b B } noting that boolean is handled as integer
           (t                                          ; by elimination
            (setq vals (split-string right " " nil))
            (setq val (nth 0 vals))
            (setq len (length vals))
            (setq num (string-to-number val))
            (cond
             ((integerp num) (setq type "int"  ) (setq value " 0"  ))
             ((numberp  num) (setq type "float") (setq value " 0.0"))
             (t (message "  xem: zero-buffer: problem: type not identified, right %s" right)))
            (if (> len 1) (setq value (concat value " ..")))))
          ;; special treatment for the program fields
          (dolist (empty emptys)
            (setq regex (concat "" empty ""))
            (when (string-match regex left)
              (setq type "special")
              (setq value "")))
          ;; reassemble
          (setq line2 (concat left angle value))
          ;; insert
          (insert line2)
          (message "  xem: zero-buffer: line2 : %2d %-7s : %s" len type line2)
          (sit-for SITFOR)
          )) ; 'while' x 2
      ;; completion reporting
      (message "  xem: zero-buffer: complete, %d values removed or modified." count)
      count )))

(defun xem-strip-field-data (buffer angle)
  "Strip field data from buffer BUFFER using ANGLE.

BUFFER is tested for existence.  ANGLE must be either \">\" for
in-data or \"<\" for out-data.  If not, an error is generated."
  (let ((count  0 ))                         ; strip count
    (if (not (string-match "^[<>]$" angle))
        (error "  xem: strip-field-data: invalid ANGLE argument '%s' supplied (coding error)" angle))
    (if (not (bufferp buffer))
        (error "  xem: strip-field-data: given buffer failed buffer test"))
    (set-buffer buffer)                      ; make 'buffer' current for editing purposes
    (goto-char (point-min))
    ;; add initial blank line if required because later logic assumes this to be so
    (when (xem-is-field angle) (goto-char (point-min)) (insert "\n") (goto-char (point-min)))
    ;; loop out-data fields
    (while (not (eobp))
      (while (progn (forward-line +1)        ; CAUTION: order is significant
                    (xem-is-field angle))    ; CAUTION: odd logic!
        (incf count)
        (beginning-of-line)
        (setq line (thing-at-point 'line))
        (delete-region (line-beginning-position) (line-end-position))  ; the newline char remains tho
        (setq parts (split-string line angle nil))
        (insert (nth 0 parts))
        (insert angle)
        (message "  xem: strip-field-data: count %2d line '%s'" count line)
        (sit-for SITFOR)
        )) ; 'while' x 2
    (message "  xem: strip-field-data: buffer stripped, %d values removed using angle '%s'"
             count angle)
    count ))

;; ---------------------------------
;;  hilighter functions
;; ---------------------------------

;; inspiration: markerpen.el --- colour and highlight arbitrary sections of buffers

(defun xem-hilighter-entity ()
  "Hilight (apply color to) the current xeona entity."
  ;; a somewhat smart interface to `xem-hilighter-region'
  (interactive)
  (save-excursion
    (let* ((entity-end  (1- (xem-move-entity-next)) )  ; move down first, note the small adjustment
           (entity-beg  (xem-move-entity-prior)     ))
      (if (and entity-beg entity-end)
          (progn
            (xem-hilighter-region entity-beg entity-end)
            (message "  xem: hilighter entity complete using %d:%d" entity-beg entity-end))
        (message "  xem: hilighter entity unable to identify a current entity")) )))

;; hilighter colors

(defface xem-hilighter-a '((t :background "dark sea green")) "hilighter a")     ; vivid
(defface xem-hilighter-b '((t :background "khaki3"        )) "hilighter b")     ; tertiary
(defface xem-hilighter-c '((t :background "peru"          )) "hilighter c")     ; dark but subtle
(defface xem-hilighter-d '((t :background "beige"         )) "hilighter d")     ; same as entity label

;; hilighter support

(make-variable-buffer-local 'xem-hilighter-overlays)

(defun xem-hilighter-region (reg-beg reg-end)
  "Hilight (apply color to) the current region ranging REG-BEG thru REG-END.

The hilighters are implemented using overlays, so they do not, in
any way, affect the contents of the buffer -- even if the buffer
uses text properties.  Because of this you can quite happily use
hilighters together with a mode which uses font locking.

The function `xem-hilighter-clear-buffer' removes all marks."
  (interactive "r")                          ; indicates region, no i/o
  (let ()
    (setq new-hilighter-overlay (make-overlay reg-beg reg-end))
    (deactivate-mark)                        ; for transient-mark-mode
    (add-to-list 'xem-hilighter-overlays new-hilighter-overlay)
    (overlay-put new-hilighter-overlay 'face 'xem-hilighter-d) ))     ; change color here

(defun xem-hilighter-clear-buffer ()
  "Clear all hilights from the current buffer."
  (interactive)
  (mapcar 'xem-hilighter-delete-overlay xem-hilighter-overlays)
  (message "  xem: hilight clear buffer complete."))

(defun xem-hilighter-clear-region (reg-beg reg-end)
  "Clear all hilights from the current region ranging REG-BEG thru REG-END."
  (interactive "r")
  (if (use-region-p)
      (progn
        (mapcar 'xem-hilighter-delete-overlay (overlays-in reg-beg reg-end))
        (deactivate-mark)                    ; for transient-mark-mode
        (message "  xem: hilight clear region complete."))
    (message "  xem: hilight clear region complete but no region was active.") ))

(defun xem-hilighter-delete-overlay (overlay)
  "Delete the supplied OVERLAY if it is a hilighter overlay."
  (setq xem-hilighter-overlays (delq overlay xem-hilighter-overlays))
  (delete-overlay overlay))

;; ---------------------------------
;;  overwrite simulate-return
;; ---------------------------------

; the form is: >    simulate-return                               < "success (1)"<

(defun xem-fix-incomplete-run (&optional return)
  "Overwrite the 'simulate-return' field when xeona fails to write the model.  Add RETURN if given."
  (save-excursion
    (let ((fieldname  "simulate-return"                 )
          (splif      "<"                               )
          (tag1       "model failed before write"       )
          (tag2       "xeona exit"                      )
          (msg1       nil                               )
          (msg2       nil                               )
          (line       nil                               ))
      ;; create message strings
      (setq msg1 tag1)
      (if (not return)
          (setq msg2 tag2)                    ; but the 'return' should normally be known
        (if (integerp return)
            (setq msg2 (format "%s %d" tag2 return))
          (setq msg2 (format "%s %s" tag2 return))))
      ;; insert message
      (goto-char (point-min))
      (if (re-search-forward fieldname nil t)
          (progn
            (setq line (thing-at-point 'line))
            (delete-region (line-beginning-position) (line-end-position))
            (setq parts (split-string line splif nil))
            (insert (nth 0 parts))
            (insert (concat splif " \"" "** " msg1 " **" "\"" "\n"))  ; trailing newline
            (insert (concat "\n" "    " "** " msg2 " **"))            ; skip a line first
            (message "  xem: fix-incomplete-run: complete using '%s'." return))
        (message "  xem: fix-incomplete-run: unable to locate fieldname '%s'." fieldname)) )))

;; ---------------------------------
;;  yeek and exit code information
;; ---------------------------------

; typical yeek:    10 = report from 'xeona::isTwoContained' call

(defun xem-get-yeek (yeek)
  "Return yeek explanation associated with YEEK."
  (interactive)
  (let ((record  12))                        ; record 12, may change but unlikely
    (xem-get-info record yeek)))

(defun xem-get-code (code)
  "Return exit code explanation associated with CODE."
  (interactive)
  (let ((record  13))                        ; record 13, may change but unlikely
    (xem-get-info record code)))

(defun xem-get-info (record number)
  "Return explanation from RECORD associated with NUMBER."
  (let* ((call-1   (concat xem-binary " --usage 2>/dev/null") )
         (call-2   (format "%s%d%s" "awk 'BEGIN { RS = \"\\n\\n\"} { if ( NR == "
                           record
                           " ) print $0 }' | tail --lines=+2" ) )
         (call     (concat call-1 " | " call-2)   )
         (capture  (shell-command-to-string call) )
         (lines    (split-string capture "\n" t)  )  ; 't' is omit nulls ('nil' makes no difference)
         (regex    nil                            )
         (explain  nil                            ))
    (if (integerp number)
        (progn
          (setq regex (format "^[[:space:]]*%d = \\(.*\\)$" number))
          (message "  xem: get-yeek: regex %s" regex)
          (dolist (line lines)
            (if (string-match regex line)
                (setq explain (match-string 1 line))))
          (message "  xem: get-yeek: explain %s" explain)
          explain)                           ; return the explanation
      (error "  xem: get-yeek: number not integer")) ))

;; ---------------------------------
;;  prioritize coloration
;; ---------------------------------

;; prioritize coloration over current line highlighting
;; source: http://www.emacswiki.org/emacs/HiLock
;; status: works as advertised up to about 2000 lines

(defadvice hi-lock-set-pattern (around use-overlays activate)
  "Function `hi-lock-mode' should prioritize coloration over current line highlighting."
  (let ((font-lock-fontified nil))
    ad-do-it))

;; ---------------------------------
;;  buffer switch functionality
;; ---------------------------------

;; key binding (see above) is common with 'xeona.el'

(defun xem-switch-to-useful-buffer ()
  "Switch to useful buffer."
  (interactive)
  (let ((target  (get-buffer xem-buffer-xeona-output) ))
    (if (and (not (null target))                  ; evaluated in order written
             (not (eq (current-buffer) target)))  ; 'eq' for same lisp object
        (switch-to-buffer target)
      (list-buffers)
      (other-window)) ))

;; ---------------------------------
;;  guard file processing
;; ---------------------------------

;; menu item: "Create model from selected guard"
;; temporary while developing ad-hoc models

(defun xem-unguard (guardname)
  "Unguard the GUARDNAME file."
  (let* ((guard    guardname                        )
         (unguard  (xem-unguard-filename guardname) )) ; possibly "stub.xem", possibly nil
    (if unguard                                        ; name recovery worked
        (if (file-exists-p guard)
            (progn
              (copy-file guard unguard 1)    ; "1" is seek confirmation on overwrite - shouldn't happen
              (find-file unguard)
              (message "  xem: futz processed guard file '%s'." guard))
          (message "  xem: unguard abandoning task as guard file '%s' not found." guard))
      (message "  xem: unguard abandoning task as selected file '%s' is not a guard file." guard)) ))

(setq use-file-dialog t)                     ; 't' is use mouse dialog from drop-down menu

(defun xem-unguard-current ()
  "Unguard the current file.

A buffer file name interface to `xem-unguard'."
  (interactive)
  (let ((guardname  (buffer-file-name nil) ))     ; this file, possibly "stub.guard.xem"
    (xem-unguard guardname) ))

(defun xem-unguard-prompted (guardname)
  "Unguard the prompted GUARDNAME file.

An ask-for-filename interface to `xem-unguard'."
  (interactive "f")
  (xem-unguard guardname))

(defun xem-unguard-filename (filename)
  "Unguard FILENAME, also acts as a predicate."
  (let* ((model-ext  (substring xem-model-extension 1)   )  ; now "xem"
         (guard-tag  (substring xem-model-guard-tag 1)   )  ; now "guard"
         (str-1      (file-name-sans-extension filename) )  ; attempt to trim ".xem"
         (str-2      (file-name-sans-extension str-1)    )
         (str-3      (file-name-extension str-1)         )) ; may return "guard", also "" or nil
    (if (and (not (null str-3))                             ; evaluated in order written
             (string-match str-3 guard-tag))
        (progn
          (message "  xem: unguard-file: true: %s." str-3)
          (concat str-2 "." model-ext))                     ; return unguarded name
      (message "  xem: unguard-file: false: %s." str-3)
      nil) ))                                               ; return nil (previously an empty string)

;; ---------------------------------
;;  screen modes
;; ---------------------------------

;; CAUTION: the toggle semantics here differ from the
;; usual toggle behavior for emacs

(defun xem-fullscreen-me-toggle ()
  "Toggle full-screen mode."
  (interactive)
  (xem-fullscreen-me t))

(defun xem-fullscreen-me (&optional toggle)
  "Adopt full-screen mode.  If TOGGLE is t, then alternate with part-screen mode."
  ;; CAUTION: contains protected platform-specific code
  ;; http://www.emacswiki.org/emacs/FullScreen
  (cond
   ((eq system-type 'gnu/linux)
    ;; 1 = fixed full, 2 = toggle for each call
    (if toggle
        (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0))
      (x-send-client-message   nil 0 nil "_NET_WM_STATE" 32 '(1 "_NET_WM_STATE_FULLSCREEN" 0))))
   ((eq system-type 'windows-nt)
    (error "  xem: full-screen mode not coded for Windows operating system"))
   (t
    (error "  xem: full-screen mode not coded for this operating system"))) )

(defun xem-maxscreen-me-toggle ()
  "Toggle maximize mode."
  (interactive)
  (xem-maxscreen-me t))

(defun xem-maxscreen-me (&optional toggle)
  "Adopt maximize mode.  If TOGGLE is t, then alternate with part-screen mode."
  ;; CAUTION: contains protected platform-specific code
  ;; http://www.emacswiki.org/emacs/FullScreen
  (cond
   ((eq system-type 'gnu/linux)
    ;; 1 = fixed full, 2 = toggle for each call
    (if toggle
        (progn
          (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
          (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
      (x-send-client-message   nil 0 nil "_NET_WM_STATE" 32 '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
      (x-send-client-message   nil 0 nil "_NET_WM_STATE" 32 '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))))
   ((eq system-type 'windows-nt)
    (error "  xem: maximize mode not coded for Windows operating system"))
   (t
    (error "  xem: maximize mode not coded for this operating system"))) )

;; ---------------------------------
;;  diff output functions
;; ---------------------------------

(defconst xem-default-face-height (face-attribute 'default :height) "Current font size.")
(defconst xem-small-face-height   90                                "Small font size.")

;; 'ediff' code
;; http://braeburn.aquamacs.org/code/master/lisp/ediff.el
;; http://www.emacswiki.org/emacs/SetFonts
;; note also <S-down-mouse-1> for interactive "Change Default Buffer Face" menu

(add-hook 'ediff-before-setup-hook 'xem-ediff-entry)
(add-hook 'ediff-cleanup-hook      'xem-ediff-exit)    ; also 'ediff-quit-hook'

(defun xem-ediff-entry ()
  "Xem 'ediff' entry function."
  (let ((face-height  xem-small-face-height ))
    (message "  xem: ediff-entry: commencing")
    (setq ediff-split-window-function 'split-window-horizontally)     ; ediff side-by-side
;;; (setq ediff-keep-variants nil)                                    ; ediff close created windows
    (set-face-attribute 'default (selected-frame) :height face-height)
    (xem-fullscreen-me nil)                                           ; CAUTION: [1]
    (message "  xem: ediff-entry: complete") ))
    ;; [1] fixed full-screen must come after face height

(defun xem-ediff-exit ()
  "Xem 'ediff' exit function."
  ;; CAUTION: this function is sensitive to the ordering of statements
  (let ((face-height  xem-default-face-height )
        (buffer-a     ediff-buffer-A          )   ; note also 'ediff-window-A' etc
        (buffer-b     ediff-buffer-B          )
        (quick-help   ediff-control-frame     ))  ; CAUTION: frame details must be captured early on
    (message "  xem: ediff-exit: commencing")
    (message "  xem: ediff-exit: buffers: A B %s %s" buffer-a buffer-b)
    (message "  xem: ediff-exit: quick-help: %S" quick-help)
    (kill-buffer      buffer-b)
    (switch-to-buffer buffer-a)
    (delete-other-windows)
    (set-face-attribute 'default nil :height face-height)   ; 'nil' is reset all
    (xem-fullscreen-me t)                                   ; CAUTION: [1]
    (if quick-help (delete-frame quick-help))               ; nuke the little quick help frame
    (message "  xem: ediff-exit: complete") ))
    ;; [1] fixed full-screen must come after face height

(defun xem-diff-me-ediff ()
  "Ediff current model after setting face height and adopting full-screen mode."
  (interactive)
  (xem-diff-me ?e))

(defun xem-diff-me (type)
  "Diff current model in new buffer, based on TYPE key press.

    e    ediff
    u    unix diff    (command-line unix diff)
    ?    help         (this message)"
  (interactive "cType character for diff type ([e]diff unix-diff)? ")
  (let* ((diff-switches    ""                                  )
         (diff-buffer      xem-buffer-xem-diff                 )
         (backup-tag       xem-backup-tag                      )
         (xem-file-1       (buffer-file-name nil)              )
         (xem-file-2       (concat xem-file-1 backup-tag)      )
         (xem-leaf-1       (file-name-nondirectory xem-file-1) )
         (xem-face-height  0                                   ))
    (if (file-readable-p xem-file-2)
        (progn
          (message "  xem: diff-buffer: type %c" type)
          (if (equal type 13)                ; catch return key ^M
              (setq type ?e))                ; set default here
          (cond                              ; elisp case statement
           ;; ediff, see also: http://www.emacswiki.org/emacs/EdiffMode
           ((equal type ?e)
            (ediff xem-file-1 xem-file-2))
           ;; diff(1), see diff manpage
           ((equal type ?u)
            (diff xem-file-1 xem-file-2 diff-switches)
            (other-window +1)
            (rename-buffer diff-buffer)
            (other-window -1))

           ((equal type ??)
            (describe-function 'xem-diff-me))
           (t
            (error "  xem: diff me quitting without action, character '%c' not supported" type)))
          (message "  xem: diff me complete using '%s' and backup tag '%s'." xem-file-1 backup-tag))
      (message "  xem: diff me abandoned as backup file '%s' not present or not readable." xem-file-2))
    ))

;; ---------------------------------
;;  clean model functions
;; ---------------------------------

(defun xem-clean-model ()
  "Clean model of \"(some problem)\" entries.

Largely obsolete, but could be useful in combination with --yeek 33"
  (interactive)
  (save-excursion
    (let ((find   "\"(some problem)\"")
          (repl   "\"\""              )
          (count 0))                         ; replacements made
      (goto-char (point-min))                ; beginning of buffer
      (while (re-search-forward find nil t)
        (replace-match repl t t)
        (incf count))                        ; increment count
      (message "  xem: clean model complete using '%s' with %d replacements." find count))))

;; ---------------------------------
;;  run model and related functions
;; ---------------------------------

(defun xem-get-field-value (fieldname splif)
  "Return the value associated with FIELDNAME and regex SPLIF.  Else return nil.

The SPLIF regex is usually, but not limited to, one of \"<\" \">\" \"[<>]\"."
  (save-excursion
    (let ((line  nil ))                      ; field line, if present
      (goto-char (point-min))
      ;; locate fieldname, grab line, strip newline, and return
      (if (re-search-forward fieldname)
          (progn
            (setq line (thing-at-point 'line))
            (setq line (xem-string-trim line))
            (message "  xem: get-field: line found '%s'" line)
            (xem-extract-data line fieldname splif))   ; success return value
        (message "  xem: get-field: not found (returning nil)")
        nil) )))                                       ; fail return value

(defun xem-run-me (numbers)
  "Run xeona using NUMBERS to code for 'report|mode|yeek' values.

This function provides a pass-thru interface to
`xeona-run-me-call'.  The various system calls and the buffer and
file modification commands are located in `xeona-run-me-call'.

This function normally recovers and passes on any sensible
'program.run-script-settings' options.  A negative sign means
omit this feature.  CAUTION: leading zeros is stripped off, hence
\"080\" produces an erroneous \"80\" which is also different from
just \"8\".

Out of range values are checked.  High values for 'report' and 'mode' are
reset as required.  An invalid 'yeek' value results in abandonment.
A report value above 7 results in '--jumpy' being applied as well.

Prefix arguments 0 thru 255 can be used to include more obscure options:
    0  no effect
    1  add --again
    2  add --jumpy
    4  add --krazy
    8  add --pepper
   16  add --zero
   32  add --tout dumb
   64  add --tout wxt
  128  add --tout x11
  256  add --tout svg

Note that the last three are mutually exclusive."
  (interactive "nEnter report [ mode [ yeek ]] (try 2 or 2714 or 772 or 280 with any leading zeros stripped): ")
  (let* ((pre       current-prefix-arg )     ; could be nil
         (noas      ""                 )     ; number as string
         (said      ""                 )     ; report
         (mode      "7"                )     ; default mode
         (yeek      "0"                )     ; default yeek
         (maxyeek   60                 )     ; current maximum yeek value (can change)
         (options   ()                 )     ; options list
         (touts     0                  )     ; used to check for multiple '--tout' entries
         (output    ""                 )     ; final output
         (exittrip  nil                )     ; from 'program.run-script-settings.script-option-exittrip'
         (nodata    nil                )     ; from 'program.run-script-settings.script-option-nodata'
         (jumpy     nil                ))    ; from 'program.run-script-settings.script-option-jumpy'
    ;; initial reporting
    (message "  xem: run-me: commencing with entry: %g" numbers)
    (message "  xem: run-me: current prefix arg: %s" current-prefix-arg)
    ;; process and check the 'current-prefix-arg'
    (if (null pre) (setq pre 0))
    (if (or (< pre 0) (> pre 511))
        (error "  xem: run-me: given prefix argument not supported: %d" pre))
    ;; if positive, process any 'program.run-script-settings' options
    (if (< numbers 0)
        (message "  xem: run-me: negative input, data sourced from XEM file NOT being used")
      (setq exittrip (xem-get-field-value "script-option-exittrip" ">"))
      (setq nodata   (xem-get-field-value "script-option-nodata"   ">"))
      (setq jumpy    (xem-get-field-value "script-option-jumpy"    ">"))
      (if exittrip (push (concat "--exittrip " exittrip) options))
      (if nodata (if (not (string-equal nodata "0")) (push "--nodata" options)))
      (if jumpy  (if (not (string-equal jumpy  "0")) (push "--jumpy"  options))))
    ;; process implied options
    (setq noas (format "%d" (abs numbers)))
    (setq len (length noas))
    (message "  xem: run-me: numbers %d" numbers)
    (message "  xem: run-me: length %d" len)
    (if (>= len 1) (setq said (substring noas 0   1)))
    (if (>= len 2) (setq mode (substring noas 1   2)))
    (if (>= len 3) (setq yeek (substring noas 2 len))) ; checked later against current 'maxyeek'
    (if (>  (string-to-number said) 7)                 ; seven is maximum report value
        (progn
          (setq said  "7")                             ; maximum '--report' value
          (if jumpy
              (if (string-equal jumpy "0")             ; set '--jumpy' if not already so
                  (push "--jumpy") options))))
    (if (> (string-to-number mode) 9) (setq mode  "7"))
    (if (> (string-to-number yeek) maxyeek)
        (error "  xem: run-me: refusing to run with yeek %s as it exceeds %d" yeek maxyeek))
    (push (concat "--report " said) options)
    (push (concat "--mode "   mode) options)
    (push (concat "--yeek "   yeek) options)
    ;; process prefix arguments using bitwise masks, see 'logand' help
    (if (not (zerop (logand pre   1))) (push "--again"     options))
    (if (not (zerop (logand pre   2))) (push "--jumpy"     options))
    (if (not (zerop (logand pre   4))) (push "--krazy"     options))
    (if (not (zerop (logand pre   8))) (push "--pepper"    options))
    (if (not (zerop (logand pre  16))) (push "--zero"      options))
    (if (not (zerop (logand pre  32))) (push "--tout dumb" options))
    (if (not (zerop (logand pre  64))) (push "--tout wxt"  options))
    (if (not (zerop (logand pre 128))) (push "--tout x11"  options))
    (if (not (zerop (logand pre 256))) (push "--tout svg"  options))
    ;; integrity check on option '--tout'
    (dolist (option options)
      (if (string-match "--tout" option)
          (incf touts)))
    (if (> touts 1)
        (error "  xem: run-me: multiple '--tout' options requested: %d" touts))
    ;; complete
    (setq output (mapconcat 'identity (reverse options) " "))    ; CAUTION: [1]
    ;; [1] 'identity' is built-in function and 'mapconcat'
    ;; returns an empty string if 'options' is '()'
    (message "  xem: run-me: options %s" output)
    (sleep-for 3)                                      ; in order to read message
    (xem-run-me-call output) ))                        ; key call -- see below

(defvar xem-file-name
  nil
  "As-run xem file with full path, use function `file-name-nondirectory' to obtain the leaf.")

(defun xem-run-me-call (options)
  "Invoke 'xeona' using given OPTIONS.

Called by `xem-run-me' as the public point of contact."
  ;; file abbreviation (conversion from "$HOME/$USER" to "~") has no effect on 'xeona' logging
  (let* ((final-refresh 1e6                                 )    ; size limit for final 'xem-mode' refresh
         (out-buffer    xem-buffer-xeona-output             )
         (bm            (current-buffer)                    )    ; xem file buffer
         (bo            (get-buffer-create out-buffer)      )    ; output capture buffer
         (xem-file      (buffer-file-name bm)               )
         (xem-leaf      (file-name-nondirectory xem-file)   )
         (temp          (split-string options)              )
         (yeekarg       (car (last temp))                   )    ; rather brittle!
         (yeek          (string-to-number yeekarg)          )
         (msgs          ()                                  )    ; list of final messages
         (msg           nil                                 )    ; stringified 'msgs'
         (call          ""                                  )
         (code          0                                   )    ; exit code
         (timestamp-1   nil                                 )
         (timestamp-2   nil                                 )
         (update        "(not set)"                         )
         (vhold         0                                   )    ; vertical (line) hold position
         (hhold         0                                   ))   ; horizontal (column) hold position
    ;; reporting
    (message "  xem: run-me-call: binary   %s" xem-binary)
    (message "  xem: run-me-call: options  %s" options)
    (message "  xem: run-me-call: xem file %s" xem-file)
    ;; update xem filename for external visibility for use by 'xog-mode' for instance
    (setq xem-file-name xem-file)
    ;; confirm not a guard file, then check write permission
    (if (xem-unguard-filename xem-file)
        (message "  xem: run me call given guard file '%s', abandoning task without action."
                 xem-file)
      (if (not (file-writable-p xem-file))
          (message "  xem: run me call given read-only file '%s', abandoning task without action."
                   xem-file)
        ;; create call
        (setq call (concat xem-binary " " options " --file " xem-file))
        (message "  xem: run-me-call: call %s" call)
        ;; prompted save if required ('bm' assumed here)
        (if (buffer-modified-p)
            (if (y-or-n-p "Save this model in order to continue? ")
                (save-buffer)
              (error "  xem: run me call abandoning '%s' call without action" call)))
        (save-buffer)                                       ; defensive programming, not really needed
        ;; record xem buffer
        (setq vhold (+ (count-lines (point-min) (point))    ; "1" indicates the first line
                       (if (= (current-column) 0) 1 0)))    ; correction for start of line, note:
        (setq hhold (current-column))                       ; "0" indicates the start-of-line column
        (message "  xem: run-me-call: vhold hhold: %d %d" vhold hhold)
        (setq timestamp-1 (xem-file-modified xem-file))     ; capture last modified
        (sleep-for 1)                                       ; to ensure distinct last modified times
        ;; prepare output buffer
        (set-buffer bo)                                     ; 'bo' is the output buffer
        (erase-buffer)                                      ; delete entire contents, ignores narrowing
        ;; MAKE CALL
        (setq code (shell-command call bo nil))             ; 'nil' means mingle stdout and stderr
        (message "  xem: run-me-call: xeona ran synchronously and returned '%s'" code)
        ;; rework output buffer
        (xog-mode)                                          ; xeona output mode
        (toggle-truncate-lines 1)                           ; "1" is never fold (wrap) long lines
        (delete-trailing-whitespace)                        ; may (and probably does) modify the buffer
        (end-of-buffer)                                     ; jump to end
        ;; reload xem file
        (switch-to-buffer bm)
        (delete-other-windows)                              ; disable to keep both buffers visible
        (revert-buffer t t t)                               ; 'ignore-auto''noconfirm''preserve-modes'
        (setq timestamp-2 (xem-file-modified xem-file))     ; capture last modified
        (if (equal timestamp-1 timestamp-2)                 ; [1]
            (progn
              (setq update "1 = UNMODIFIED")
              (xem-fix-incomplete-run code))
          (setq update "0 = modified"))
        (goto-line vhold)                                   ; CAUTION: must precede next line
        (move-to-column hhold)
        (message "  xem: run-me-call: xem file revisited: xem-file")
        ;; completion reporting
        (push (format "binary   : %s"      (file-name-nondirectory xem-binary)) msgs)
        (push (format "options  : %s"      options)                             msgs)
        (push (format "model    : %s"      xem-leaf)                            msgs)
        (push (format "yeek     : %2d = %s" yeek (xem-get-yeek yeek))           msgs)
        (if (integerp code)                                 ; can be "Aborted"
            (push (format "exit     : %2d = %s" code (xem-get-code code))       msgs)
          (push (format "exit     : '%s' (from shell not xeona)" code)          msgs))
        (push (format "xem file :  %s" update)                                  msgs)
        (setq msg (mapconcat 'identity (nreverse msgs) "\n"))    ; stringify [1]
        ;; [1] 'identity' is built-in function and 'mapconcat' returns an empty string if 'msgs' is '()'
        ;; reapply mode only if buffer is small, else the matcher buffer overflows
        (if (< (buffer-size) final-refresh)
            (xem-mode))                                     ; reapply mode to regain highlighting
        ;; final msg
        (message "xem: run-me-call: complete except for the final report")
        (message "%s" msg))) ))

        ;; [1] tried (verify-visited-file-modtime) to no avail

(defun xem-file-modified (filename)
  "Return last modified timestamp for FILENAME.

More specifically, return the fifth field from `file-attributes', a list
of two integers, which represents the timestamp.

Typical output (19516 57041) as the fourth element of `file-attributes.'
Typical output (57 46 23 13 7 2010 2 t 7200) under `decode-time.'"
  (let* ((attribs (file-attributes filename 'string) )
         (timestamp (nth 4 attribs)                  ))     ; zero-based list indexing
    (message "  xem: file-modified complete using '%s' and giving %S and %s."
             filename timestamp (decode-time timestamp))
    timestamp))                                             ; return timestamp

(defun xem-run-tidy ()
  "Run xeona '--tidy' on current buffer.

Calls `xem-run-me-call' using option '--tidy'."
  (interactive)
  (let ((options "--tidy"))
    (xem-run-me-call options)))

(defconst xem-run-me-call-regex              ; order counts
  "--[-=[:alnum:]]+[[:space:]]arg\\|--[-=[:alnum:]]+[[:space:]][+[:digit:]]+\\|--[-=[:alnum:]]+")

(defun xem-usage-filtered (filter)
  "Filter xeona '--usage' output based on FILTER regex.

Calls `xem-run-me-call' using option '--usage' and then applies `string-match'."
  (interactive "sEnter a regex usage filter (or + for all) ")
  (let* ((buffer   xem-buffer-xeona-usage                     )
         (call     (concat xem-binary " --usage 2>/dev/null") )
         (capture  (shell-command-to-string call)             )
         (lines    (split-string capture "\n" t)              )  ; 't' is omit nulls ('nil' also works)
         (regex    xem-run-me-call-regex                      )
         (hits     ""                                         ))
    (cond ((string-equal filter "+")
           (switch-to-buffer buffer)
           (insert capture)
           (delete-trailing-whitespace)
           (goto-char (point-min))
           (highlight-phrase regex 'xem-hi-color-B07)
           (message "  xem: full usage complete using '%s'." filter))
          (t
           (dolist (line lines)
             (if (string-match filter line)
                 (setq hits (concat hits line "\n"))))
           (if (string-match "\n$" hits)                    ; trim trailing newline
               (setq hits (replace-match "" t t hits)))   ; fixed case and literal
           (message "  xem: filtered usage using regex '%s':\n%s" filter hits))) ))

;; ---------------------------------
;;  buffer functions
;; ---------------------------------

;; interactive function `xem-revisit' is provided as a menu item,
;; however (revert-buffer t t t) may be equivalent and better
;; style -- note too that, at the time of writing, the
;; 'Global-Auto-Revert' mode was automatically activated

(defun xem-revisit ()
  "Revist current file."
  (interactive)
  (let ((xem-file (buffer-file-name nil)))   ; this file
    (save-buffer)                            ; emacs will ask if write permissions have been withdrawn
    (kill-buffer)
    (find-file xem-file)
    (message "  xem: revisit complete using '%s'." (abbreviate-file-name xem-file))))

(defun xem-insert-strong-comment ()
  "Insert \"** COMMENT\", suitably aligned."
  (interactive)
  (let* ((prepend  "** "                  )
         (marker   "COMMENT"              )
         (line     (thing-at-point 'line) )
         (len      (string-bytes line)    ))
    (when (> len 1)                          ; contains only a newline if empty
      (end-of-line)
      (insert "\n"))
    (insert "    ")                          ; 4 spaces
    (insert prepend)
    (insert marker)
    (message "  xem: insert string comment complete.")))

;; ---------------------------------
;;  find binary
;; ---------------------------------

(defvar xem-binary   nil "Binary name.")
(defvar xem-have-run nil "XEM file have run flag.")    ; 'nil' = not run, 't' = run
(defvar xem-svn      nil "Binary svn revision.")       ; should be zero or integer

(make-local-variable 'xem-binary)
(make-local-variable 'xem-have-run)
(make-local-variable 'xem-svn)

(defun xem-rebinary (new-binary)
  "Prompted find for new xeona binary NEW-BINARY."
  (interactive "f")
  (let* ((old-binary  xem-binary))
    (setq xem-binary new-binary)
    (setq xem-svn (xem-binary-svn xem-binary))
    (message " xem: rebinary complete using '%s',\n                   previously '%s'."
             xem-binary old-binary) ))

(defun xem-locate-binary (&optional xeona) ; optional binary name
  "Locate the desired XEONA binary somewhat intelligently.

Indeed it can be useful during testing not to place the 'xeona' binary
on the shell search path.  This function returns the fully-qualified name."
  (interactive)
  (let ((binary  "xeona.mach"                         )     ; default binary name
        (paths   (list "." ".." "../.." "../xeona1" ) )     ; preferred search order
        (which   nil                                  )     ; which output
        (call    nil                                  ))    ; return value
    (if xeona (setq binary xeona))           ; overwrite the default as required
    (setq which (shell-command-to-string (concat "which" " " binary)))
    (if (> (length which) 0)
        ;; 'binary' is on the shell search PATH
        (setq call (xem-string-trim which))  ; remove the trailing newline
      ;; else hunt thru the given list of 'paths'
      (setq paths (nreverse paths))          ; work backwards
      (dolist (path paths)
        (setq possible (expand-file-name binary path))
        (if (file-executable-p possible)
            (setq call possible))))
    ;; report
    (if call
        (message "  xem: locate-binary: call '%s'" call)
      (message "  xem-locate-binary: using paths %S" paths)
      (message "  xem: locate-binary: FAIL using binary name '%s'" binary))
    call))                                   ; expose the return value

(defun xem-binary-svn (binary)
  "Obtain svn revision of named BINARY."
  (let ((capture  nil ))                     ; shell command capture
    (if binary
        (progn
          (setq capture (shell-command-to-string (concat binary " --svn" " 2>/dev/null")))
          (setq capture (xem-string-trim capture))
          (message "  xem: xem-binary-svn: capture '%s'" capture)
          (string-to-number capture))
      (message "  xem: xem-binary-svn: binary set to nil")
      nil)))

;; ---------------------------------
;;  create new model
;; ---------------------------------

(defun xem-insert-new-model (&optional xeona)
  "Insert the output from \"--xem comb\" using the XEONA binary, if supplied."
  (interactive)
  (let ((option  "--xem comb" )              ; xeona option
        (skel    ""           ))             ; skeleton XEM
    (if (not xeona)                          ; xeona name not set externally
        (setq  xeona xem-binary))
    (if (file-executable-p xeona)            ; xeona is executable by you
        ;; main code
        (progn
          (if (> (point-max) (point-min))
              (if (y-or-n-p "Buffer not empty, continue? ")
                  ()
                (error "  xem: insert-new-model: abandoning task without action on user request")))
          (setq call (concat xeona " " option " " "2>/dev/null"))
          (setq skel (shell-command-to-string call))
          (beginning-of-line)
          (insert skel)
          ;; completion reporting
          (message "  xem: insert-new-model complete using xeona option '%s'." option))
      (progn
        ;; executable not found reporting
        (message "  xem: insert-new-model FAILURE, xeona executable not found '%s'." xeona)))))

;; ---------------------------------
;;  rule shift
;; ---------------------------------

(defvar xem-rule-indent 2 "Current rule indent value, usually either 1 or 2.")
(make-local-variable 'xem-rule-indent)

(defun xem-toggle-rules ()
  "Toggle rule indentation between 1 and 2 for reasons of visibility."
  (interactive)
  (save-excursion                            ; hold cursor position
    (let ((rule  "----------" ))             ; used to identify rules
      ;; toggle
      (cond ((= xem-rule-indent 1) (setq xem-rule-indent 2))
            ((= xem-rule-indent 2) (setq xem-rule-indent 1))
            (t (message "  xem: toggle-rules: cond fall-thru")))
      ;; action
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at (concat "^[[:blank:]]+" rule))
            (xem-rule xem-rule-indent))      ; the argument is strictly unnecessary
        (forward-line))
      (message "  xem: toggle-rules complete with %d" xem-rule-indent)) ))

;; ---------------------------------
;;  highlight output
;; ---------------------------------

(defvar xem-highlight-output-flag nil
  "Non-nil means `xem-toggle-highlight-output' active.")
(make-local-variable 'xem-highlight-output-flag)

(defun xem-toggle-highlight-output ()
  "Toggle Xem field 'program.r-processing.r-highlight-output' highlighting.

Utilizes `occur'."
  (interactive)
  (if (xem-have-i-run)                       ; 'true' if XEM file has run
      (progn
        (if xem-highlight-output-flag
            ;; non-occur
            (progn
              (kill-buffer "*Occur*")
              (delete-other-windows)
              (setq xem-highlight-output-flag nil)
              (message "  xem: toggle-highlight-output revert complete."))
          ;; occur
          (progn
            (xem-highlight-output)
            (setq xem-highlight-output-flag t)
            (message "  xem: toggle-highlight-output occur complete."))))
    (ding)                                   ; acknowledge visual bell setting
    (message "  xem: toggle-highlight-output: this XEM file not identified as having run.")))

;; ---------------------------------
;;  squish display
;; ---------------------------------

(defvar xem-squish-flag 0
  "Flag for `xem-cycle-squish', 0 = no squish, 2 = maximum squish.")
(make-local-variable 'xem-squish-flag)

(defun xem-cycle-squish ()
  "Cycle thru two levels of selective display.  The cursor color will also change."
  (interactive)
  (let ((level-zero  0 )
        (level-one   6 )
        (level-two   2 ))
    (cond ((= xem-squish-flag 0)
           (xem-selective-set level-one)     ; squish to depth 6
           (setq xem-squish-flag 1)
           (message "  xem: cycle-squish selective display set at indent %d." level-one))
          ((= xem-squish-flag 1)
           (xem-selective-set level-two)     ; squish to depth 2
           (recenter)
           (setq xem-squish-flag 2)
           (message "  xem: cycle-squish selective display set at indent %d." level-two))
          ((= xem-squish-flag 2)
           (xem-selective-set level-zero)             ; unsquish
           (setq xem-squish-flag 0)
           (message "  xem: cycle-squish selective display unset."))
          (t (message "  xem: cycle-squish: problem with xem-squish-flag %d" xem-squish-flag)))))

(defun xem-selective-set (indent)
  "Set `set-selective-display' to INDENT and change cursor color."
  (set-selective-display indent)
  (cond ((= indent 0) (set-cursor-color "black"))
        ((= indent 2) (set-cursor-color "cyan1"))
        ((= indent 6) (set-cursor-color "cyan3")))
  (setq xem-squish-flag t)
  (message "  xem: set-selective un/set using indent %d" indent))

;; ---------------------------------
;;  key/value functions
;; ---------------------------------

(defun xem-highlight-output ()
  "Show occurs for trigger 'r-highlight-output'."
  (let ((trigger "r-highlight-output"))
    (xem-key-occur (xem-value-get trigger))
    (message "  xem: highlight-output complete.")))

(defun xem-have-i-run ()
  "Examine 'used-svn' value and return t if substantial, else nil."
  (let ((trigger  "used-svn" )
        (value               ))
    (setq value (xem-value-get trigger))
    (message "  xem: have-i-run complete, recovered '%s'" value)
    (if (= (length value) 0) nil t)))        ; expose the return value

(defun xem-value-get (key &optional stripquotes)
  "Return value for input or output field key KEY and perhaps STRIPQUOTES.  Else return nil.

This function searches the current buffer for the given KEY.

Note that string values retain their double-quotes."
  (save-excursion                            ; hold cursor position
    (let ((regex  (concat "^[[:blank:]]+" key ".*[<>][[:blank:]]*\\(.*\\)$") )
          (value  nil                                                        ))
      (message "  xem: value-get: using search regex '%s'" regex)
      (goto-char (point-min))
      (if (re-search-forward regex
                             nil t)
          (setq value (match-string 1)))     ; 1 indicates first \( \) construct
      (if stripquotes
          (if (string-match "^\"\\(.*\\)\"$" value)
              (setq value (match-string 1 value))))
      (message "  xem: value-get: complete with key '%s' with recovered value '%s'" key value)
      value)))                               ; expose the return value

(defun xem-key-occur (key)
  "Run `occur' on output fields containing KEY key.

KEY may be double-quoted, in which case the quotes are first removed."
  (save-excursion                            ; hold cursor position
    (let ((regex-1  "^\\\"\\(.*?\\)\\\"$" )   ; un-double-quote regex (note the triple escapes)
          (key-2    nil                   )
          (regex-2  nil                   ))
      (if (not key)
          (error "  xem: key-occur supplied nil key '%s'" key))
      (if (string-match regex-1 key)
          (setq key-2 (match-string 1 key)))   ; 1 indicates first \( \) construct
      (setq regex-2 (concat "^[[:blank:]]+" key-2 ".*<"))
      (message "  xem: key-occur: using occur regex '%s'" regex-2)
      (occur regex-2)
      (message "  xem: key-occur: complete"))))

;; ---------------------------------
;;  field navigation
;; ---------------------------------

(defun xem-is-field (wrangle)
  "Test current line for a particular field type determined by regex WRANGLE.

This function returns either t or nil.  WRANGLE is normally \">\"
for in-data, \"<\" for out-data, or \"<>\" for either."
  (let* ((regex  (concat "[" wrangle "]") )              ; add the regex list operator sytax
         (line   (thing-at-point 'line) ))
    (message "  xem: is-field: regex '%s'" regex)
    (if (and (not (string-match "<.*>\\|>.*<" line))     ; email addresses and similar are excluded
             (string-match regex line))
        t
      nil) ))

(defun xem-move-field-next ()
  "Move to next field."
  (interactive)
  (while (and (forward-line +1)
              (not (xem-is-field "<>"))
              (not (eobp))) )
  (end-of-line)
  (message "  xem: move next field: complete"))

(defun xem-move-field-prior ()
  "Move to previous field."
  (interactive)
  (while (and (forward-line -1)
              (not (xem-is-field "<>"))
              (not (bobp))) )
  (end-of-line)
  (message "  xem: move prior field: complete"))

(defun xem-move-field-in-next ()
  "Move to next in-data field."
  (interactive)
  (while (and (forward-line +1)
              (not (xem-is-field ">"))
              (not (eobp))) )
  (end-of-line)
  (message "  xem: move next in-data field: complete"))

(defun xem-move-field-in-prior ()
  "Move to previous in-data field."
  (interactive)
  (while (and (forward-line -1)
              (not (xem-is-field ">"))
              (not (bobp))) )
  (end-of-line)
  (message "  xem: move prior in-data field: complete"))

(defun xem-move-field-out-next ()
  "Move to next out-daat field."
  (interactive)
  (while (and (forward-line +1)
              (not (xem-is-field "<"))
              (not (eobp))) )
  (end-of-line)
  (message "  xem: move next out-data field: complete"))

(defun xem-move-field-out-prior ()
  "Move to previous out-data field."
  (interactive)
  (while (and (forward-line -1)
              (not (xem-is-field "<"))
              (not (bobp))) )
  (end-of-line)
  (message "  xem: move prior out-data field: complete"))

;; ---------------------------------
;;  entity navigation
;; ---------------------------------

(defconst xem-entity-move-regex (concat "^entity\\.\\|^" xem-dchar "[[:blank:]]*entity\\."))
(defconst xem-recenter 1) ; useful values: 0, 1, nil (to center)

(defun xem-move-entity-next ()
  "Navigate to next entity record."
  (interactive)
  (let ((regex  xem-entity-move-regex ))
    (if (looking-at regex)
        (forward-char 1))                    ; need to "kick-start" the search
    (if (re-search-forward regex
                           nil t)            ; limit (no), no error (true)
        (progn
          (beginning-of-line)
          (if (= xem-squish-flag 2)          ; squish-level sensitive 'recenter'
              () ;(recenter nil)
            (recenter xem-recenter))
          (message "  xem: move to next entity complete."))
      (beginning-of-line)
      (message "  xem: move to next entity FAILED."))
    (match-beginning 0) ))                   ; nil if no match

(defun xem-move-entity-prior ()
  "Navigate to prior (meaning previous) entity record."
  (interactive)
  (let ((regex  xem-entity-move-regex ))
    (if (re-search-backward regex
                            nil t)           ; limit (no), no error (true)
        (progn
          (beginning-of-line)                ; superfluous in this case
          (if (= xem-squish-flag 2)          ; squish-level sensitive 'recenter'
              () ; (recenter nil)
            (recenter xem-recenter))
          (message "  xem: move to prior entity complete."))
      (beginning-of-line)
      (message "  xem: move to prior entity FAILED."))
    (match-beginning 0) ))                   ; nil if no match

;; ---------------------------------
;;  hash navigation
;; ---------------------------------

(defconst xem-hash-move-regex
  (concat "^" xem-dchar "[[:blank:]][[:alnum:]]"                 ; records
          "\\|"
          "^[[:blank:]]*" xem-dchar "[[:blank:]][[:alnum:]]") ; fields
  "Regex for locating disabled entities and fields.")

(defconst xem-hash-recenter
  nil
  "Controls search centering behavior, useful values: 0, 1, nil (to center).")

(defun xem-move-hash-ring ()
  "Cycle to next disabled field, returning to the top when necessary."
  (interactive)
  (let ((here  (point) ))
    (if (xem-move-hash-next)
        ()
      (beginning-of-buffer)
      (if (xem-move-hash-next)
          ()
        (goto-char here)))))

(defun xem-move-hash-next ()
  "Navigate to next disabled field."
  (interactive)
  (let ((regex  xem-hash-move-regex ))
    (if (looking-at regex)
        (forward-char 1))                    ; need to "kick-start" the search
    (if (re-search-forward regex
                           nil t)            ; limit (no), no error (true)
        (progn
          (beginning-of-line)
          (recenter xem-hash-recenter)
          (message "  xem: move to next hash complete.")
          t)
      (beginning-of-line)
      (message "  xem: move to next hash FAILED.")
      nil)))

(defun xem-move-hash-prior ()
  "Navigate to prior (meaning previous) disabled field."
  (interactive)
  (let ((regex  xem-hash-move-regex ))
    (if (re-search-backward regex
                            nil t)           ; limit (no), no error (true)
        (progn
          (beginning-of-line)                ; superfluous in this case
          (recenter xem-hash-recenter)
          (message "  xem: move to prior hash complete."))
      (beginning-of-line)
      (message "  xem: move to prior hash FAILED."))))

;; ---------------------------------
;;  narrowing
;; ---------------------------------

(defvar xem-narrow-flag 0
  "Flag for `xem-toggle-narrow', 0 = wide, 1 = narrow.")
(make-local-variable 'xem-narrow-flag)

(defun xem-toggle-narrow ()
  "Toggle between `xem-narrow-to-entity' and `xem-widen'."
  (interactive)
  (cond ((= xem-narrow-flag 0)
         (xem-narrow-to-entity))
        ((= xem-narrow-flag 1)
         (xem-widen))))

;; assumes (put 'narrow-to-region 'disabled nil)

(defconst xem-note-move-regex "^note$")           ; notes cannot be disabled
(defconst xem-rule-regex "^[[:blank:]]+------")   ; can add second blank for a more restricted view

(defun xem-widen ()
  "Widen buffer."
  (interactive)
  (save-excursion
    (widen)
    (recenter xem-recenter)
    (customize-set-variable 'cursor-type 'box)    ; default cursor
    (setq xem-narrow-flag 0)
    (message "  xem: widen complete.") ))

(defun xem-narrow-to-entity ()
  "Narrow to current entity.  Use \\[widen] to widen.

Requires the cursor be at the start of the entity in question.

Will need the user to set (put 'narrow-to-region 'disabled nil)."
  (interactive)
  (save-excursion
    (let ((regex  xem-note-move-regex )
          (beg    (point-min)         )
          (end    (point-max)         )
          (tmp    (point-max)         ))
      (if (looking-at xem-entity-move-regex)
          (progn
            (setq beg (point))
            (xem-move-entity-next)
            (setq end (point))
            ;; deal with last entity
            (if (= beg end)
                (if (re-search-forward regex
                                       nil t)
                    (setq end (match-beginning 0))))
            ;; deal with rules
            (goto-char beg)
            (if (re-search-forward xem-rule-regex
                                   nil t)
                (setq tmp (match-beginning 0)))
            (if (< tmp end)
                (setq end tmp))
            (if (not (= beg end))            ; protect against second narrowing
                (progn
                  (narrow-to-region beg end) ; opposite is (widen)
                  (setq xem-narrow-flag 1)
                  (customize-set-variable 'cursor-type 'hollow)
                  (customize-set-variable 'cursor-type 'bar)
                  (message "  xem: narrow to entity complete, %d to %d." beg end))
              (message "  xem: narrow to entity already applied.")))
        (message "  xem: narrow to entity requires cursor be positioned at start of target entity."))
      )))

;; ---------------------------------
;;  colorizing
;; ---------------------------------

;; Xll colors  : /etc/X11/rgb.txt
;; show colors : file:///home/robbie/synk/xeona/x11-colors/x11-color-names.html
;; package     : /usr/share/emacs/21.4/lisp/hi-lock.el

;; interesting: aquamarine gold salmon yellow lemon_chiffon

;; complain if not Xll
(if (eq window-system 'x)                    ; nil = ordinary terminal, 'w32 = MSWin
    (message "  xem: X11 found.")
  (error "  xem: WARNING: X11 assumed but not found"))

;; add support package
(require 'hi-lock)

;; define colors
(defface xem-hi-color-B01
  '((((background dark)) (:background "wheat" :foreground "black"))
    (t (:background "wheat")))              ; this entry is normally used
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-B02
  '((((background dark)) (:background "gold" :foreground "black"))
    (t (:background "gold")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-B03
  '((((background dark)) (:background "wheat" :foreground "black"))
    (t (:background "wheat")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-B04
  '((((background dark)) (:background "salmon" :foreground "black"))
    (t (:background "salmon")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-B05
  '((((background dark)) (:background "gold" :foreground "black"))
    (t (:background "gold")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-B06
  '((((background dark)) (:background "burlywood" :foreground "black"))
    (t (:background "burlywood")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-B07
  '((((background dark)) (:background "beige" :foreground "black"))
    (t (:background "beige")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-B08
  '((((background dark)) (:background "sandy brown" :foreground "black"))
    (t (:background "sandy brown")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-F01
  '((t (:weight normal :foreground "steel blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-F02
  '((t (:weight normal :foreground "deep pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-F03
  '((t (:weight normal :foreground "peru")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-F04
  '((t (:weight normal :foreground "tomato")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-F05
  '((t (:weight normal :foreground "tan")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-F06
  '((t (:weight normal :foreground "black")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-F07
  '((t (:weight normal :foreground "peru")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xem-hi-color-F08
  '((t (:weight bold :foreground "black")))  ; was 'sienna'
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

;; define colorizing regexes
(defconst xem-entity-tmp-regex  "^entity\\..*-0$")
(defconst xem-entity-regex      "^entity\\.")
(defconst xem-entity-dis-regex  (concat "^" xem-dchar "[[:blank:]]*entity\\."))
(defconst xem-record-regex      "^program\\.\\|^model-end$")
(defconst xem-note-regex        "^note")
(defconst xem-class-regex       "class[[:blank:]]+>.*$")
(defconst xem-strong-regex      "^    [[:blank:]]*\\*\\*+[[:blank:]][[:print:]]+$")
(defconst xem-field-dis-regex   (concat xem-dchar "[[:blank:]][[:graph:]][[:print:]]+$"))
(defconst xem-string-regex      "\".*?\"")
(defconst xem-comment-regex     "^      [[:blank:]]*[[:graph:]]")
(defconst xem-header-regex      "header: [[:graph:]]+")
(defconst xem-domain-regex      " DOMAIN [[:alnum:]] ")     ; upper case not totally respected, was [A-Z]
(defconst xem-common-regex      " COMMON [0-9]+ ")          ; upper case not totally respected

;; define overarching function
(defun xem-colorize ()
  "Colorize a XEM file."
  (interactive)
  ;; CAUTION: order is important
  (highlight-lines-matching-regexp xem-entity-tmp-regex 'xem-hi-color-B08)
  (highlight-lines-matching-regexp xem-entity-regex     'xem-hi-color-B07)
  (highlight-lines-matching-regexp xem-entity-dis-regex 'xem-hi-color-B04)
  (highlight-lines-matching-regexp xem-record-regex     'xem-hi-color-B01)
  (highlight-lines-matching-regexp xem-note-regex       'xem-hi-color-B06)
  (highlight-regexp                xem-class-regex      'xem-hi-color-B02)
  (highlight-regexp                xem-strong-regex     'xem-hi-color-F02)
  (highlight-regexp                xem-field-dis-regex  'xem-hi-color-F03)
  (highlight-regexp                xem-string-regex     'xem-hi-color-F04)
  (highlight-lines-matching-regexp xem-comment-regex    'xem-hi-color-F05)
  (highlight-regexp                xem-header-regex     'xem-hi-color-F07)
  (highlight-regexp                xem-domain-regex     'xem-hi-color-F08)
  (highlight-regexp                xem-common-regex     'xem-hi-color-F08)
  (message "  xem: colorize: colorizing complete"))

;; show colors : firefox file:///home/robbie/synk/xeona/x11-colors/x11-color-names.html
;; show colors : `list-colors-display'

;; ---------------------------------
;;  toggle data
;; ---------------------------------

;; make records and fields active and inactive

(defun xem-toggle-data ()
  "Toggle enable on records and fields.

Caution: there needs to be one uppermost ACTIVE field."
  (interactive)
  (let ((dchar  xem-dchar )
        (line   nil       ))
    (setq line (thing-at-point 'line))
    (message "  xem: toggle-data: line '%s'" line)

    (defun replace (nline)                             ; assumes 'nline' has trailing newline
      (beginning-of-line)
      (kill-line nil)
      (insert nline))

    (cond                                              ; elisp case statement
     ((string-match "^program\\.[[:alnum:]]"   line)   ; record identifier "entity.[a-z..].."
      (message "  xem: toggle-data record complete (no action taken)."))
     ((string-match "^entity\\.[[:alnum:]]"    line)
      (replace (concat dchar " " line))
      (forward-line -1)
      (message "  xem: toggle-data record complete."))
     ((string-match (concat dchar "[[:blank:]]*entity\\.[[:alnum:]]")  line)
      (replace (substring line 2 nil))                 ; toggle disabled, now explicit leading "# "
      (forward-line -1)
      (message "  xem: toggle-data record complete."))
     ((string-match "^[[:blank:]]*[[:alnum:]]" line)
      ;; could shift some code to 'xem-activate-field'
      (end-of-line)
      (insert "\n")
      (replace (xem-toggle-disable line dchar))
      (forward-line -1)
      (message "  xem: toggle-data disable field complete."))
     (t
      (xem-activate-field)) )))                        ; toggle disabled

(defun xem-activate-field ()
  "Support for `xem-toggle-data'."
  (let ((dchar  xem-dchar )                  ; xem disable character (assumed length one)
        (king   nil       )                  ; xem field line to be promoted
        (count  0         ))                 ; shuffle count
    ;; local function
    (defun replace (nline)
      (beginning-of-line)
      (kill-line nil)
      (insert nline))
    ;; main code
    (if (xem-is-enabled (thing-at-point 'line) dchar)
        (message "  xem: activate-field: nothing to do.")  ; final message
      ; else block
      (setq king (thing-at-point 'line))
      (setq king (xem-toggle-disable king dchar))
      (replace "")                           ; effectively kill entire line
      (forward-line -1)                      ; move up
      (setq count 1)
      ;; loop
      (while (not (xem-is-enabled (thing-at-point 'line) dchar))
        (forward-line -1)
        (incf count))
      ;; completion
      (replace (xem-toggle-disable (thing-at-point 'line) dchar))
      (forward-line -1)
      (insert king)
      (message "  xem: activate-field complete with %d shuffles up." count)))) ; final message

(defun xem-toggle-disable (line dchar)
  "For supplied field LINE string, toggle the disable DCHAR."
  ;; assumes "  # " style alignment
  (if (xem-is-enabled line dchar)
      (concat "  "  dchar  " " (substring line 4 nil))
    (concat "    " (substring line 4 nil))))

(defun xem-is-enabled (line dchar)
  "Report as to whether supplied field LINE string is DCHAR disabled."
  (let ((first (car (split-string line))))   ; use default separator "[ \f\t\n\r\v]+"
    (not (equal dchar first))))              ; strings compared element by element

;; ---------------------------------
;;  occur calls
;; ---------------------------------

(defun xem-occur-builtin-remarks ()
  "List builtin-remark lines."
  (interactive)
  (let ((regex  "builtin-remark" ))
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xem: occur-builtin-remarks complete, regex '%s'." regex)))

(defun xem-occur-entities ()
  "List entity lines."
  (interactive)
  (let ((regex  "^entity\\." ))
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xem: occur-entities complete, regex '%s'." regex)))

(defun xem-occur-entities-disabled ()
  "List disabled entities."
  (interactive)
  (let ((regex  (concat "^" xem-dchar) ))
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xem: occur-entities-disabled complete, regex '%s'." regex)))

(defun xem-occur-domcons ()
  "List domain controller entities."
  (interactive)
  (let ((regex "^entity\\.domain-controller-" ))
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xem: occur-entities-domcons complete, regex '%s'." regex)))

(defun xem-occur-classes ()
  "List class lines."
  (interactive)
  (let ((regex  "class[[:space:]]*>" ))
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xem: occur-classes complete, regex '%s'." regex)))

(defun xem-occur-rules ()
  "List rules."
  (interactive)
  (let ((regex  "^[[:blank:]]+---+ [[:alnum:] ]+$" ))  ; at least three dashes are required "---"
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xem: occur-rules complete, regex '%s'." regex)))

(defun xem-occur-hashes ()
  "List disabled field lines."
  (interactive)
  (let ((regex  (concat "^[[:blank:]]+" xem-dchar) ))
    (occur regex 2)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xem: occur-hashes complete, regex '%s'." regex)))

(defun xem-occur-identities ()
  "Show {actor,asset,block,commodity,..} identity lines."
  (interactive)
  (let ((enlist   nil )                      ; user-defined list of entities phrases
        (entity   nil )
        (entitys  nil )                      ; 'enlist' in \\|-separated form
        (iflist   nil )                      ; user-defined list of interface phrases
        (iface    nil )
        (ifaces   nil )
        (regex    nil ))                     ; 'occur' regex
    ;; add to these as required
    (setq enlist "actor asset block commodity context domain junction operator selgate")
    (setq enlist (concat enlist " " "[1-9]")); for "socket-1" and so on
    (setq iflist "[[:alnum:]-]+")            ; earlier also "cable socket"
    ;; active code
    (dolist (entity (split-string enlist))   ; use default separator "[ \f\t\n\r\v]+"
      (setq entitys (concat entitys entity "\\|")))
    (dolist (iface (split-string iflist))    ; use default separator "[ \f\t\n\r\v]+"
      (setq ifaces (concat ifaces iface "\\|")))
    (setq entitys (substring entitys 0 -2))  ; trim the final "\\|", -2 is correct
    (setq ifaces  (substring ifaces  0 -2))  ; trim the final "\\|", -2 is correct
    (setq regex (concat "\\(" ifaces "\\)-\\(" entitys "\\)s?[[:space:]]+[lL]"))
    (occur regex)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xem: occur-identities complete, regex '%s'." regex)))

(defun xem-occur-asterisks ()
  "List \"** \" comments."
  (interactive)
  (let ((regex "^[[:blank:]]+\\*\\* " ))
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xem: occur-hashes complete, regex '%s'." regex)))

;; ---------------------------------
;;  insert entity calls
;; ---------------------------------

(defun xem-insert-entity-regex (&optional xeona)
  "Attempt to insert sought XEDOCs into current buffer, optionally using XEONA.

The user will be prompted for a class name regex."
  (interactive)
  (let ((option  "--class" )                 ; xeona option
        (class   ""        )                 ; entity class name
        (xedoc   ""        ))                ; sought XEDOC or XEDOCs
    (if (not xeona)                          ; xeona name not set externally
        (setq  xeona xem-binary))

    (if (file-executable-p xeona)            ; xeona is executable by you
        ;; main code
        (progn
          (setq class
                (read-string
                 "Enter case-sensitive class name regex [default .]: "     ; prompt
                 nil                                                       ; initial
                 nil                                                       ; history list
                 "."))                                                     ; set default

          (setq call (concat xeona " " option " " class " " "2>/dev/null"))
          (setq xedoc (shell-command-to-string call))

          (if (not (looking-at "^")) (progn (end-of-line) (insert "\n")))
          (insert xedoc)
          ;; completion reporting
          (message "  xem: insert-entity-regex complete using class regex '%s'." class))

      (progn
        ;; executable not found reporting
        (message "  xem: insert-entity-regex FAILURE, xeona executable not found '%s'." xeona)))))

(defun xem-insert-entity-scroll ()
  "Select an entity from a refreshed list of entities."
  (interactive)
  (let ((binary   xem-binary                                                 )
        (args1   "--class + 2>/dev/null | gawk '/[[:alpha:]]/ { print $1 }'" )   ; [1]
        (args2   "--class"                                                   )
        (args3   "2>/dev/null"                                               )
        (call                                                                )
        (entity-list                                                         )
        (class                                                               ))
    ;; [1] the "/[[:alpha:]]/" is to screen out blank lines
    ;; check for 'gawk' utility
    (if (equal "" (shell-command-to-string "which gawk"))
        (message "  xem: insert-entity-scroll: 'gawk' utility not present on system"))
    ;; active code
    (if (file-executable-p binary)           ; 'xeona' is executable by you
        ;; executable found
        (progn
          (setq call (concat binary " " args1))
          (setq entities (shell-command-to-string call))
          (setq entity-list (split-string entities "\n"))
          (message "  xem: insert-entity-scroll: entity list length %d" (length entity-list))
          (setq class
                (read-string
                 "Scroll and enter class name: "  ; prompt
                 (pop entity-list)                ; initial
                 'entity-list                     ; history list
                 nil))                            ; set default

          (setq call (concat binary " " args2 " " class " " args3))
          (setq xedoc (shell-command-to-string call))

          (when (not (looking-at "^")) (end-of-line) (insert "\n"))
          (insert xedoc)
          ;; completion reporting
          (message "  xem: insert-entity-scroll complete using class regex '%s'." class))
      (progn
        ;; executable not found reporting
        (message "  xem: insert-entity-scroll FAILURE, xeona executable not found '%s'." binary)))))

;; ---------------------------------
;;  file alignment calls
;; ---------------------------------

(defun xem-reindent ()                       ; quite well coded, see RCS:1.538 for different 'fmtstr'
  "Reset a XEM file based on prevailing 'tab-stop-list' setting."
  (interactive)
  (save-excursion
    (let ((prior     nil        )  ; prior 'kill-whole-line' setting
          (indent-1  4          )  ; desired indent from existing 'tab-stop-list', else accept this
          (indent-2  55         )  ; desired indent from existing 'tab-stop-list', else accept this
          (fmtstr    nil        )  ; reformat string for 'format' call
          (tags      '(">" "<") )  ; indicating input and output data lines
          (line      nil        )  ; prevailing line
          (chunks    nil        )  ; split line
          (count     0          )) ; processed lines count
      ;; preamble
      (setq prior kill-whole-line)           ; keep for later
      (setq kill-whole-line nil)
      ;; set indents
      (goto-char (point-min))                     ; park point at beginning of buffer
      (if (search-forward "tab-stop-list" nil t)  ; 'nil' to end of buffer, 't' means no error on fail
          (progn
            (setq indent-1 (nth 0 tab-stop-list))      ; nth 0 is the first element!
            (setq indent-2 (nth 1 tab-stop-list))))    ; nth 1 is the second element!
      ;; main code
      (goto-char (point-min))                ; park point at beginning of buffer
      (while (not (eobp))
        (setq line (thing-at-point 'line))   ; contains trailing newline
        (dolist (tag tags)                   ; note shortened form with default return
          (when (string-match tag line)
            (setq chunks (split-string line tag))  ; returns list, also modifies the match data
            (setq fmtstr (format "%%-%ds %%s %%s" (1- indent-2)))  ; minus one for padding
            (message "  xem: fmtstr '%s'" fmtstr)
            (insert
             (format fmtstr (robbie-rdeblank (pop chunks)) tag (robbie-ldeblank(pop chunks))))
            (kill-line nil)              ; leave terminating newline if 'kill-whole-line' is 'nil'
            (delete-char -1)             ; back up too!
            (incf count) ))
        (forward-line +1) )                  ; creep forward
      ;; housekeeping
      (setq kill-whole-line prior)           ; reinstate previous setting
      (delete-trailing-whitespace)           ; remove trailing whitespace / can be commented out
      (message "  xem: reindent complete with %d lines processed using %d indent." count indent-2) ))
  (beginning-of-line))                       ; outside 'save-excursion' block

;; the following 'xem-reset-tabstops' is now mostly redundant
;; because 'xeona' automatically does this from r2299

(defun xem-reset-tabstops ()
  "Reset the prevailing 'tab-stop-list' based on current indentation.\n
This call is mostly redundant because 'xeona' does this automatically
from commit r2299 onwards."
  (interactive)
  (save-excursion
    (let ((lines    0           )            ; number of lines
          (tab      0           )            ; current second tab setting
          (index    0           )            ; angle index
          (line     ""          )            ; captured line
          (lo       0           )            ; lowest angle index
          (hi       0           )            ; highest angle index
          (tabstops ""          )            ; something like "(04 45 47)"
          (singles  0           )            ; number of single angles { < > }
          (doubles  0           )            ; number of multiple angles, such as email addresses
          (indexes  ()          )            ; empty list
          (change   "no change" ))           ; completion reporting
      ;; get current tab value
      (setq tab (nth 1 tab-stop-list))
      (message "  xem: reset-tabstops: tab %d" tab)
      ;; loop thru lines
      (goto-char (point-min))                ; park point at beginning of buffer
      (while (not (eobp))
        (setq line (thing-at-point 'line))   ; contains trailing newline
        ;; examine line
        (if (and (string-match "<" line)
                 (string-match ">" line))    ; exclude "< >" style constructs
            (progn
              (incf doubles))
          (progn
            (if (or (string-match "<" line)  ; output mark
                    (string-match ">" line)) ; input mark
                (progn
                  (incf singles)
                  (setq index (1- (match-end 0)))
                  (push index indexes) ))))
        ;; creep forward
        (incf lines)
        (forward-line +1) )
      ;; modify tab-stop-list given indexes are consistent
      (when indexes
        (setq indexes (sort indexes '<)) ; CAUTION: must reassign 'indexes'
        (setq lo (nth 0 indexes))
        (setq hi (car (last indexes)))
        (message "  xem: reset-tabstops: lo %d hi %d" lo hi)
;;;     (if (and (= lo hi) (not (= lo tab)))       ; old code
        (if (or (not (= lo hi)) (not (= lo tab)))  ; new code
            (progn
              (goto-char (point-min))
              (setq occurrence (re-search-forward "tab-stop-list: " nil t))
              (if occurrence
                  (progn
                    (setq change "modified")
                    (goto-char occurrence)
                    (kill-line)          ; delete to end of line
;;;                 (setq tabstops (format "(%02d %02d %02d)" 4 lo (+ lo 2)))    ; old code
                    (setq tabstops (format "(%02d %02d %02d)" 4 hi (+ hi 2)))    ; new code
                    (insert tabstops) ))))
        (setq tabstops "<no change, nothing written>"))     ; never 'inserted'
      ;; completion reporting
      (setq msg1 (format "reset-tabstops complete: %s." change))
      (setq msg2 (format "hits: %d lines, %d singles, %d doubles (email)." lines singles doubles))
      (setq msg3 (format "second tab: old = %d, lo = %d, hi = %d / tabstops = %s." tab lo hi tabstops))
      (message "  xem: %s\n%s\n%s" msg1 msg2 msg3) )))

;; ---------------------------------
;;  rule calls
;; ---------------------------------

(defun xem-rule (&optional indent)
  "Create standard separator rules using optional INDENT.

The separator rules take the form:

  --------------------------------------- mandatory entities

If necessary, the user will be prompted for text."
  (interactive)
  (let ((default-indent xem-rule-indent )    ; indent
        (overall        60              )    ; final line length
        (line           nil             )
        (words          nil             )
        (text           nil             )
        (len            0               ))
    (unless indent (setq indent default-indent))
    ;; function 'split-string': default 'SEPARATOR' is
    ;; "[ \f\t\n\r\v]+", empty matches do count when not
    ;; adjacent to another match
    (setq line (thing-at-point 'line))       ; contains trailing newline
    (setq words (split-string line))
    ;; establish 'text'
    (if (equal words nil)                    ; 'nil' if only non-words (meaning spaces)
        (progn
          (setq text
                (read-string
                 "Enter text [default 'xxx']: "   ; prompt
                 nil                              ; initial
                 nil                              ; history list
                 "xxx")))                         ; set default
      (progn
        (if (string-match "-+" (car words))       ; '+' is match-one-or-more operator
            (pop words))
        (while words
          (setq text (concat text (pop words) " ")))
        (setq text (substring text 0 -1)) ))  ; remove final blank
    ;; remake line
    (setq len (- overall (+ indent (length text) 1)))
    (setq output (concat (make-string indent 32) (make-string len ?-) " " text))
    ;; replace current line
    (beginning-of-line)
    (kill-line)
    (insert output)
    (insert "\n")
    (forward-char -1)                        ; realign point
    ;; completion reporting
    (message "  xem: rule complete using text '%s'." text)))

;; ---------------------------------
;;  scan-builtin-remark
;; ---------------------------------

(defun xem-extract-data (line                ; input (no newline)
                         key                 ; sought key
                         splif)              ; split string regex, usually " < " or " > "
  "Extract field data from the given LINE, KEY, and SPLIF regex.

The SPLIF regex is usually, but not limited to, one of \"<\" \">\" \"[<>]\".
The call returns nil if the sought KEY and located key differ."
  (let ((data      nil )                      ; return value
        (elements  nil )
        (first     nil )
        (second    nil )
        (temp      nil )
        (key2      nil ))
    (setq elements (split-string line splif))
    (when (< 0 (length elements))
      (setq first  (xem-string-trim (pop elements)))
      (setq second (xem-string-trim (pop elements)))
      (setq temp   (split-string first))
      (setq key2   (xem-string-trim (pop temp)))
      (if (string-match key key2)
          (setq data second)))
    (if data (message "  xem: extract-data : %s %-30s %s" splif key data))
    data ))                                  ; expose the return value

(defun xem-unique (list)
  "Remove adjacent duplicate elements from the supplied LIST using `equal'.

It is recommended the list be sorted first, for instance:

    (setq results (sort results 'string<))  ; sort lexicographically"
  (let ((delay       "" )
        (element        )
        (duplicates  0  )
        (output         ))
    (if (not (listp list))                   ; list, includes 'nil'
        (message " xem unique  : list failed predicate test")
      (dolist (element list)                 ; else
        (if (not (equal element delay))
            (push element output)
          (setq duplicates (+ 1 duplicates)))
        (setq delay element))
      (setq output (nreverse output))        ; reverse the order (can also use 'reverse')
      (message "  xem: unique  : complete, excluded duplicates %d" duplicates)
      output)))                              ; expose the return value

(defun xem-scan-class-buffer ()
  "Scan buffer and extract and print sorted 'class' and 'builtin-remark' data."
  (interactive)
  (let ((lines   0                )          ; number of lines processed
        (buffer  (current-buffer) ))         ; current buffer object
    (setq lines (xem-scan-class-region (point-min) (point-max)))
    (split-window-vertically)
    (switch-to-buffer buffer)
    (message "  xem: scan-class-buffer complete, %d lines processed." lines)))

(defun xem-scan-class-region (reg-start reg-end)
  "Scan REG-START and REG-END region and extract and print sorted 'class' and 'builtin-remark' data."
  (interactive "r")
  (save-excursion
    (let ((buffer-name  "*XEM-builtin-remarks*" )      ; results buffer
          (result       nil                     )      ; formatted result
          (results      nil                     )      ; vector of 'result'
          (linebuf      nil                     )      ; line buffer
          (class        nil                     )      ; class data
          (remark       nil                     )      ; builtin-remark data
          (lines        0                       ))     ; lines processed
      ;; grab data
      (goto-char reg-start)                       ; goto start region
      (while (< (point) reg-end)                  ; cycle thru lines in the region
        (setq linebuf (thing-at-point 'line))     ; contains trailing newline
        (if (string-match "\n$" linebuf)          ; remove trailing newline
            (setq linebuf (substring linebuf 0 (match-beginning 0)))) ; zero is string beginning
        (if (not class)  (setq class  (xem-extract-data linebuf "class"          " > ")))
        (if (not remark) (setq remark (xem-extract-data linebuf "builtin-remark" " < ")))
        (while remark
          (while (string-match "\"" remark)   ; CAUTION: 'while' not 'if' necessary
            (setq remark (replace-match "" t t remark)))
          (if (string-match "^$" remark) (setq remark "(not set)"))
          (push (format "    %-25s %s" class remark) results)
          (setq class  nil)
          (setq remark nil))
        (setq lines (+ lines 1))
        (forward-line 1))
      ;; sort output
      (setq results (sort results 'string<))      ; sort lexicographically
      (setq results (xem-unique results))         ; local function
      ;; (setq results (nreverse results))         ; reverse the order (can also use 'reverse')
      ;; new buffer
      (switch-to-buffer buffer-name)
      (text-mode)
      (setq truncate-lines t)
      ;; insert results
      (insert (format "unique sorted classes: %d\n" (length results)))
      (dolist (result results)                    ; cycle thru lines, non-intrusive
        (insert result)
        (insert "\n"))
      ;; housekeeping
      (goto-char (point-min))                     ; go back to the beginning
      (message "  xem: scan-class-region complete, %d lines processed." lines)
      lines)))                                    ; expose the return value

;; ---------------------------------
;;  insert rule
;; ---------------------------------

(defun xem-insert-rule-scroll ()
  "Insert rule using standard list or by direct entry."
  (interactive)
  (let ((rule-string)
        (rules (list "program admin"
                     "mandatory entities"
                     "domain 1"
                     "operators"
                     "entities"
                     "nodes"
                     "commodities"
                     "contexts"
                     "tail")))
    ;; query user
    (setq rule-string
          (read-string
           "Scan rule strings: "             ; prompt
;;;        (pop rules)                       ; initial
           ""                                ; initial
           'rules                            ; history list
           ""))                              ; set default
    ;; insert rule
    (while (not (looking-at "^$")) (end-of-line) (insert "\n"))
    (insert rule-string)
    (insert "\n")
    (forward-line -1)
    (xem-rule)                        ; process rule
    (message "  xem: insert-rule complete using '%s'." rule-string)))

;; ---------------------------------
;;  current entity identifier
;; ---------------------------------

(defconst xem-get-current-record-kinds
  (list "note" "program\.*" "entity\.*" "model-end")
  "List of 'xeona' record kind regexes.")

(defun xem-get-current-record-id ()
  "Locate and return the identifier for the current record.

Return 'nil' otherwise.  A well-formed XEM file is presumed."
  (interactive)
  (save-excursion
    (let* ((kinds  xem-get-current-record-kinds )
           (buffer ()                           )
           (regex  nil                          )
           (id     nil                          ))  ; sought identifier
      ;; initial reporting
      (message "  xem-get-current-record-id: commencing with kinds %s" kinds)
      ;; create 'regex'
      (dolist (kind kinds)
        (push (concat "^" kind "$") buffer))
      (setq regex (mapconcat 'identity (nreverse buffer) "\\|"))      ; stringify [1]
      ;; [1] 'identity' is built-in function and 'mapconcat' returns an empty string if 'buffer' is '()'
      (message "  xem-get-current-record-id: created regex '%s'" regex)
      ;; undertake search
      (if (re-search-backward regex (point-min) t)
          (progn
            (setq id (match-string 0))
            (setq id (substring-no-properties id))     ; remove text properties including coloration
            (message "  xem: get-current-record-id: complete with '%s'" id))
        (message "  xem: get-current-record-id: search failed"))
      id )))                                 ; expose return value

;; ---------------------------------
;;  provide
;; ---------------------------------

;; put the mode symbol into the list "features", so that users can
;; invoke (require 'xem-mode) and load your code only when needed

(provide 'xem-mode)

;;; xem.el ends here

;  $Id: xem.el 9269 2012-12-10 13:45:38Z robbie $
;  end of file

