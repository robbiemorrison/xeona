;;; xog.el --- display support for captured xeona terminal output

;  file-purpose     : emacs display support for captured xeona terminal output
;  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
;  file-create-date : Thu 12-Aug-2010 22:07 UTC
;  file-status      : working
;  file-keywords    : emacs xeona

;  $Revision: 9029 $
;  $Date: 2012-02-14 14:21:09 +0100 (Tue, 14 Feb 2012) $
;  $Author: robbie $
;  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/elisp/xog.el $

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
;;      * check current buffer logic (although no problems have
;;        been detected), while noting the warning in section 27.2
;;        of the elisp manual
;;
;;      * add `abbreviate-file-name' to messages where appropriate
;;
;;      * run `checkdoc' periodically
;;
;;  Known issues
;;

;;; History:

;;      * see the subversion log messages

;;; Code:

;; ---------------------------------
;;  user-modifiable global constants
;; ---------------------------------

(defconst xog-workingcopy
  (file-truename (substitute-in-file-name "$HOME/$SYNK/xeona/svn2/futz/"))
  "Subversion working copy for 'xeona'.")

(defconst xog-sourcecode
  (file-truename (concat xog-workingcopy "trunk/xeona1/"))
  "Source code root for 'xeona'.")

(defconst xog-extension
  ".xog"
  "Xeona terminal log file extension (NOT set in 'common.cc').")

(defconst xog-recenter nil                   ; useful values: 0, 1, nil (to center)
  "Define recenter after search behavior.")

(defconst xog-file-stem
  xeona-file-stem
  "Current directory.")

(defconst xog-xumber-file
  (concat xog-file-stem "xumber.el")
  "Xumber mode file to drag in.")

;; ---------------------------------
;;  requirements
;; ---------------------------------

(require 'hi-lock)                      ; "interactive automatic highlighting"
(require 'highlight-current-line)       ; "highlight line where the cursor is"

;; note the various packages which support current line highlighting

;;; (load-library "/home/robbie/synk/xeona/svn2/futz/trunk/elisp/highline.el")
;;; (require 'highline)       ; "highlight current line in buffer" (non-std)
;;; (require 'hl-line)        ; "highlight the current line"

;; ---------------------------------
;;  mode function
;; ---------------------------------

(defun xog-mode ()
  "Major mode for displaying captured xeona terminal output.

All interactive functions start \"xog-\" for easy identification.

If the menu bar is hidden, a background Xog menu can be brought
up with: \"Ctrl-Shift-leftmouse\".

Dedicated key binding are indicated in the Xog menu.  For a
complete listing, use \\[describe-bindings]."
  (interactive)

  (message "  xog: xog-mode setup commencing")

  ;; recommended
  (kill-all-local-variables)

  ;; emacs stuff
  (setq major-mode 'xog-mode)           ; used by C-h m aka 'describe-mode'
  (setq mode-name  "Xog")
  (use-local-map    xog-mode-map)
  (run-hooks       'xog-mode-hook)      ; optionally defined in user's configs

  ;; use our custom menu, "Xog", located to the right of "Tools"
  (menu-bar-mode 1)

  ;; visual bell (because the Ubuntu system bell is broken as of mid-2010)
  (setq visible-bell t)

  ;; background (can be redefined within 'xog-mode-hook')
  (setq fill-column 999)                ; auto-fill wrap, essentially disabled
  (setq selective-display-ellipses nil) ; omit "..."
  (toggle-truncate-lines 1)             ; "1" is never fold (wrap) long lines

  ;; global variables
  ;; <none>

  ;; make your comment command use the same shortcut
  ;;   for 'comment-dwim' (dwim = do what I mean),
  ;; the user may have changed their default
  ;; (define-key xog-mode-map [remap comment-dwim] 'xog-comment-dwim)

  ;; miscellaneous
  (setq message-log-max t)              ; 't' is do not truncate "*Messages*"

  ;; general actions
  (highlight-current-line-minor-mode 1) ; highlight current line
  (xog-colorize)                        ; automatically colorize on opening
  (goto-line 3)

  ;; reset some variables
  ;;
  ;; tests indicate that these values do persist between killed
  ;; (`kill-buffer') non-file buffers -- moreover, making them
  ;; buffer local (`make-variable-buffer-local') did not help
  (setq xog-quieten-level 0)
  (setq xog-invisible-areas-list ())

  ;; Xumber mode
  ; both work the same
  (autoload 'xumber-mode
    xog-xumber-file
    "Toggle xog number mode."
    t)                                  ; 't' for interactive
  (xumber-mode t)
  (if xumber-mode
      (message "  xog: xog-mode: Xumber mode now active")
    (message "  xog: xog-mode: Xumber mode INACTIVE"))

  ;; completion reporting
  (message "  xog: xog-mode setup complete."))

;; ---------------------------------
;;  custom key maps
;; ---------------------------------

(defvar xog-mode-map nil "Local keymap for Xog mode buffers.")
;; CAUTION: must NOT be (make-local-variable 'xog-mode-map)

;; US keyboards
;;
;; 'prior' is designated PgUp and 'next' is PgDn --
;; moreover S-<prior> and S-<next> are not normally
;; bound in emacs

(if xog-mode-map
    ()                                  ; protection against double loading

  ;; keyboard shortcuts
  ;; 'super' is the Windows/Penguin key
  (setq xog-mode-map (make-sparse-keymap))
  (define-key xog-mode-map [(shift            up)] 'xog-move-warn-prior)
  (define-key xog-mode-map [(shift          down)] 'xog-move-warn-next)
  (define-key xog-mode-map [(shift          menu)] 'xog-show-source-context)
  (define-key xog-mode-map [(control        menu)] 'xog-show-header-context)
  (define-key xog-mode-map [(shift         prior)] 'xog-move-source-prior)
  (define-key xog-mode-map [(shift          next)] 'xog-move-source-next)
  (define-key xog-mode-map [(shift          home)] 'xog-move-source-define)
  (define-key xog-mode-map [(shift         right)] 'xog-move-pplus-next)
  (define-key xog-mode-map [(shift          left)] 'xog-move-pplus-prior)
  (define-key xog-mode-map [(shift           end)] 'xog-move-cta-next)
  (define-key xog-mode-map [(shift         pause)] 'xog-quieten-cycle)
  (define-key xog-mode-map [(shift         print)] 'xog-unquieten)
  (define-key xog-mode-map [(super          menu)] 'xog-occur-generic)
  (define-key xog-mode-map [(meta           menu)] 'xog-occur-warn)
  (define-key xog-mode-map [(                 f6)] 'xog-occur-next)
  (define-key xog-mode-map [(shift            f6)] 'xog-occur-prev)

  ;; custom menu (does not make use of the 'easymenu' elisp package)
  (defvar menuXog0 (make-sparse-keymap "Xog Menu"))    ; mouse label
  (defvar menuXog1 (make-sparse-keymap "Xog1"))
  (defvar menuXog2 (make-sparse-keymap "Xog2"))
  (defvar menuXog3 (make-sparse-keymap "Xog3"))
  (defvar menuXog4 (make-sparse-keymap "Xog4"))
  (defvar menuXog5 (make-sparse-keymap "Xog5"))

  ;; mouse usage
  (define-key xog-mode-map [C-S-down-mouse-1] menuXog0)

  ;; top-level entry (visible in menu bar, right of "Tools")
  (define-key xog-mode-map [menu-bar xog]          (cons "Xog"             menuXog0)) ; menu-bar label

  ;; main menu bottom (reverse order)
  (define-key menuXog0 [about]       '(menu-item "About"                 xog-menu-about    :help "One-line report on Xog mode"))
  (define-key menuXog0 [help]        '(menu-item "Describe mode"         xog-menu-describe :help "Standard emacs 'describe-mode' call"))
  (define-key menuXog0 [bindings]    '(menu-item "Describe key bindings" describe-bindings :help "Standard emacs 'describe-bindings' call"))
  (define-key menuXog0 [separator-1] '("--"))

  ;; sub-menu entries (reverse order)
  (define-key xog-mode-map [menu-bar xog submenu5] (cons "Miscellaneous"      menuXog5))
  (define-key xog-mode-map [menu-bar xog submenu4] (cons "Restrict"           menuXog4))
  (define-key xog-mode-map [menu-bar xog submenu3] (cons "Color"              menuXog3))
  (define-key xog-mode-map [menu-bar xog submenu2] (cons "Summarize (occur)"  menuXog2))
  (define-key xog-mode-map [menu-bar xog submenu1] (cons "Navigate"           menuXog1))

  ;; main menu top (reverse order)
  (define-key menuXog0 [separator-2] '("--"))
  (define-key menuXog0 [tog-screen]  '(menu-item "Full-screen (toggles)"       xog-fullscreen-me-toggle    :help "Toggle full screen (Linux only)"))
  (define-key menuXog0 [repeat]      '(menu-item "Repeat last command"         repeat                      :help "Standard emacs 'repeat' command"))
  (define-key menuXog0 [write-xog]   '(menu-item "Write free buffer to file"   xog-write-xog               :help "Write unvisited buffer to file after extracting the filename from the contents of the buffer"))
; (define-key menuXog0 [switch]      '(menu-item "Switch to useful buffer"     xog-switch-to-useful-buffer :help "Switch to useful buffer (can use C-x b to return)"))
  (define-key menuXog0 [cycle-bufs]  '(menu-item "Cycle to next useful buffer" xeona-cycle-buffers         :help "Cycle to next useful buffer"))
  (define-key menuXog0 [readonly]    '(menu-item "Toggle read-only"            toggle-read-only            :help "Standard emacs 'toggle-read-only' command"))
  (define-key menuXog0 [separator-3] '("--"))
  (define-key menuXog0 [header]      '(menu-item "Visit C++ associated header" xog-show-header-context     :help "Visit C++ header file in read-only mode (C-x k to 'kill-buffer')"))
  (define-key menuXog0 [source]      '(menu-item "Visit C++ source"            xog-show-source-context     :help "Visit C++ source file in read-only mode and then center on given line (C-x k to 'kill-buffer')"))

  ;; ---

  ;; submenu 1 (reverse order)
  (define-key menuXog1 [kkt-next]    '(menu-item "Next KKT fail"           xog-move-kkt-next        :help "Move down to next KKT quality fail"))
  (define-key menuXog1 [kkt-prior]   '(menu-item "Previous KKT fail"       xog-move-kkt-prior       :help "Move up to previous KKT quality fail"))
  (define-key menuXog1 [valg-next]   '(menu-item "Next valgrind"           xog-move-valgrind-next   :help "Move down to next main valgrind call"))
  (define-key menuXog1 [valg-prior]  '(menu-item "Previous valgrind"       xog-move-valgrind-prior  :help "Move up to previous main valgrind call"))
  (define-key menuXog1 [glpk-next]   '(menu-item "Next GLPK"               xog-move-glpk-next       :help "Move down to next GLPK call"))
  (define-key menuXog1 [glpk-prior]  '(menu-item "Previous GLPK"           xog-move-glpk-prior      :help "Move up to previous GLPK call"))
  (define-key menuXog1 [call-next]   '(menu-item "Next call"               xog-move-call-next       :help "Move down to next manually defined call regex (see set call column regex)"))
  (define-key menuXog1 [call-prior]  '(menu-item "Previous call"           xog-move-call-prior      :help "Move up to previous manually defined call regex (see set call column regex)"))
  (define-key menuXog1 [call-set]    '(menu-item "Set call column regex"   xog-move-call-define     :help "Set call column regex for next and previous call commands"))
  (define-key menuXog1 [src-next]    '(menu-item "Next source"             xog-move-source-next     :help "Move down to next manually defined source regex (see set source column regex)"))
  (define-key menuXog1 [src-prior]   '(menu-item "Previous source"         xog-move-source-prior    :help "Move up to previous manually defined source regex (see set source column regex)"))
  (define-key menuXog1 [src-set]     '(menu-item "Set source column regex" xog-move-source-define   :help "Set source column regex for next and previous source commands"))
  (define-key menuXog1 [scale-next]  '(menu-item "Next scaling report"     xog-move-scale-next      :help "Move down to next GLPK scaling report"))
  (define-key menuXog1 [scale-prior] '(menu-item "Previous scaling report" xog-move-scale-prior     :help "Move up to previous GLPK scaling report"))
  (define-key menuXog1 [cta-next]    '(menu-item "Next CTA algorithm"      xog-move-cta-next        :help "Move down to next CAPSET or TRANSOLVE marker"))
  (define-key menuXog1 [cta-prior]   '(menu-item "Previous CTA algorithm"  xog-move-cta-prior       :help "Move up to previous CAPSET or TRANSOLVE marker"))
  (define-key menuXog1 [yeek-next]   '(menu-item "Next yeek"               xog-move-yeek-next       :help "Move down to next yeek"))
  (define-key menuXog1 [yeek-prior]  '(menu-item "Previous yeek"           xog-move-yeek-prior      :help "Move up to previous yeek"))
  (define-key menuXog1 [pplus-next]  '(menu-item "Next ++"                 xog-move-pplus-next      :help "Move down to next ++ simulation marker"))
  (define-key menuXog1 [pplus-prior] '(menu-item "Previous ++"             xog-move-pplus-prior     :help "Move up to previous ++ simulation marker"))
  (define-key menuXog1 [glpwa-next]  '(menu-item "Next GLPK Warning"       xog-move-glpk-warn-next  :help "Move down to next GLPK Warning:"))
  (define-key menuXog1 [glpwa-prior] '(menu-item "Previous GLPK Warning"   xog-move-glpk-warn-prior :help "Move up to previous GLPK Warning:"))
  (define-key menuXog1 [warn-next]   '(menu-item "Next WARN"               xog-move-warn-next       :help "Move down to next WARN log"))
  (define-key menuXog1 [warn-prior]  '(menu-item "Previous WARN"           xog-move-warn-prior      :help "Move up to previous WARN log"))

  ;; submenu 2 (reverse order)
  (define-key menuXog2 [occur-next]  '(menu-item "Navigate down thru summaries"  xog-occur-next     :help "Navigate down thru the summaries"))
  (define-key menuXog2 [occur-osp]   '(menu-item "List OSP markers OSP-00"       xog-occur-osp-00   :help "Summarize OSP-00 optimization sub-problem markers"))
  (define-key menuXog2 [occur-fac]   '(menu-item "List factory markers FAC-00"   xog-occur-fac-00   :help "Summarize FAC-00 factory markers"))
  (define-key menuXog2 [occur-eq]    '(menu-item "List scaling reports EQ:"      xog-occur-scale-eq :help "Summarize GLPK EQ: scaling reports"))
  (define-key menuXog2 [occur-a]     '(menu-item "List scaling reports  A:"      xog-occur-scale-a  :help "Summarize GLPK A: scaling reports"))
  (define-key menuXog2 [occur-warn]  '(menu-item "List WARNs"                    xog-occur-warn     :help "Summarize lines containing \"   WARN   \"."))
  (define-key menuXog2 [occur-gen]   '(menu-item "General interface to 'occur'"  xog-occur-generic  :help "Summarize lines containing prompted regex using 'occur'."))

  ;; submenu 3 (reverse order)
  (define-key menuXog3 [uncolorize]  '(menu-item "Uncolorize buffer"                   hi-lock-mode                      :help "Remove color emphasis from buffer (by running 'hi-lock-mode')"))
  (define-key menuXog3 [colorize]    '(menu-item "Colorize buffer"                     xog-colorize                      :help "Add color emphasis to buffer (normally on by default)"))
  (define-key menuXog3 [hi-phrase]   '(menu-item "Highlight a regex"                   highlight-phrase                  :help "Highlight regex phrase (by running 'highlight-phrase')"))
  (define-key menuXog3 [highlight]   '(menu-item "Toggle current line highlight (hcl)" highlight-current-line-minor-mode :help "Toggle current line highlight (light-brown) (by running 'highlight-current-line-minor-mode')"))
; (define-key menuXog3 [hl-line]     '(menu-item "Toggle highlight line"               hl-line-mode                      :help "Toggle current line highlight (green) (by running 'hl-line-mode')"))
; (define-key menuXog3 [highline]    '(menu-item "Toggle highline (hl)"                highline-mode                     :help "Toggle current line highlight (grey) (by running 'highline-mode')"))

  ;; submenu 4 (reverse order)
  (define-key menuXog4 [unquieten]   '(menu-item "Reveal hidden rank logs" xog-unquieten     :help "Reveal all previously hidden log ranks"))
  (define-key menuXog4 [cycle]       '(menu-item "Cycle hide rank logs"    xog-quieten-cycle :help "Cycle log ranks in { WARN info dbug xtra adhc }"))

  ;; submenu 5 (reverse order)
  (define-key menuXog5 [font-dec]    '(menu-item "Decrease text size here" text-scale-decrease :help "Decrease text size using a 'text-scale' function (also <S-down-mouse-1>)"))

 ) ;; final parenthesis

;; ---------------------------------
;;  aliases
;; ---------------------------------

;; CAUTION: `defalias' on 2007 GNU Emacs 21.4 does not support an optional DOCSTRING

;; ---------------------------------
;;  prioritize coloration
;; ---------------------------------

;; prioritize coloration over current line highlighting
;; source: http://www.emacswiki.org/emacs/HiLock
;; status: works as advertised up to about 2000 lines

(defadvice hi-lock-set-pattern (around use-overlays activate)
  "Mode function `hi-lock-mode' to prioritize coloration over current line highlight."
  (let ((font-lock-fontified nil))
    ad-do-it))

;; ---------------------------------
;;  buffer switch
;; ---------------------------------

;; key binding common with 'xem.el'

(defun xog-switch-to-useful-buffer ()
  "Switch to useful buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;; ---------------------------------
;;  menu support
;; ---------------------------------

(defun xog-menu-about ()
  "About message for `xog-mode'."
  (interactive)
  (let* ((file    (car (load-history-filename-element "xog"))                                ) ; [1]
         (sedstr  "s/^[[:space:]]*;\\+[[:space:]]*\\$Revision: \\([[:digit:]]*\\) \\$/\\1/p" )
         (call    (concat "sed --quiet '" sedstr "' " file)                                  )
         (svnver "(not set)"                                                                 ))
    (message "  xog: about: file: '%s'" file)
    (message "  xog: about: sed: '%s'" sedstr)
    (setq svnver (shell-command-to-string call))
    (if (string-match "\n$" svnver)                    ; trim trailing newline
        (setq svnver (replace-match "" t t svnver)))   ; fixed case and literal
    (message "  xog: about: svnver: '%s'" svnver)
    (message "%s major mode %s for reviewing xeona output." mode-name svnver)))
    ;; [1] this file

(defun xog-menu-describe ()
  "Wrapper to `describe-mode'."
  (interactive)
  (describe-mode))

;; ---------------------------------
;;  full screen mode
;; ---------------------------------

(defun xog-fullscreen-me-toggle ()
  "Toggle full-screen mode."
  (interactive)
  (xog-fullscreen-me t))

(defun xog-fullscreen-me (&optional toggle)
  "Adopt full-screen mode.  If TOGGLE, then alternate with part-screen mode."
  ;; CAUTION: contains platform-specific code
  ;; http://www.emacswiki.org/emacs/FullScreen
  (cond
   ((eq system-type 'gnu/linux)
    ;; 1 = fixed full, 2 = toggle for each call
    (if toggle
        (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0))
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(1 "_NET_WM_STATE_FULLSCREEN" 0))))
   ((eq system-type 'windows-nt)
    (message "  xog: fullscreen-me: WARNING: full-screen mode not coded for Windows operating system"))
   (t
    (message "  xog: fullscreen-me: WARNING: full-screen mode not coded for this operating system"))) )

;; ---------------------------------
;;  CTA, ++, GLPK Warning navigation
;; ---------------------------------

(defconst xog-move-cta-regex " CAPSET-[0-9][0-9] \\| TRANSOLVE-[0-9][0-9] ")

(defun xog-move-cta-next ()
  "Move to next CTA (capset and transolve algorithm) entry."
  (interactive)
  (let ((regex  xog-move-cta-regex))
    (if (xog-move-regex-next regex)
        (message " xog: move to next CTA using '%s' complete." regex)
      (message "  xog: move to next CTA using '%s' FAILED." regex))))

(defun xog-move-cta-prior ()
  "Move to prior CTA (capset and transolve algorithm) entry."
  (interactive)
  (let ((regex  xog-move-cta-regex))
    (if (xog-move-regex-prior regex)
        (message " xog: move to previous CTA using '%s' complete." regex)
      (message "  xog: move to previous CTA using '%s' FAILED." regex))))

(defconst xog-move-scale-regex "^ A: ")

(defun xog-move-scale-next ()
  "Move to next scaling report entry."
  (interactive)
  (let ((regex  xog-move-scale-regex))
    (if (xog-move-regex-next regex)
        (message " xog: move to next scaling report using '%s' complete." regex)
      (message "  xog: move to next scaling report using '%s' FAILED." regex))))

(defun xog-move-scale-prior ()
  "Move to prior scaling report entry."
  (interactive)
  (let ((regex  xog-move-scale-regex))
    (if (xog-move-regex-prior regex)
        (message " xog: move to previous scaling report using '%s' complete." regex)
      (message "  xog: move to previous scaling report using '%s' FAILED." regex))))

(defconst xog-move-pplus-regex "^   \\+\\+ ")

(defun xog-move-pplus-next ()
  "Move to next ++ entry."
  (interactive)
  (let ((regex  xog-move-pplus-regex))
    (if (xog-move-regex-next regex)
        (message " xog: move to next ++ using '%s' complete." regex)
      (message "  xog: move to next ++ using '%s' FAILED." regex))))

(defun xog-move-pplus-prior ()
  "Move to prior ++ entry."
  (interactive)
  (let ((regex  xog-move-pplus-regex))
    (if (xog-move-regex-prior regex)
        (message " xog: move to previous ++ using '%s' complete." regex)
      (message "  xog: move to previous ++ using '%s' FAILED." regex))))

(defconst xog-move-yeek-regex "yeek[[:space:]]+[[:digit:]]+$")

(defun xog-move-yeek-next ()
  "Move to next yeek 00 message."
  (interactive)
  (let ((regex xog-move-yeek-regex))
    (if (xog-move-regex-next regex)
        (message " xog: move to next yeek using '%s' complete." regex)
      (message "  xog: move to next yeek using '%s' FAILED." regex))))

(defun xog-move-yeek-prior ()
  "Move to previous yeek 00 message."
  (interactive)
  (let ((regex xog-move-yeek-regex))
    (if (xog-move-regex-prior regex)
        (message " xog: move to previous yeek using '%s' complete." regex)
      (message "  xog: move to previous yeek using '%s' FAILED." regex))))

(defconst xog-move-glpk-warn-regex "^Warning:")

(defun xog-move-glpk-warn-next ()
  "Move to next GLPK Warning entry."
  (interactive)
  (let ((regex  xog-move-glpk-warn-regex))
    (if (xog-move-regex-next regex)
        (message " xog: move to next Warning using '%s' complete." regex)
      (message "  xog: move to next Warning using '%s' FAILED." regex))))

(defun xog-move-glpk-warn-prior ()
  "Move to prior GLPK warning entry."
  (interactive)
  (let ((regex  xog-move-glpk-warn-regex))
    (if (xog-move-regex-prior regex)
        (message " xog: move to previous Warning using '%s' complete." regex)
      (message "  xog: move to previous Warning using '%s' FAILED." regex))))

;; ---------------------------------
;;  navigation support
;; ---------------------------------

(defun xog-move-regex-next (regex)
  "Navigate to next REGEX."
  (let ()
    (if (looking-at regex)
        (forward-char +1))                   ; need to "kick-start" the search
    (if (re-search-forward regex nil t)      ; limit (no), no error (true)
        (progn
          (recenter xog-recenter)
          (message "  xog: move-regex-next: move to next '%s' complete." regex)
          t)
      (message "  xog: move-regex-next: move to next '%s' FAILED." regex)
      nil)))

(defun xog-move-regex-prior (regex)
  "Navigate to previous REGEX."
  (let ()
    (if (re-search-backward regex nil t)      ; limit (no), no error (true)
        (progn
          (recenter xog-recenter)
          (message "  xog: move-regex-prior: move to previous '%s' complete." regex)
          t)
      (message "  xog: move-regex-prior: move to previous '%s' FAILED." regex)
      nil)))

;; ---------------------------------
;;  call navigation
;; ---------------------------------

(defvar xog-move-call-regex "" "Call column navigation pattern.")

(defun xog-move-call-define (regex)
  "Seek and record call column navigation pattern REGEX."
  (interactive "sEnter call column regex: ")
  (let ((was xog-move-call-regex))
    (setq xog-move-call-regex regex)
    (message "  xog: move call define complete moving from '%s' to '%s'." was regex)))

(defun xog-move-call-next ()
  "Navigate to next call log."
  (interactive)
  (let ((regex    xog-move-call-regex)
        (parts    (make-list 7 "")       )   ; dummy for first iteration
        (start    (point)))
    (message "  xog: move-call-next: %s" parts)
    (while (and (not (eobp))
                (not (string-match regex (nth 2 parts))))   ; (string-match "a+" "") returns 'nil'
      (forward-line +1)
      (setq parts (xog-split-repx (thing-at-point 'line)))
      (if parts () (setq parts (make-list 7 "") )))
    (recenter xog-recenter)
    (if (eobp)
        (progn
          (goto-char start)
          (message "  xog: move to next call '%s' FAILED." regex))
      (message "  xog: move to next call '%s' complete." regex))))

(defun xog-move-call-prior ()
  "Navigate to previous call log."
  (interactive)
  (let ((regex    xog-move-call-regex)
        (parts    (make-list 7 "")       )   ; dummy for first iteration
        (start    (point)))
    (message "  xog: move-call-next: %s" parts)
    (while (and (not (bobp))
                (not (string-match regex (nth 2 parts))))   ; (string-match "a+" "") returns 'nil'
      (forward-line -1)
      (setq parts (xog-split-repx (thing-at-point 'line)))
      (if parts () (setq parts (make-list 7 "") )))
    (recenter xog-recenter)
    (if (bobp)
        (progn
          (goto-char start)
          (message "  xog: move to previous call '%s' FAILED." regex))
      (message "  xog: move to previous call '%s' complete." regex))))

;; ---------------------------------
;;  source navigation
;; ---------------------------------

(defvar xog-move-source-regex "" "Source column navigation pattern.")

(defun xog-move-source-define (regex)
  "Seek and record source column navigation pattern REGEX."
  (interactive "sEnter source column regex (single escape real dots): ")
  (let ((was xog-move-source-regex))
    (setq xog-move-source-regex regex)
    (message "  xog: move source define complete moving from '%s' to '%s'." was regex)))

(defun xog-move-source-next ()
  "Navigate to next source log."
  (interactive)
  (let ((regex    xog-move-source-regex)
        (parts    (make-list 7 "")       )                  ; dummy for first iteration
        (start    (point)))
    (message "  xog: move-source-next: %s" parts)
    (while (and (not (eobp))
                (not (string-match regex (nth 1 parts))))   ; (string-match "a+" "") returns 'nil'
      (forward-line +1)
      (setq parts (xog-split-repx (thing-at-point 'line)))
      (if parts () (setq parts (make-list 7 "") )))
    (recenter xog-recenter)
    (if (eobp)
        (progn
          (goto-char start)
          (message "  xog: move to next source '%s' FAILED." regex))
      (message "  xog: move to next source '%s' complete." regex))))

(defun xog-move-source-prior ()
  "Navigate to previous source log."
  (interactive)
  (let ((regex    xog-move-source-regex )
        (parts    (make-list 7 "")      )                  ; dummy for first iteration
        (start    (point)               ))
    (message "  xog: move-source-next: %s" parts)
    (while (and (not (bobp))
                (not (string-match regex (nth 1 parts))))   ; (string-match "a+" "") returns 'nil'
      (forward-line -1)
      (setq parts (xog-split-repx (thing-at-point 'line)))
      (if parts () (setq parts (make-list 7 "") )))
    (recenter xog-recenter)
    (if (bobp)
        (progn
          (goto-char start)
          (message "  xog: move to previous source '%s' FAILED." regex))
      (message "  xog: move to previous source '%s' complete." regex))))

;;---------------------------------
;; GLPK navigation
;;---------------------------------

;; CAUTION: the line number below may change
;; (defconst xog-move-glpk-regex "2735   d/siglp.cc       solverInvokeSolver")
(defconst xog-move-glpk-regex "   problem name                    : ")

(defun xog-move-glpk-next ()
  "Move to next GLPK message."
  (interactive)
  (let ((regex xog-move-glpk-regex))
    (if (xog-move-regex-next regex)
        (message " xog: move to next GLPK using '%s' complete." regex)
      (message "  xog: move to next GLPK using '%s' FAILED." regex))))

(defun xog-move-glpk-prior ()
  "Move to next GLPK message."
  (interactive)
  (let ((regex xog-move-glpk-regex))
    (if (xog-move-regex-prior regex)
        (message " xog: move to previous GLPK using '%s' complete." regex)
      (message "  xog: move to previous GLPK using '%s' FAILED." regex))))

;;---------------------------------
;; KKT navigation
;;---------------------------------

(defconst xog-move-kkt-regex "quality : low\\|quality : problematic")      ; note the or'ing

(defun xog-move-kkt-next ()
  "Move to next kkt message."
  (interactive)
  (let ((regex xog-move-kkt-regex))
    (if (xog-move-regex-next regex)
        (message " xog: move to next KKT using '%s' complete." regex)
      (message "  xog: move to next KKT using '%s' FAILED." regex))))

(defun xog-move-kkt-prior ()
  "Move to next kkt message."
  (interactive)
  (let ((regex xog-move-kkt-regex))
    (if (xog-move-regex-prior regex)
        (message " xog: move to previous KKT using '%s' complete." regex)
      (message "  xog: move to previous KKT using '%s' FAILED." regex))))

;;---------------------------------
;; valgrind navigation
;;---------------------------------

(defconst xog-move-valgrind-regex "^==[[:digit:]]+== [[:alnum:]]")    ; just one space separator

(defun xog-move-valgrind-next ()
  "Move to next valgrind message."
  (interactive)
  (let ((regex xog-move-valgrind-regex))
    (if (xog-move-regex-next regex)
        (message " xog: move to next valgrind using '%s' complete." regex)
      (message "  xog: move to next valgrind using '%s' FAILED." regex))))

(defun xog-move-valgrind-prior ()
  "Move to next valgrind message."
  (interactive)
  (let ((regex xog-move-valgrind-regex))
    (if (xog-move-regex-prior regex)
        (message " xog: move to previous valgrind using '%s' complete." regex)
      (message "  xog: move to previous valgrind using '%s' FAILED." regex))))

;; ---------------------------------
;;  WARN navigation
;; ---------------------------------

(defconst xog-warn-tag "WARN")

(defun xog-move-warn-next ()
  "Navigate to next WARN log."
  (interactive)
  (let ((tag    xog-warn-tag)
        (parts  nil           ))
    (while (and (not (eobp))
                (not (string-equal tag (nth 5 parts))))     ; [1]
      (forward-line +1)
      (setq parts (xog-split-repx (thing-at-point 'line))))
    (recenter xog-recenter)
    (if (eobp)
        (message "  xog: move to next WARN FAILED.")
      (message "  xog: move to next WARN complete."))))
      ;; [1] (nth 5 nil) returns nil / (string-equal "abc" nil) returns nil

(defun xog-move-warn-prior ()
  "Navigate to previous WARN log."
  (interactive)
  (let ((tag    xog-warn-tag)
        (parts  nil           ))
    (while (and (not (bobp))
                (not (string-equal tag (nth 5 parts))))
      (forward-line -1)
      (setq parts (xog-split-repx (thing-at-point 'line))))
    (recenter xog-recenter)
    (if (bobp)
        (message "  xog: move to previous WARN FAILED.")
      (message "  xog: move to previous WARN complete."))))

;; ---------------------------------
;; generic occur call
;; ---------------------------------

(defvar xog-occur-generic-hx
  (list "   WARN   "                         ; normal WARN
        "problem name[[:blank:]]+:"          ; search for optimization problem details
        "AC transmission"                    ; for AC transmission
        "DC transmission"                    ; for DC transmission
        "building simulation summary$"       ; from class 'BuildingSim'
        "current plant role"                 ; part of the "building simulation summary"
        "plant data:$"                       ; part of the "building simulation summary"
        "carryover"                          ; part of the "building simulation summary"
        "bid tweak"                          ; used by operators bidding for more than one asset
        "    FAC-[[:digit:]]+ "              ; factory marker
        "    OSP-[[:digit:]]+ "              ; optimization sub-problem marker (six leading space actually)
        "asset capacitation"                 ; from 'AsopLmpBid*' at least
        "adhc   weighted price"              ; from 'AsopLmpBidAdaptive1::constrain'
        "ratio incap : outcap"               ; from 'OpsStore_A::uploadEngineering'
        "storage OSP reporting"              ; from 'OpsStore_A' calls more generally
        "note row"                           ; from 'svif::SolverIf::loadRhs'
        "note col"                           ; from 'svif::SolverIf::loadObj'
        "note row\\|note col"                ; from both the above
        "bughunt:"                           ; special reporting
        "variable ghg cost"                  ; from 'TeasOxidToElec::washup''TeasCcgt::washup' 'TeasCcsGeological::washup'
        "carbon burial"                      ; ditto
        "high precision report"              ; ".. for overseer final value" from 'Overseer::run'
        " [+-]?inf "                         ; generalized 'inf' search
        )
  "Current and hardcoded history list for `xog-occur-generic-hx'.")

(defun xog-occur-generic ()
  "Interface to `occur' with session (short-term) and hardcoded (long-term) memory."
  (interactive)
  (let* ((regex  nil                        )
         (hx     xog-occur-generic-hx       ))
    (setq regex (read-string
                 "Enter regex (or scroll): " ; prompt
                 (pop hx)                    ; initial
                 'hx                         ; history list
                 nil))                       ; set default
    (if (null regex)
        (message "  xog: occur-generic: no regex given, no action taken.")
      (if (not (string-equal regex (car xog-occur-generic-hx)))  ; avoid doubling up
          (push regex xog-occur-generic-hx))
      (occur regex 0)
      (shrink-window-if-larger-than-buffer (next-window))
      (message "  xog: occur-generic: complete using regex '%s'." regex)) ))

(defun xog-occur-next ()                     ; could be cyclic
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
    (message "  xog: occur-next: complete.") ))

(defun xog-occur-prev ()                     ; could be cyclic
  "Reverse skip thru `occur' occurrences.

Continues to work fine if the main window has been maximized using `delete-other-windows'."
  ;; gracefully reports "No buffer named *Occur*" if that is the case
  ;; this is probably key bound too, perhaps to <S-f6>
  (interactive)
  (let* ((ob  "*Occur*" ))
    (set-buffer ob)
    (occur-prev 1)
    (occur-mode-goto-occurrence)
    (message "  xog: occur-next: complete.") ))

;; ---------------------------------
;; occur calls
;; ---------------------------------

(defun xog-occur-warn ()
  "List WARN lines."
  (interactive)
  (let ((regex "   WARN   " ))
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xog: occur-warn complete, regex '%s'." regex)))

(defun xog-occur-scale-a ()
  "List scaling A: reports."
  (interactive)
  (let ((regex "^ A:" ))
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xog: occur-scale-a complete, regex '%s'." regex)))

(defun xog-occur-scale-eq ()
  "List scaling EQ: reports."
  (interactive)
  (let ((regex "^EQ:" ))
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xog: occur-scale-eq complete, regex '%s'." regex)))

(defun xog-occur-fac-00 ()
  "List FAC-00 markers."
  (interactive)
  (let ((regex "^    FAC-[[:digit:]]+ "))
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xog: occur-fac-00 complete, regex '%s'." regex)))

(defun xog-occur-osp-00 ()
  "List OSP-00 markers."
  (interactive)
  (let ((regex "^      OSP-[[:digit:]]+ "))
    (occur regex 0)
    (shrink-window-if-larger-than-buffer (next-window))
    (message "  xog: occur-osp-00 complete, regex '%s'." regex)))

;; ---------------------------------
;;  write lost buffer to XOG file
;; ---------------------------------

(defun xog-write-xog ()
  "Write XOG file after extracting the filename from the contents of the buffer.

Existing files are not backed up, but overwrites are prompted."
  ;; typical "     model file        :  /home/robbie/..../trunk/xeona1/xeona-xmoks/submodel.16.xem"
  (interactive)
  (save-excursion
    (let ((xog-key        "^     model file        :  " )
          (xog-extension  xog-extension                 )
          (xog-name                                     )
          (capture                                      ))
      (if (buffer-file-name)                 ; indicates a visited file
          (message "  xog: write xog encountered a live file '%s' so quiting without action."
                   (abbreviate-file-name buffer-file-name))
        (goto-char (point-min))
        (if (re-search-forward xog-key)
            (progn
              (setq capture (buffer-substring-no-properties (match-end 0) (point-at-eol)))
              (message "  xog: write-xog: points: %d:%d capture: '%s'"
                       (match-end 0) (point-at-eol) capture)
              (setq cature (xog-string-trim capture))
              (setq xog-name (concat (file-name-sans-extension capture) xog-extension))
              (write-file xog-name t)        ; 't' means seek confirmation on overwrite
              (message "  xog: write xog succeeded using '%s'." (abbreviate-file-name xog-name)))
          (message "  xog: write xog cannot find suitable model file (is this really xog output?).")))
      )))

;; ---------------------------------
;;  visit source code
;; ---------------------------------

(defun xog-show-source-context ()
  "Open the source file indicated by the current log line.
Or else try to fail gracefully."
  (interactive)
  (let ((parts      nil )
        (lineno     0   )
        (source     ""  )
        (filename   ""  )
        (buffername ""  ))
    (setq parts (xog-split-repx (thing-at-point 'line)))
    (if parts
        (progn
          (setq lineno (nth 0 parts))
          (setq lineno (string-to-number lineno))
          (setq source (nth 1 parts))
          (setq filename (file-truename (concat xog-sourcecode source)))
          (if (xog-open-source filename lineno)
              (message "  xog: show source context opening source file '%s' at line '%d'."
                       (substring-no-properties source)     ; remove (yellow) coloration
                       lineno)
            (message "  xog: show-source-context: source file unreadable '%s'" filename)))
      (message "  xog: show source context unable to identify a source file at the current line.")) ))

(defun xog-show-header-context ()
  "Open the associated header file indicated by the current log line.
Or else try to fail gracefully."
  (interactive)
  (let ((parts       nil )
        (lineno      0   )    ; CAUTION: never reset - not a valid lineno but 'emacs' doesn't mind
        (source      ""  )
        (filename    ""  )
        (buffername  ""  )
        (header-ext  "h" ))
    (setq parts (xog-split-repx (thing-at-point 'line)))
    (if parts
        (progn
          (setq source (nth 1 parts))
          (setq source (concat (file-name-sans-extension source) "." header-ext))
          (setq filename (file-truename (concat xog-sourcecode source)))
          (if (xog-open-source filename lineno)
              (message "  xog: show header context opening header file '%s' at line '%d'."
                       (substring-no-properties source)     ; remove (yellow) coloration
                       lineno)
            (message "  xog: show-header-context: header file unreadable '%s'" filename)))
      (message "  xog: show header context unable to identify a source file at the current line.")) ))

(defun xog-open-source (filename lineno)
  "Open FILENAME at LINENO using hardcoded behaviors.

The FILENAME must be absolute.  And LINENO must be an integer.
LINENO zero means use first line.

Currently, the buffer is read-only and the entire current line
is color highlighted."
  (interactive)
  (if (and (stringp filename)
           (integerp lineno))
      (if (file-readable-p filename)
          (progn
;;;         (find-file filename)             ; also names and switches to buffer
            (find-file-read-only filename)   ; also names and switches to buffer
            (goto-line lineno)
            (recenter nil)
            (highlight-current-line-minor-mode 1)
            (message "  xog: open-source: success"))
        (message "  xog: open-source: filename bad or file not readable %s" filename))
    (message "  xog: open-source: faulty arguments (is lineno an integer)")) )

;; ---------------------------------
;;  repx functions (find and parse logs)
;; ---------------------------------

;; the repx functions recover information made with 'xeona'
;; logging call, which might be invoked by:
;;
;;   s_logger->repx(logga::dbug, "some text", someFloat)
;;
;; function `xog-split-repx' is used be a number of other
;; functions, including the source code context function and
;; the WARN navigation functions (unless this proves to slow)
;;
;; representative logging output follows
;;
;; line   source           call                     no   delta-t    rank   message                              comment/value
;; ..................................................................................................................................
;; 1869   d/siglp.cc       resetProblem           2150   00.0000s   info   problem reset, rows x cols           00 x 00
;;
;; function `xog-split-repx' is predicated on the fact
;; that separation of at least two spaces is always
;; present -- this is believed to be correct

(defconst xog-rank-regex
  "YEEK\\|KILL\\|WARN\\|info\\|dbug\\|xtra\\|adhc\\|\\?\\?\\?\\?"
  "Repx tags as defined in function 'Logger::calcRank'.")

(defun xog-split-repx (line)
  "Return a list of the eight log elements if LINE is a xeona log line.
Otherwise return nil.

The LINE argument is deemed to have been produced by 'logga::Logger::repx'
after testing its contents.  A false positive is extremely unlikely."
  (interactive)
  (let ((repx-sep    "[ ][ ]+"      )
        (rank-regex  xog-rank-regex ))
    ;; ensure string
    (if (stringp line)
        (progn
          (setq line (xog-string-trim line))
;         (message "  xog: split-repx: line\n    '%s'" line)
          (setq parts (split-string line repx-sep t))
;;;       (if (= (length parts) 7) (push "" parts))    ; this line resulted in a noticeable speedup
;         (message "  xog: split-repx: length %d" (length parts))
;         (dolist (part parts)
;           (message "    %s" (xog-string-trim part)))
;         (message "  xog: split-repx listing complete")
          ;; integrity tests
          ;;
          ;; [1] source file line number __LINE__
          ;; [2] source file name __FILE__
          ;; [3] function name __func__
          ;; [4] repx print number
          ;; [5] elapsed time interval
          ;; [6] repx rank
          ;; [7] message
          ;; [8] can be absent
          (if (and (xog-split-repx-integrity "^[[:digit:]]+$"  (nth 0 parts)  "line")          ; [1]
                   (xog-split-repx-integrity  "\\.h\\|\\.cc$"  (nth 1 parts)  "source")        ; [2]
                   (xog-split-repx-integrity "^[[:print:]]+$"  (nth 2 parts)  "call")          ; [3]
                   (xog-split-repx-integrity "^[[:digit:]]+$"  (nth 3 parts)  "no")            ; [4]
                   (xog-split-repx-integrity  "[[:digit:]]+s$" (nth 4 parts)  "delta-t")       ; [5]
                   (xog-split-repx-integrity  rank-regex       (nth 5 parts)  "rank")          ; [6]
                   (xog-split-repx-integrity "^[[:print:]]+$"  (nth 6 parts)  "message")       ; [7]
;;;                (xog-split-repx-integrity "^[[:print:]]*$"  (nth 7 parts) "comment/value")  ; [8]
                   ) ; and
              (progn
;               (message "  xog: split-repx: line split and checked for integrity")
                parts)                       ; return meaningful list
;           (message "  xog: split-repx: line split but failed at least one integrity check")
            nil))
;     (message "  xog: split-repx: line not string")
      nil) ))                                ; return empty list (same as ())

(defun xog-split-repx-integrity (regex input &optional explain)
  "Check if REGEX is able to match INPUT.

If not, provide a readable error message, incorporating EXPLAIN if provided."
  (if (not explain)
      (setq explain "not explained"))
  (if (stringp input)
      (if (string-match regex input)
          t                                  ; return success
;       (message "  xog: split-repx-integrity: match with regex \"%s\" and input \"%s\" (%s) failed"
;                regex input explain)
        nil)                                 ; return fail
;   (message "  xog: split-repx-integrity: input not string")
    nil))                                    ; return fail

;; ---------------------------------
;;  quieten (progressively hide rank logs)
;; ---------------------------------

;; the rank strings are defined in 'xeona' function 'Logger::calcRank'
;; note also "????" may be used, which regexes to "\\?\\?\\?\\?"

(defconst xog-quieten-ranks
  (list "WARN" "info" "dbug" "xtra" "adhc")  ; add or remove ranks as required
  "Ordered list of log ranks.")

(defvar xog-quieten-level 0
  "Current log rank quieten level.")

(defun xog-quieten-cycle ()
  "Cycle thru levels of quietness."
  (interactive)
  (save-excursion
    (let* ((level   xog-quieten-level  )     ; 24-Jul-2010: can this be an alias?
           (ranks   xog-quieten-ranks  )
           (depth   (length ranks)       )
           (trip    nil                  )
           (count   0                    ))  ; count returned from `xog-quieten'
      (incf level)                           ; increment level
      (if (> level depth) (setq level 0))    ; roll around to zero as required
      (setq xog-quieten-level level)
      (if (= level 0)                        ; show all
          (progn
            (message "  xog: quieten-cycle: about to call (xog-unquieten)")
            (xog-unquieten)
            (message "  xog: quieten-cycle: reset to level %d." level))
        (setq trip (nth (1- level) (reverse ranks)))   ; simple regex
        (message "  xog: quieten-cycle: trip %s "trip)
        (message "  xog: quieten-cycle: about to call (xog-quieten %s')" trip)
        (setq count (xog-quieten trip))
        (message "  xog: quieten-cycle: level %d with ranks %S removing %d lines this time."
                 level trip count))
      trip)))

;; core hiding code based on 'hide-lines' package by Mark Hulme-Jones
;; source: http://www.emacswiki.org/emacs/hide-lines.el

(defvar xog-invisible-areas-list ()
  "List of invisible overlays used by Xog mode.")

(add-to-invisibility-spec 'xog)

(defun add-invisible-overlay (start end)
  "Add an overlay from START to END for the current buffer.
Push that overlay onto the `xog-invisible-areas-list' list."
  (let ((overlay (make-overlay start end)))
    (setq xog-invisible-areas-list (cons overlay xog-invisible-areas-list))
    (overlay-put overlay 'invisible 'xog))) ; overlay 'overlay' given property 'invisible' value 'xog'

(defun xog-quieten (ranks)
  "Hide log ranks given by RANKS.

Use function `xog-unquieten' to reverse this action.

Some aesthetic problems with current line highlighting may arise,
but these disappear on unquieting."
  (make-variable-buffer-local 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (let ((regex  ranks                )
        (parts  nil                  )
        (count  0                    )                 ; hide count
        (start                       ))
    (goto-char (point-min))                            ; beginning of buffer
    (while (not (eobp))
      (setq parts (xog-split-repx (thing-at-point 'line)))
      (if (and (not (null parts))                      ; conditions evaluated in order (unlike C)
               (string-match regex (nth 5 parts)))     ; (string-match "a+" "") returns 'nil'
          (progn
            (incf count)
            (beginning-of-line)
            (setq start (point))
            (end-of-line)
            (forward-char +1)
            (add-invisible-overlay start (point)))
        (forward-line +1)))
    (message "  xog: quieten '%s' complete with %d lines removed." regex count)
    count))

(defun xog-unquieten ()
  "Reveal areas previous hidden by `xog-quieten'."
  (interactive)
  (mapcar (lambda (overlay) (delete-overlay overlay))
          xog-invisible-areas-list)
  (setq xog-invisible-areas-list ())
  (setq xog-quieten-level 0)                 ; reset level
  (xog-colorize)
  (message "  xog: unquieten complete."))

;; ---------------------------------
;;  colorizing
;; ---------------------------------

;; Xll colors  : /etc/X11/rgb.txt
;; show colors : file:///home/robbie/synk/xeona/x11-colors/x11-color-names.html
;; package     : /usr/share/emacs/21.4/lisp/hi-lock.el

;; interesting: aquamarine gold salmon yellow lemon_chiffon

;; complain if not Xll -- defensive programming
(if (eq window-system 'x)                    ; nil = ordinary terminal, 'w32 = MSWin
    (message "  xog: X11 found.")
  (error "  xog: WARNING: X11 assumed but not found"))

;; define colors
(defface xog-hi-color-B02
  '((((background dark)) (:background "gold" :foreground "black"))
    (t (:background "gold")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xog-hi-color-B04
  '((((background dark)) (:background "salmon" :foreground "black"))
    (t (:background "salmon")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xog-hi-color-B06
  '((((background dark)) (:background "burlywood" :foreground "black"))
    (t (:background "burlywood")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xog-hi-color-B07
  '((((background dark)) (:background "beige" :foreground "black"))
    (t (:background "beige")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface xog-hi-color-B08
  '((((background dark)) (:background "sandy brown" :foreground "black"))
    (t (:background "sandy brown")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

;; define colorizing regexes
(defconst xog-fake         "this regex should probably never match")
(defconst xog-warn-regex1  "[[:digit:]]s   WARN  ")
(defconst xog-warn-regex2  "^.*[[:digit:]]s   WARN  ")
(defconst xog-BIG_1-regex  " LOOP-[0-9][0-9] \\| OSP-[0-9][0-9] \\| FAC-[0-9][0-9] ")
(defconst xog-BIG_2-regex  " CAPSET-[0-9][0-9] \\| TRANSOLVE-[0-9][0-9] ")
(defconst xog-pplus-regex  " \\+\\+ ")

(defun xog-colorize ()
  "Colorize a xog output buffer."
  (interactive)
  (message "  xog: colorize commencing")
  ;; CAUTION: order is important .. this is like paint
  ;; CAUTION: the first "fake" regex causes any current line
  ;; highlighting to slip gracefully underneath -- tested with
  ;; both 'highlight-current-line-minor-mode' (part of Emacs) and
  ;; 'highline' (not part of Emacs) -- also requires the
  ;; associated `hi-lock-set-pattern' defadvice
  (highlight-phrase                 xog-fake         'xog-hi-color-B04)
; (highlight-lines-matching-regexp  xog-warn-regex1  'xog-hi-color-B04)
  (highlight-phrase                 xog-warn-regex2  'xog-hi-color-B04)
  (highlight-phrase                 xog-BIG_1-regex  'xog-hi-color-B06)
  (highlight-phrase                 xog-BIG_2-regex  'xog-hi-color-B06)
  (highlight-phrase                 xog-pplus-regex  'xog-hi-color-B06)
  (message "  xog: colorize complete"))

;; ---------------------------------
;;  general utilities
;; ---------------------------------

(defun xog-string-trim (string)
  "Trim STRING from both ends.

Unlike the standard version, this function returns an empty string if supplied a 'nil'."
  ;; [:space:] comprises space, carriage-return, newline, tab, vertical-tab, form-feed
  (let ((regex "^[[:space:]]*\\(.*?\\)[[:space:]]*$")) ; CAUTION: non-greedy '*?' required
    (if (not (stringp string))
        ""                                   ; else could be () to return 'nil'
      (if (string-match regex string)
          (match-string 1 string)            ; 1 indicates first \( \) construct
        string))))                           ; no match, return original

(defun xog-squeeze-lines ()
  "Automatically squeeze all multiple strictly-blank lines in current buffer."
  (interactive)
  (save-excursion                            ; hold cursor position
    (let ((sections   0  )                   ; sections treated
          (lines      0  )                   ; lines processed
          (sect-plural "s" )                 ; "section" pluralization
          (line-plural "s" ))                ; "line" pluralization
      (setq lines (count-lines (point-min) (point-max)))              ; original line count
      (goto-char (point-min))                ; start at beginning
      (while (re-search-forward              ; look for matches
              "\n\n\n+"
              nil t)                         ; buffer position bound, return 'nil' not error on fail
        (replace-match
         "\n\n"
         t t)                                ; do not alter case; insert literally
        (setq sections (+ 1 sections)))
      (goto-char (point-min))                ; start at beginning again
      (when (looking-at "\n\n")              ; look for special match
        (kill-line)
        (setq sections (+ 1 sections)))
      (setq lines (- lines (count-lines (point-min) (point-max))))    ; original minus final line count
      (if (= sections 1) (setq sect-plural ""))
      (if (= lines    1) (setq line-plural ""))
      (message "  xog: squeeze-lines complete, processed %d section%s totaling %d line%s."
               sections sect-plural lines line-plural)) ))

;; ---------------------------------
;;  obtain xem stub
;; ---------------------------------

;; this function is not currently in use, see also 'xog-write-xog'

(defun xog-get-xem-stub ()
  "Obtain xem file stub.

Assumes that the public `xem-run-me' command has indirectly set the
variable `xem-file-name' after a call `xem-run-me-call'."
  (interactive)
  (let* ((xog-ext xog-extension                         )
         (xem-full  xem-file-name                       )
         (xem-leaf  (file-name-nondirectory xem-full)   )
         (xem-stub  (file-name-sans-extension xem-leaf) ))
    (message "  xog: get-xem-stub: '%s'" xem-stub)
    xem-stub ))                              ; expose return value

;; ---------------------------------
;;  provide
;; ---------------------------------

;; put the mode symbol into the list "features", so that users can
;; invoke (require 'xog-mode) and load your code only when needed

(provide 'xog-mode)

;;; xog.el ends here

;  $Id: xog.el 9029 2012-02-14 13:21:09Z robbie $
;  end of file

