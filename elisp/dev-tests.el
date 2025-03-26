;;; dev-tests.el --- development tests (omitted from 'pdfall.sh')

;  file-purpose     : development tests (omitted from 'pdfall.sh')
;  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
;  file-create-date : Sat 21-Aug-2010 19:11 UTC
;  file-status      : ongoing
;  file-keywords    : emacs xeona

;  $Revision: 5160 $
;  $Date: 2010-08-23 20:29:01 +0200 (Mon, 23 Aug 2010) $
;  $Author: robbie $
;  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/elisp/dev-tests.el $

;; ---------------------------------
;;  xem tests
;; ---------------------------------

(defalias 'xem-1 'xem-system-info-test)

(defun xem-system-info-test ()
  "Test interface to `xem-system-info'."
  (interactive)
  (let* ()
    (xem-system-info)
    (message "  xem: system-info-test: svn %s" (get 'xem-sysinf 'svnver))
    (sit-for 2)
    (message "  xem: system-info-test:: complete") ))

(defalias 'xem-2 'xem-file-modified-test)

(defun xem-file-modified-test ()
  "Test wrapper for function `xem-file-modified'."
  (interactive)
  (let ((filename "/home/robbie/.bashrc"))
    (xem-file-modified filename) ))

(defalias 'xem-3 'xem-uncamel-test)

(defun xem-uncamel-test ()
  "Test for `xem-uncamel'."
  (interactive)
  (let ((buffer-name  "*Xem-uncamel-test*" )
        (test         "abcD_fgHij"         )
        (here         nil                  )
        (msg          nil                  ))     ; relayed message
    (switch-to-buffer buffer-name)
    (setq buffer-offer-save nil)                  ; offer to save non-file buffers on exit (no)
    (insert "\n")
    (setq here (point))
    (insert test)
    (insert "\n")
    (goto-char here)
    (forward-char 1)
    (setq msg (xem-uncamel))                      ; capture original output too
    (message "  xem: uncamel-test output: '%s'" msg)))

(defalias 'xem-4 'xem-dump-summarize-test)

(defun xem-dump-summarize-test ()
  "Test interface to `xem-dump-summarize'."
  (interactive)
  (xem-dump-summarize))

(defalias 'xem-5 'xem-time-decomp-test)

(defun xem-time-decomp-test ()
  "Test interface to `xem-time-decomp'."
  (interactive)
  (let* ((datestr "Sat 21-Aug-2010 15:04 UTC +0000"))
    (xem-time-decomp datestr)
    (switch-to-buffer "*Messages*")))

(defalias 'xem-6 'xem-time-delta-test)

(defun xem-time-delta-test ()
  "Test interface to `xem-time-delta'."
  (interactive)
  (let* ()
    (message "  1 ----------")
    (setq datestr "Sat 21-Aug-2010 23:37 UTC +0000")
    (setq datestr "Sat 18-Aug-2010 15:04 UTC +0000")
    (setq datelst (xem-time-decomp datestr))
    (setq delta (xem-time-delta datelst))
    (message "  2 ----------")
    (setq was (xem-current-time-utc))
    (sleep-for 2)
    (xem-time-delta was)
    (switch-to-buffer "*Messages*") ))

;; ---------------------------------
;;  xumber tests
;; ---------------------------------

(defalias 'xumber-1 'xumber-comma-ize-test)

(defun xumber-comma-ize-test (number)
  "Prompted test for `xumber-comma-ize' using NUMBER."
  (interactive "nEnter a long integer or decimal: ")
  (switch-to-buffer "*Messages*")
  (message "%f to %s" number (xumber-comma-ize number)))

;; ---------------------------------
;;  xog tests
;; ---------------------------------

(defalias 'xog-1 'xog-split-repx-test)

(defun xog-split-repx-test ()
  "Test interface for function `xog-split-repx'."
  (interactive)
  (let ((line   )
        (parts  ))
    (message "  xog: split-repx-test: starting")
    (setq line (thing-at-point 'line))       ; contains trailing newline
    (setq parts (xog-split-repx line))
    (if parts
        (progn
          (message "  xog: split-repx-test: okay"))
      (message "  xog: split-repx-test: empty split"))))

;; ---------------------------------
;;  xrstat tests
;; ---------------------------------

(defalias 'xrstat-1 'xrstat-stringify-list-test)

(defun xrstat-stringify-list-test ()
  "Test interface to `xem-stringify-list'."
  (interactive)
  (let* ((mylist (list "abc def" "ghi jkl" "mno pqr" "stu vwx") )
         (temp   (xem-stringify-list mylist)                    ))
    (message "temp: '%s'" temp) ))

;  $Id: dev-tests.el 5160 2010-08-23 18:29:01Z robbie $
;  end of file

