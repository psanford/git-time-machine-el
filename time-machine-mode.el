;;; git-time-machine.el -- Easy file diffs.

;; Copyright (C) 2011 Peter Sanford

;; Author: Peter Sanford <peter AT petersdanceparty.com>
;; Version: 0.1
;; Keywords: tools

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defvar time-machine-current-position nil)
(make-variable-buffer-local 'time-machine-current-position)

(defvar time-machine-filename nil)
(make-variable-buffer-local 'time-machine-filename)

(defun time-machine-git-diff-backwards ()
  (interactive)
  (let ((first-rev) (last-rev) (filename) (current-position))
    (if time-machine-filename
        (progn
          (setq
           filename time-machine-filename
           current-position time-machine-current-position))
      (setq
       filename (buffer-file-name)
       current-position 0))
    (with-temp-buffer
      (message "%s" current-position)
      (call-process "/usr/bin/git" nil t nil
                    "log" "--pretty=format:%h"
                    "-n" "2" (format "--skip=%d" current-position) filename)
      (beginning-of-buffer)
      (setq last-rev (buffer-substring (point) (line-end-position)))
      (next-line)
      (setq first-rev (buffer-substring (point) (line-end-position))))
    (switch-to-buffer (get-buffer-create
                       (format "*vc-diff %s %s*" first-rev last-rev)))
    (call-process "/usr/bin/git" nil t nil
                  "diff" first-rev last-rev filename)
    (beginning-of-buffer)
    (diff-mode)
    (setq time-machine-current-position (+ 1 current-position)
          time-machine-filename filename)))

(defun time-machine-git-diff-forwards ()
  (interactive)
  (let ((first-rev) (last-rev) (filename) (current-position))
    (if time-machine-filename
        (progn
          (setq
           filename time-machine-filename
           current-position (- time-machine-current-position 2)))
      (error "Cannot time machine forwards, not a time machine buffer"))
    (if (< current-position 0)
        (find-file filename)
      (with-temp-buffer
        (message "%s" current-position)
        (call-process "/usr/bin/git" nil t nil
                      "log" "--pretty=format:%h"
                      "-n" "2" (format "--skip=%d" current-position) filename)
        (beginning-of-buffer)
        (setq last-rev (buffer-substring (point) (line-end-position)))
        (next-line)
        (setq first-rev (buffer-substring (point) (line-end-position))))
      (switch-to-buffer (get-buffer-create
                         (format "*vc-diff %s %s*" first-rev last-rev)))
      (erase-buffer)
      (call-process "/usr/bin/git" nil t nil
                    "diff" first-rev last-rev filename)
      (beginning-of-buffer)
      (diff-mode)
      (setq time-machine-current-position (+ 1 current-position)
            time-machine-filename filename))))

(define-minor-mode time-machine-mode
  "Toggle Time Machine mode, globally.
Time Machine mode is an extension to vc mode that allows you
to easily go backward or forward in a file's history."

  ;; Init val
  nil
  "Time Machine"
  )

(provide 'time-machine-mode)
