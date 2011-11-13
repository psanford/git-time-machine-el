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
;;
;; Commentary:
;; Git Time Machine allows you to easily go backward or forward
;; in a file's history.

(defvar git-time-machine-current-position nil)
(make-variable-buffer-local 'git-time-machine-current-position)

(defvar git-time-machine-filename nil)
(make-variable-buffer-local 'git-time-machine-filename)

(defvar git-time-machine-buffer-name nil)
(make-variable-buffer-local 'git-time-machine-buffer-name)

;;;###autoload
(defun git-time-machine-diff-backwards ()
  "Show the previous (older) commited diff to the current file"
  (interactive)
  (let ((filename) (current-position) (buffername))
    (if git-time-machine-filename
        (progn
          (setq
           filename git-time-machine-filename
           buffername git-time-machine-buffer-name
           current-position git-time-machine-current-position))
      (setq
       filename (buffer-file-name)
       buffername (buffer-name)
       current-position 0))
    (git-time-machine-create-buffer filename current-position buffername)))

;;;###autoload
(defun git-time-machine-diff-forwards ()
  "Show the next (newer) commited diff to the current file"
  (interactive)
  (let ((first-rev) (last-rev) (filename) (current-position) (buffername))
    (if git-time-machine-filename
        (progn
          (setq
           filename git-time-machine-filename
           buffername git-time-machine-buffer-name
           current-position (- git-time-machine-current-position 2)))
      (error "Cannot time machine forwards, not a time machine buffer"))
    (if (< current-position 0)
        (find-file filename)
      (git-time-machine-create-buffer filename current-position buffername))))

(defun git-time-machine-create-buffer (filename current-position buffername)
  (let (first-rev last-rev)
    (with-temp-buffer
      (call-process "/usr/bin/git" nil t nil
                    "log" "--pretty=format:%h"
                    "-n" "2" (format "--skip=%d" current-position) filename)
      (beginning-of-buffer)
      (setq last-rev (buffer-substring (point) (line-end-position)))
      (next-line)
      (setq first-rev (buffer-substring (point) (line-end-position))))
    (switch-to-buffer (get-buffer-create
                       (format "*timemachine %s*" buffername)))
    (setq buffer-read-only nil)
    (erase-buffer)
    (call-process "/usr/bin/git" nil t nil
                  "log" (format "%s..%s" first-rev last-rev) "--" filename)
    (call-process "/usr/bin/git" nil t nil
                  "diff" first-rev last-rev filename)
    (beginning-of-buffer)
    (diff-mode)
    (setq buffer-read-only t)
    (setq git-time-machine-current-position (+ 1 current-position)
          git-time-machine-buffer-name buffername
          git-time-machine-filename filename)))

(provide 'git-time-machine)
