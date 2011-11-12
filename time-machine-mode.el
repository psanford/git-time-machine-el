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
