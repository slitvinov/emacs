(defun li/maxima-keys0 ()
  (li/define-key "C-c C-v" 'maxima-send-form)
  (li/define-key "C-c C-p" 'maxima-send-previous-form)
  (li/define-key "C-c C-z" 'maxima-display-buffer)
  (li/define-key "C-c C-c" 'li/maxima-send-line)
  (li/define-key "C-c C-e" 'li/maxima-to-here)
  (li/define-key "C-c C-t" 'li/maxima-test)
  (li/define-key "TAB"     'maxima-complete))

(defun li/inferior-maxima-keys0 ()
  (li/define-key "TAB"     'maxima-complete))

(defun li/maxima-send-line ()
  "Send line and move"
  (interactive)
  (maxima-send-line)
  (next-line))

(defun li/maxima-to-here (&optional arg)
  "Execute to current point in buffer"
  (interactive)
  (maxima-send-region (point-min) (point) arg))

(defun li/maxima-test ()
  "Run current buffer as a test"
  (interactive)
  (setq f (buffer-file-name))
  (setq f (expand-file-name f))
  (maxima-string (concat "batch(\"" f "\", \'test);"))
  (maxima-display-buffer))

(defun li/maxima-keys ()
  "Sets keys in maxima-mode"
  (let ((MAP maxima-mode-map)) (li/maxima-keys0)))

(add-hook 'maxima-mode-hook 'li/maxima-keys)

(defun li/inferior-maxima-keys ()
  "Sets keys in maxima-mode"
  (let ((MAP inferior-maxima-mode-map)) (li/inferior-maxima-keys0)))

(add-hook 'inferior-maxima-mode-hook 'li/inferior-maxima-keys)
