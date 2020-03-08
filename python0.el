(defun li/python-keys0 ()
  (li/define-key "C-c   :" 'li/python-set-magic)
  (li/define-key "C-c C-k" 'li/python-stop)
  (li/define-key "C-c C-b" 'li/python-send-buffer)
  (li/define-key "C-c C-c" 'li/python-send-line))

(defun li/python-send-buffer ()
  "Send the buffer to the Python process."
  (interactive)
  (run-python)
  (li/python-send-region (point-min) (point-max)))

(defun li/python-dispay-buffer ()
  "Display the inferior-python-process buffer so the recent output is visible"
  (interactive)
  (let ((org (current-buffer)))
    (unless (processp (python-shell-get-process))
      (run-python))
    (pop-to-buffer (process-buffer (python-shell-get-process)))
    (goto-char (point-max))
    (pop-to-buffer org)))

(defun li/python-send-line ()
   "Send the line to the inferior Python process."
   (interactive)
   (unless (processp (python-shell-get-process))
     (run-python))
   (let ((b (line-beginning-position))
         (e (line-end-position)))
     (li/python-send-region b e))
   (next-line))

(defun li/python-send-region (beg end)
  (interactive "r\n")
  (python-shell-send-region beg end)
  (li/python-dispay-buffer))

(defun li/python-set-magic ()
  "Adds shebang and make script executable"
  (interactive)
  (executable-set-magic "python"))

(defun li/python-stop ()
  "Kill the currently running python process"
  (interactive)
  (let ((proc (python-shell-get-process)))
    (when proc
      (kill-buffer (python-shell-get-buffer))
      (delete-process proc))))

(defun li/python-keys ()
  "Sets keys in python-mode"
  (let ((MAP python-mode-map))
    (li/python-keys0)))

(defun li/python-variables ()
  (setq python-shell-interpreter "python3"))

(add-hook 'python-mode-hook 'li/python-keys)
(add-hook 'python-mode-hook 'li/python-variables)
          
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))
