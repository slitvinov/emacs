(defun li/python-keys0 ()
  (li/define-key "C-c   :" 'li/python-set-magic)
  (li/define-key "C-c C-k" 'li/python-stop)
  (li/define-key "C-c C-r" 'li/python-send-region)
  (li/define-key "C-c C-b" 'li/python-send-buffer)
  (li/define-key "C-c C-c" 'li/python-send-line))

(defun li/run-python ()
  (get-buffer-process
   (python-shell-make-comint
    (python-shell-calculate-command)
    (python-shell-get-process-name nil) nil)))

(defun li/python-send-buffer ()
  (interactive)
  (li/run-python)
  (li/python-send-region (point-min) (point-max)))

(defun li/python-dispay-buffer ()
  (interactive)
  (let ((org (current-buffer)))
    (unless (processp (python-shell-get-process))
      (li/run-python))
    (pop-to-buffer (process-buffer (python-shell-get-process)))
    (goto-char (point-max))
    (pop-to-buffer org)))

(defun li/python-send-line ()
   (interactive)
   (unless (processp (python-shell-get-process))
     (li/run-python))
   (let ((b (line-beginning-position))
         (e (line-end-position)))
     (li/python-send-region b e))
   (next-line))

(defun li/python-send-region (beg end)
  (interactive "r\n")
  (python-shell-send-region beg end)
  (li/python-dispay-buffer))

(defun li/python-set-magic ()
  (interactive)
  (executable-set-magic python-shell-interpreter))

(defun li/python-stop ()
  (interactive)
  (let ((proc (python-shell-get-process)))
    (when proc
      (kill-buffer (python-shell-get-buffer))
      (delete-process proc))))

(defun li/python-keys ()
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
