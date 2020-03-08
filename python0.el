(defun li/python-keys0 ()
  (li/define-key "C-c   :" 'li/python-set-magic)
  (li/define-key "C-c C-k" 'li/python-stop)
  (li/define-key "C-c C-r" 'li/python-send-region)
  (li/define-key "C-c C-b" 'li/python-send-buffer)
  (li/define-key "C-c C-z" 'li/python-dispay-buffer)
  (li/define-key "C-c C-c" 'li/python-send-line))

(defun li/python-shell-send-string (string process)
  (if (string-match ".\n+." string)
      (let* ((temp-file-name (python-shell--save-temp-file string))
	     (file-name (or (buffer-file-name) temp-file-name)))
	(li/python-shell-send-file file-name process temp-file-name t))
    (comint-send-string process string)
    (when (or (not (string-match "\n\\'" string))
	      (string-match "\n[ \t].*\n?\\'" string))
      (comint-send-string process "\n"))))

(defun li/python-shell-make-comint (cmd proc-name)
  (save-excursion
    (python-shell-with-environment
      (let* ((buffer-name
              (format "*%s*" proc-name)))
        (when (not (comint-check-proc buffer-name))
          (let* ((cmdlist (split-string-and-unquote cmd))
                 (interpreter (car cmdlist))
                 (args (cdr cmdlist))
                 (buffer (apply #'make-comint-in-buffer proc-name buffer-name
                                interpreter nil args))
                 (python-shell--parent-buffer (current-buffer))
                 (process (get-buffer-process buffer))
                 (python-shell--interpreter interpreter)
                 (python-shell--interpreter-args
                  (mapconcat #'identity args " ")))
            (with-current-buffer buffer
              (inferior-python-mode))
            (and nil (set-process-query-on-exit-flag process nil))))
        buffer-name))))

(defun li/python-send-region (start end)
  (interactive "r\n")
  (let* ((string (python-shell-buffer-substring start end t))
         (process (li/python-shell-get-process)))
    (li/python-shell-send-string string process))
  (li/python-dispay-buffer))

(defun li/python-shell-get-buffer ()
  (if (derived-mode-p 'inferior-python-mode)
      (current-buffer)
    (let* ((name  python-shell-buffer-name)
           (buffer-name (format "*%s*" name))
           (running (comint-check-proc buffer-name)))
      (and running buffer-name))))

(defun li/python-shell-get-process ()
  (get-buffer-process (li/python-shell-get-buffer)))

(defun li/python-shell-calculate-command ()
  (format "%s %s"
	  (combine-and-quote-strings (list python-shell-interpreter))
	  python-shell-interpreter-args))

(defun li/run-python ()
  (get-buffer-process
   (li/python-shell-make-comint (li/python-shell-calculate-command)
				python-shell-buffer-name)))

(defun li/python-send-buffer ()
  (interactive)
  (li/run-python)
  (li/python-send-region (point-min) (point-max)))

(defun li/python-dispay-buffer ()
  (interactive)
  (let ((org (current-buffer)))
    (unless (processp (li/python-shell-get-process))
      (li/run-python))
    (pop-to-buffer (process-buffer (li/python-shell-get-process)))
    (goto-char (point-max))
    (pop-to-buffer org)))

(defun li/python-send-line ()
   (interactive)
   (unless (processp (li/python-shell-get-process))
     (li/run-python))
   (let ((b (line-beginning-position))
         (e (line-end-position)))
     (li/python-send-region b e))
   (next-line))

(defun li/python-set-magic ()
  (interactive)
  (executable-set-magic python-shell-interpreter))

(defun li/python-stop ()
  (interactive)
  (let ((proc (li/python-shell-get-process)))
    (when proc
      (kill-buffer (li/python-shell-get-buffer))
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
