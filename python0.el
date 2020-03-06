(defun li/python-keys0 ()
  (li/define-key "C-c   :" 'li/python-set-magic)
  (li/define-key "C-c C-k" 'li/python-stop)
  (li/define-key "C-c C-b" 'li/python-send-buffer)
  (li/define-key "C-c C-c" 'li/python-send-line))

(defun li/python-line-beginning-position ()
  (if (not (fboundp 'line-beginning-position))
      (save-excursion
	(beginning-of-line)
	(point))
    (line-beginning-position)))

(defun li/python-line-end-position ()
  (if (not (fboundp 'line-end-position))
      (save-excursion
	(end-of-line)
	(point))
    (line-end-position)))

(defun li/python-send-buffer (&optional arg)
  "Send the buffer to the Python process."
  (interactive "P")
  (run-python)
  (python-shell-send-region (point-min) (point-max)))

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
   (let ((b (li/python-line-beginning-position))
         (e (li/python-line-end-position)))
     (python-shell-send-region b e))
   (li/python-dispay-buffer)
   (next-line))

(defun li/python-set-magic ()
  "Adds shebang and make script executable"
  (interactive)
  (executable-set-magic "python"))

(defun li/line-beginning-position ()
  (if (not (fboundp 'line-beginning-position))
      (save-excursion
	(beginning-of-line)
	(point))
    (line-beginning-position)))

(defun li/line-end-position ()
  (if (not (fboundp 'line-end-position))
      (save-excursion
	(end-of-line)
	(point))
    (line-end-position)))

(defun li/python-stop (&optional arg)
  "Kill the currently running python process"
  (interactive "P")
  (if (processp (python-shell-get-process))
      (progn 
        (delete-process (python-shell-get-process))
        (kill-buffer "*Python*"))))

(defun li/python-keys ()
  "Sets keys in python-mode"
  (let ((MAP python-mode-map)) (li/python-keys0)))

(add-hook 'python-mode-hook 'li/python-keys)
          
(defun ps (&optional arg)
  (interactive "P")
  (ps0)
  (li/python-set-magic)
  (python-mode))

(define-skeleton ps0
  "Insert python codeforces skeleton"
  ""
  "from collections import deque\n"
  "\n"
  "def ir(): return int(raw_input())\n"
  "def ia(): return map(int, raw_input().split())\n"
  "\n")

(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))
