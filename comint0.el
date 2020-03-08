(defun li/make-comint-in-buffer (name buffer program &rest args)
  (setq buffer (get-buffer-create (or buffer (concat "*" name "*"))))
  (unless (comint-check-proc buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p 'comint-mode)
	(comint-mode)))
    (comint-exec buffer name program nil args))
  buffer)

;;(li/make-comint-in-buffer "Python" "*Python*" "sh" "-c"
;;			  "python -i /u/q.py 'a b' c < /u/q.py")
