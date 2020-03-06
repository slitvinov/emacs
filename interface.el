(defun li/primary ()
  "yank from primary PRIMARY selection"
  (interactive)
  (insert
   (gui-get-primary-selection)))
(global-set-key (kbd "C-c C-y") 'li/primary)
