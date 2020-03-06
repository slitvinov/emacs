(defun li/define-key (keyseq-text binding)
  "A wrpper for define-key"
  (define-key MAP (kbd keyseq-text) binding))
