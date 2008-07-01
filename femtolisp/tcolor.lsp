; color for performance

(load "color.lsp")

; 100x color 5 queens
(setq Q (generate-5x5-pairs))
(defun ct ()
  (setq C (color-pairs Q '(a b c d e)))
  (dotimes (n 99) (color-pairs Q '(a b c d e))))
(time (ct))
(print C)
