;; line moving
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)

;; number windows for organization
;; (require 'window-number)
;; (window-number-mode)

;; rebind meta to command key
;; (setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

;; set these to something better though
; (global-set-key [triple-wheel-left] 'previous-buffer)
; (global-set-key [triple-wheel-right] 'next-buffer)
