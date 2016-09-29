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

;; less janky mouse scrolling

(use-package smooth-scroll
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 1))

;; rebind meta to command key
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; set these to something better though
; (global-set-key [triple-wheel-left] 'previous-buffer)
; (global-set-key [triple-wheel-right] 'next-buffer)
