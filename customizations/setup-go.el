(setq exec-path (cons "/usr/local/bin" exec-path))
(add-to-list 'exec-path "/Users/andrewmelis/dev/personal/go")

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'go-eldoc-setup)

(global-subword-mode 1)

(defun my-godoc (query)
  "go-mode `godoc` command seems to display an abridged version
   of the docs from `go doc` rather than `godoc`"
  (interactive "sgodoc: ")
  (let* ((godoc "godoc ")
         (formatted-godoc (concat "*" godoc "*"))
         (godoc-buffer-name (concat "*" godoc query "*")))
    (let ((message-log-max nil))
      (shell-command (concat godoc query) godoc-buffer-name))
    (switch-to-buffer-other-window godoc-buffer-name)
    (godoc-mode)
    (view-mode)
    (linum-mode -1)))
