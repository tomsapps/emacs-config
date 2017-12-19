(defun api-shell ()
  "open a shell in api monorepo and source correct env vars"
  (let* ((api-dir "/Users/andrewmelis/projects/api")
         (env-file ".envrc")
         (api-shell-buffer-name "*shell*api"))
    (let ((message-log-max nil))
      (shell-command (concat "cd" api-dir) api-shell-buffer-name))
    (switch-to-buffer-other-window api-shell-buffer-name)
    (linum-mode -1)))
