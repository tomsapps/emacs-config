(setq exec-path (cons "/usr/local/bin" exec-path))
(add-to-list 'exec-path "/Users/andrewmelis/dev/personal/go")

(add-hook 'before-save-hook 'gofmt-before-save)
;; (add-hook 'go-mode-hook 'go-eldoc-setup)

