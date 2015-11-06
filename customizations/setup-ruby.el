;;;;
;; Ruby
;;;;

;; rbenv
(require 'rbenv)
(setq rbenv-show-active-ruby-in-modeline nil)
(global-rbenv-mode)


;; rspec-mode
(require 'rspec-mode)
(setq compilation-scroll-output t)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)
