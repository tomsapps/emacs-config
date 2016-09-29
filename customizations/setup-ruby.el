;; smartparens setup shit
; (require 'smartparens-config)
; (require 'smartparens-ruby)
; (smartparens-global-mode)
; (show-smartparens-global-mode t)
; (sp-with-modes '(rhtml-mode)
;   (sp-local-pair "<" ">")
;   (sp-local-pair ""))

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'electric-pair-mode)