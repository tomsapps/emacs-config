;;;;
;; Helm-Dash
;;;;

(require 'helm-dash)

;; use eww to browse docsets
(setq helm-dash-browser-func 'eww)

;; common docsets

;; fancy mode appropriate local docsets

;; TODO fix this
;; (defun ruby-doc ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("Ruby")))

(add-hook 'ruby-mode-hook '(lambda () (setq-local helm-dash-docsets '("Ruby" "Ruby on Rails"))))
(add-hook 'clojure-mode-hook '(lambda () (setq-local helm-dash-docsets '("Clojure"))))

