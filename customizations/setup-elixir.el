(require 'smartparens)
(require 'alchemist)
(autoload 'alchemist "alchemist" "Minor mode for elixir files" t)
;; (add-to-list 'auto-mode-alist '("\\.exs$" . alchemist))
;; (add-to-list 'auto-mode-alist '("\\.ex$" . alchemist))
;; (add-to-list 'auto-mode-alist '("\\.elixir$" . alchemist))
;; (add-to-list 'auto-mode-alist '("\\.eex$" . alchemist))

(setq alchemist-project-compile-when-needed t)

;; allow goto definition in Erlang and Elxir source files
;; (setq alchemist-goto-erlang-source-dir "/usr/local/bin/elixir")
;; (setq alchemist-goto-elixir-source-dir "/usr/local/bin/erlang")

(defun custom-elixir-mode-hook ()
 (yas/minor-mode +1)
 (smartparens-mode +1))
 ;; (tester-init-test-run #'alchemist-mix-test-file "_test.exs$")
 ;; (tester-init-test-suite-run #'alchemist-mix-test))

(defun my-elixir-do-end-close-action (id action context)
 (when (eq action 'insert)
   (newline-and-indent)
   (forward-line -1)
   (indent-according-to-mode)))

(sp-with-modes '(elixir-mode)
 (sp-local-pair "->" "end"
                :when '(("RET"))
                :post-handlers '(:add my-elixir-do-end-close-action)
                :actions '(insert)))

(sp-with-modes '(elixir-mode)
 (sp-local-pair "do" "end"
                :when '(("SPC" "RET"))
                :post-handlers '(:add my-elixir-do-end-close-action)
                :actions '(insert)))

(defun custom-erlang-mode-hook ()
 (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

;; (defun custom-alchemist-custom-keybindings ()
 ;; (define-key alchemist-mode-map (kbd "M-w") 'alchemist-goto-list-symbol-definitions))

(defun alchemist-my-iex-keys ()
 (define-key alchemist-iex-mode-map (kbd "C-d") 'windmove-right))

(add-hook 'alchemist-iex-mode-hook 'alchemist-my-iex-keys)
;; (add-hook 'alchemist-mode-hook 'custom-alchemist-custom-keybindings)
(add-hook 'elixir-mode-hook  'custom-elixir-mode-hook)
(add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)

(with-eval-after-load 'popwin
 (add-to-list 'popwin:special-display-config `"​*alchemist test report*​"))
