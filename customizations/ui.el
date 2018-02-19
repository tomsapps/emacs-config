;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Display time in modeline
(display-time-mode 1)

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Show line numbers
(global-linum-mode)

;; Show column numbers
(column-number-mode 1)

;; Start in mac os x full-screen mode
;; (toggle-frame-fullscreen)

;; Set winner-mode (C-c, left and C-c, right to undo/redo window actions)
(winner-mode 1)

(nyan-mode 1)

;; Use windmove package (shift+arrows to move around windows)
;; (windmove-default-keybindings)

;; Piggyback framemove with windmove
(require 'framemove)
(setq framemove-hook-into-windmove t)

;; Map previous-multiframe-window to s+" (matches next s+')
(global-set-key (kbd "s-\"") 'previous-multiframe-window)

;; Use buffers in other frames if they exist
;; (only need this if using multiple monitors in my workflow)
(setq display-buffer-reuse-frames t)

;; You can uncomment this to remove the graphical toolbar at the top. After
;; awhile, you won't need the toolbar.
;; (when (fboundp 'tool-bar-mode)
(tool-bar-mode -1)

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;; (load-theme 'tomorrow-night-eighties-MINE t)
(load-theme 'atom-one-dark t)

;; increase font size for better readability
(set-face-attribute 'default nil :height 140)
;; (set-face-attribute 'default nil :height 200)
;; (set-face-attribute 'default nil :height 300) ; presentation-mode

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
(setq initial-frame-alist '((top . 0) (left . 0) (width . 90) (height . 35)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; set C-O (that's capital 'o') to go to previous window
;; reverse of C-o
(defun other-window-back()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x O") 'other-window-back)
