(message "started loading %s" (format-time-string "%H:%M:%S"))

(require 'cl)

(server-start)

(if (fboundp 'menu-bar-mode) (menu-bar-mode 't))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 'nil))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 'nil))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; separate customization file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; set some basic preferences
(setq color-theme-is-global t)
(setq echo-keystrokes 0.1)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq fill-column 125)
(setq font-lock-maximum-decoration t)
(setq indent-tabs-mode nil) 
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq mouse-yank-at-point t)
(setq require-final-newline nil)
(setq query-replace-highlight t)
(setq search-highlight t)
(setq shift-select-mode nil)
(setq split-width-threshold nil)
(setq standard-indent 2)
(setq tab-width 2)
(setq transient-mark-mode t)
(setq truncate-partial-width-windows nil)
(setq uniquify-buffer-name-style 'forward)
(setq ring-bell-function 'ignore)
(setq whitespace-line-column 100)
(setq whitespace-style '(trailing lines space-before-tab indentation space-after-tab))
(setq xterm-mouse-mode t)

;; display time
(display-time-mode t)

;; stop the accidental suspends
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; map call-last-kbd-macro to a single key
(global-set-key [f5] 'call-last-kbd-macro)

;; switch between two buffers easier
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-x C-x") 'switch-to-previous-buffer)

;; kill emacs daemon
(global-set-key (kbd "C-<f12>") 'kill-emacs)

;; save editor state on exit
(desktop-save-mode t)

;; make select/paste work in natural way
(delete-selection-mode t)

;; global syntax highlight
(global-font-lock-mode t)

;; answer y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; highlite parentheses
(show-paren-mode t)

;; allow ANSI colors in emacs terminal
(ansi-color-for-comint-mode-on)

;; name buffers better
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; enable enhanced directory browsing
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching)

;; enable enhanced buffer switching
(require 'edmacro)
(icomplete-mode 99)

(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; buffer switching
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c C-c") 'switch-to-previous-buffer)

;; fullscreen 
(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_ABOVE" 0)))

(global-set-key [f11] 'fullscreen)
;(fullscreen)

;; default window size & position
(add-to-list 'default-frame-alist '(width . 240))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(top . 40))
(add-to-list 'default-frame-alist '(left . 125))

;; color scheme
(set-cursor-color "red")
(set-foreground-color "green")
(set-background-color "black")

;; add local plugin directories to load path
(add-to-list 'load-path "~/.emacs.d/plugins")
(mapcar (lambda (d) (add-to-list 'load-path d)) (directory-files "~/.emacs.d/plugins" t))

;; initialize package system
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(message "done with general setup %s" (format-time-string "%H:%M:%S"))

;; Emacs Lisp Mode
(require 'highlight-parentheses)
(add-hook 'emacs-lisp-mode-hook '(lambda () (highlight-parentheses-mode)))

;; Git for Emacs
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-status-buffer-switch-function 'switch-to-buffer)
(global-set-key (kbd "C-x C-v") 'magit-status)

;; YAML support 
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; CSS support 
(require 'css-mode)

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode)

;; javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; python support
(elpy-enable)
(setq elpy-rpc-backend "jedi")

;; pony mode
(require 'pony-mode)

;; setup multi-term
(require 'multi-term)
(global-set-key [f1] 'multi-term)
(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)
	    (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)))

(message "finished loading %s" (format-time-string "%H:%M:%S"))
