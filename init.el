(message "started loading %s" (format-time-string "%H:%M:%S"))

(require 'cl)

(server-start)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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
(setq visible-bell t)
(setq whitespace-line-column 100)
(setq whitespace-style '(trailing lines space-before-tab indentation space-after-tab))
(setq xterm-mouse-mode t)

;; display time
(display-time-mode 't)

;; map call-last-kbd-macro to a single key
(global-set-key [f5] 'call-last-kbd-macro)

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
(iswitchb-mode t)

(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; fullscreen 
(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_ABOVE" 0)))

(global-set-key [f11] 'fullscreen)
(fullscreen)

;; default window size & position
;(add-to-list 'default-frame-alist '(width . 149))
;(add-to-list 'default-frame-alist '(height . 48))
;(add-to-list 'default-frame-alist '(top . 43))
;(add-to-list 'default-frame-alist '(left . 105))

;; color scheme
(set-cursor-color "red")
(set-foreground-color "green")
(set-background-color "black")

;; add local plugin directories to load path
(mapcar (lambda (d) (add-to-list 'load-path d)) (directory-files "~/.emacs.d/plugins" t))

;; Gist for GitHub
(require 'gist)

;; Emacs Got Git
(require 'magit)

;; rainbow parentheses
(require 'highlight-parentheses)
(setq hl-paren-colors
      '("red1" "orange1" "greenyellow" "green1" "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

;; ERC configurations
(require 'erc)
(setq erc-nick "MadWombat")
(setq erc-user-full-name "Dmitriy Kropivnitskiy")
(setq erc-email-userid "nigde@mitechki.net")
(setq erc-password "whatever23")
(setq erc-port 6667)
(setq erc-server "irc.freenode.net")
(setq erc-quit-reason (lambda (x) "The rest is silence..."))

(add-hook 'erc-text-matched-hook
	  (lambda (match-type nickuserhost message)
	    (cond
	     ((eq match-type 'current-nick)
	      (emms-play-file "/usr/share/sounds/gnome/default/alerts/glass.ogg"))
	     ((eq match-type 'keyword)
	      (emms-play-file "/usr/share/sounds/gnome/default/alerts/glass.ogg")))))

(defun irc ()
  "Connect to IRC"
  (interactive)
  (erc :server erc-server 
       :port erc-port
       :nick erc-nick 
       :password erc-password 
       :full-name erc-user-full-name))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook '(lambda () (highlight-parentheses-mode)))

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; javascript
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(add-hook 'js-mode-hook 
	  '(lambda ()
	     (setq js-indent-level 2)))

;; nXhtml
(load "nxhtml/autostart.el")
(setq mumamo-background-colors nil) 
(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode))

;; python support
(require 'python)
(add-hook 'python-mode-hook 
	  '(lambda () 
	     (set-variable 'py-indent-offset 4)
	     (set-variable 'indent-tabs-mode nil)
	     (define-key python-mode-map "\C-m" 'newline-and-indent)
	     (add-to-list 'ac-sources 'ac-source-ropemacs)))

(require 'pymacs)

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
(setq ropemacs-guess-project t)

; flymake
(when (load "flymake" t)
  ; add pyflakes to check python files
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pyflakes-init))
  ; disable checking of html files (doesn't work for django templates)
  (delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(message "finished loading %s" (format-time-string "%H:%M:%S"))