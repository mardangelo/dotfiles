;; No startup screen
(setq inhibit-startup-message t)

;; Remove UI elements
(menu-bar-mode -1) ; toggle menu bar per frame
(tooltip-mode -1) ; show help text in echo area, not pop up window

;; ===============
;;    truecolor
;; ===============

(unless (display-graphic-p)
  (setq tty-color-mode 'direct)
  (setq frame-use-direct-color t)
  (setq tty-max-colors 16777216))

(defun true-color-test ()
  "Display a gradient of colors to test true color support."
  (interactive)
  (let ((buffer (get-buffer-create "*true-color-test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (dotimes (r 6)
        (dotimes (g 6)
          (dotimes (b 6)
            (let ((color (format "#%02x%02x%02x" 
                               (* r 51) (* g 51) (* b 51))))
              (insert (propertize "■ " 'face 
                                `(:background ,color :foreground ,color))))))
          (insert "\n")))
    (switch-to-buffer buffer)))

;; Line and column numbers, but not in all modes
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Word wrap in org mode
(add-hook 'org-mode-hook #'visual-line-mode)

;; Set default width of fill mode
(setq-default fill-column 100)

;; ======================
;; Set up package sources
;; ======================

(require 'package)

;; Nice macro for updating lists in place.
(defmacro append-to-list (target suffix)
  "Append SUFFIX to TARGET in place."
  `(setq ,target (append ,target ,suffix)))

;; Set up emacs package archives with 'package
(append-to-list package-archives
                '(("melpa" . "http://melpa.org/packages/") ;; Main package archive
                  ("melpa-stable" . "http://stable.melpa.org/packages/"))) ;; Some packages might only do stable releases?

(package-initialize)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package, a macro for importing and installing packages. Also, refresh the package archive on load so we can pull the latest packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq
 use-package-always-ensure t ;; Makes sure to download new packages if they aren't already downloaded
 use-package-verbose t) ;; Package install logging. Packages break, it's nice to know why.

;; ============
;;   Packages
;; ============

;; Better terminal emulator
(use-package vterm
  :config
  (setq-default vterm-exit-functions #'kill-buffer))

;; Organization tool
(use-package org
  :pin gnu)

;; Highlight hex, rgb colour codes
(use-package rainbow-mode
  :init
  (rainbow-mode t))

;; Better delimiter highlighting for elisp
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Enable which-key to show available keybindings when you start a key sequence
(use-package which-key
  :init
  (which-key-mode 1) ; enable minor mode
  :config
  (setq which-key-idle-delay 0.4 ; delay before popup appears
	which-key-popup-type 'side-window ; side window enables paging
	which-key-side-window-location 'bottom
	which-key-max-description-length 40
	which-key-max-display-columns nil ; unlimited columns
	which-key-sort-order 'which-key-key-order-alpha ; sort keys alphabetically
	which-key-show-prefix 'echo ; display current prefix sequence
	which-key-dont-use-unicode nil ; use unicode
	which-key-unicode-correction 3)
  )

;; Better keybinding config
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer mars/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC" ; not insert mode
			  :global-prefix "M-SPC") ; insert mode
  (mars/leader-keys
     "t" '(:ignore t :which-key "toggles")
     "tt" '(load-theme :which-key "choose theme")))
;; ===========================

;; Hydra - transient key bindings
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(mars/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; ====================

;; Straight up better modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Show command name and key binding for whatever was just executed
(use-package keycast
  :after doom-modeline
  :hook (after-init . keycast-mode)
  :config
  (defun keycast-active-frame-bottom-right-pp ()
    "Predicate to determine if should show keycast in the modeline."
    (eq (selected-frame) (car (visible-frame-list))))
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for use with doom-mode-line)."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  (setq keycast-mode-line-window-predicate 'keycast-active-frame-bottom-right-pp
	keycast-mode-line-remove-tail-elements nil
        keycast-mode-line-insert-after 'doom-modeline-misc-info ; '(:eval (doom-modeline-format--main))
	keycast-mode-line-format "%k%c%r"
	keycast-log-format "%-10K%C%R\n"))

;; Enhanced documentation with examples
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ("C-h x" . helpful-command)
  ("C-c C-d" . helpful-at-point)
  ("C-h F" . helpful-function))

;; ============================
;; Incremental fuzzy completion
;; ============================

;; Vertical interactive completion
(use-package vertico
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Enable TTY child frames if available
(when (featurep 'tty-child-frames)
  ;; Use Unicode box-drawing characters for frame borders
  (standard-display-unicode-special-glyphs)
  ;; Use "floating" child frame for vertico completions 
  (use-package vertico-posframe
    :after vertico
    :init
    (vertico-posframe-mode)))
    ;; :config
    ;; (setq vertico-posframe-parameters
    ;; 	  '((left-fringe . 8)
    ;; 	    (right-fringe . 8)
    ;; 	    (undecorated . nil)))))

;; Persist history across emacs restarts
(use-package savehist
  :init
  (savehist-mode))

;; Minibuffer configuration for within emacs
(use-package emacs
  :custom
  ; minibuffers inside minibuffers
  (enable-recursive-minibuffers t) 
  ; hide commands that don't work in the current mode (vertico benefits)
  (read-extended-command-predicate #'command-completion-default-include-p)
  ; no cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))) ;

;; Orderless Completion Style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Search and navigation based on emacs completing function, can quickly select an item from a list of candidates
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map`
	 ("C-c M-x" . consult-mode-command)
	 ("C-c h" . consult-history)
	 ("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map`
	 ("C-x M-:" . consult-complex-command)
	 ("C-x b" . consult-buffer)
	 ("C-x 4 b" . consult-buffer-other-window)
	 ("C-x 5 b" . consult-buffer-other-frame)
	 ("C-x t b" . consult-buffer-other-tab)
	 ("C-x r b" . consult-bookmark)
	 ("C-x p b" . consult-project-buffer)
	 ;; Custom M-# bindings for fast register
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)
	 ;; M-g bindings in `goto-map`
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)
	 ("M-g g" . consult-goto-line)
	 ("M-g M-g" . consult-goto-line)
	 ("M-g o" . consult-outline)
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map`
	 ("M-s d" . consult-find)
	 ("M-s c" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)
	 ("M-s e" . consult-isearch-history)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)
	 ("M-r" . consult-history))
  ;; Enable automatic preview at point in the *Completions* buffer. Relevant for default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Tweak register preview for `consult-register-load`, `consult-register-store` and the built-in commands. Improves register formatting, adds thin separator lines, register sorting, and hides the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  ;; Configure preview. The default value is 'any, such that any key triggers preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key `M-.`
   :preview-key '(:debounce 0.4 any))

  ;; Configure the narrowing key.
  (setq consult-narrow-key "<") ;; `C-+` works equally well

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

;; Enable rich minibuffer annotations
(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)
         :map completion-list-mode-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; ==============
;;     Themes
;; ==============

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-visual-bell-config) ; flash mode-line on error
  (doom-themes-org-config) ; better compatibility with org mode
  (load-theme 'doom-tokyo-night t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
