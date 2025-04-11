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
                '(("melpa" . "https://melpa.org/packages/") ;; Main package archive
		  ("org" . "https://orgmode.org/elpa/")
                  ("melpa-stable" . "https://stable.melpa.org/packages/"))) ;; Some packages might only do stable releases?

(package-initialize)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	(or (bound-and-true-p straight-base-dir)
	    user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package '(dired :type built-in))
(setq package-enable-at-startup nil)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package, a macro for importing and installing packages. Also, refresh the package archive on load so we can pull the latest packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (straight-use-package 'use-package))

(setq straight-use-package-by-default t)

(require 'use-package)
(setq
 use-package-always-ensure t ;; Makes sure to download new packages if they aren't already downloaded
 use-package-verbose t) ;; Package install logging. Packages break, it's nice to know why.

;; ============
;;   Packages
;; ============

;; automatically install, use, fallback for tree-sitter major modes
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

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

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

;; Use sudo to edit files
(use-package sudo-edit)

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

;; =======
;;   git 
;; =======

;; git porcelain
(use-package magit)

;; client to fetch/manipulate issues, pull requests, and other data from forges (github, gitlab)
(use-package sqlite3)
(use-package forge)

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

;; ====================
;;   emacs web wowser
;; ===================

(use-package eww
  :config
  (setq eww-auto-rename-buffer t)
  (setq browse-url-browser-function 'eww-browse-url)
  ;; Set the built in no frills browser to use readable-mode by default for most pages, except the ones listed here
  (setq eww-readable-urls '(("^https://wikipedia.org$" . nil)
  			  ("^https://github.com$" . nil)
  			  ("^https://developer.mozilla.org$" . nil)
  			  ".*")))

;; ==================
;;       Dired
;; ==================

(defun mars/dired-mode-hook ()
  (interactive)
  (dired-hide-details-mode 1)
  (hl-line-mode 1))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("b" . dired-up-directory))
  :config
  (setq dired-listing-switches "-alv --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-dwim-target 'dired-dwim-target-next
        dired-hide-details-hide-symlink-targets nil
        dired-kill-when-opening-new-dired-buffer t
        delete-by-moving-to-trash t)

  (add-hook 'dired-mode-hook #'mars/dired-mode-hook))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))


;; ==================
;;    Keybindings
;; ==================

;; Hydra - transient key bindings
;; tl;dr create commands that create temporary keybindings
;; good for repetitive tasks like resizing windows, navigating windows because you create a new mode where you can freely repeat commands without prefixes until you quit (q)
(use-package hydra)

;; Straight up better modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Remap keys, create submenus, etc.
(use-package general
  :ensure t
  :config
  ;; * Global Keybindings
  (general-define-key
   "M-x" 'execute-extended-command
   "C-s" 'consult-line
   "C-r" 'consult-ripgrep
   "C-x b" 'consult-buffer
   "C-x C-f" 'find-file
   "M-." 'embark-act
   "M-g g" 'consult-goto-line
   "M-g M-g" 'consult-goto-line)

  ;; Create a leader key definer for C-c key
  (general-create-definer mars-leader-def
    :prefix "C-c")

  ;; ** Global Keybindings with mars-leader-def
  (mars-leader-def
    ;; Single key bindings
    "a" 'org-agenda
    "c" 'org-capture
    "e" 'embark-act
    "w" 'hydra-window/body
    ;; "p" 'hydra-project/body
    "o" 'hydra-org/body
    
    ;; Files section
    "f" '(:ignore t :which-key "files")
    "ff" 'find-file
    "fr" 'consult-recent-file
    "fp" 'projectile-find-file
    
    ;; Search section
    "s" '(:ignore t :which-key "search")
    "ss" 'consult-line
    "sp" 'consult-ripgrep
    "si" 'consult-imenu
    "so" 'consult-outline
    
    ;; Buffer section
    "b" '(:ignore t :which-key "buffers")
    "bb" 'consult-buffer
    "bd" 'kill-current-buffer
    "bs" 'save-buffer
    
    ;; Help section
    "h" '(:ignore t :which-key "help")
    "hf" 'describe-function
    "hv" 'describe-variable
    "hk" 'describe-key)

  (defhydra hydra-window (:columns 4)
    "Window Management"
    ("h" windmove-left "left")
    ("j" windmove-down "down")
    ("k" windmove-up "up")
    ("l" windmove-right "right")
    ("v" split-window-right "vsplit")
    ("s" split-window-below "hsplit")
    ("c" delete-window "close")
    ("d" delete-window "delete")
    ("u" winner-undo "undo")
    ("o" delete-other-windows "only")
    ("w" ace-window "pick")
    ("+" enlarge-window "increase")
    ("-" shrink-window "decrease")
    ("=" balance-windows "equalize")
    ("r" (lambda () (interactive) (window-swap-states (selected-window) (next-window))) "rotate")
    ("b" balance-windows "balance")
    ("m" maximize-window "maximize")
    ("f" toggle-frame-fullscreen "fullframe")
    ("q" nil "quit" :exit t))
  
  (defhydra hydra-navigate (:columns 4)
    "Navigation"
    ("n" next-buffer "next buffer")
    ("p" previous-buffer "prev buffer")
    ("k" kill-buffer "kill buffer")
    ("R" rename-buffer "rename buffer")
    ("f" find-file "find file")
    ("s" save-buffer "save file")
    ("S" save-some-buffers "save all")
    ("r" consult-recent-file "recent files")
    ("d" dired "dired")
    ("g" goto-line "goto line")
    ("t" beginning-of-buffer "top")
    ("e" end-of-buffer "end")
    ("m" set-mark-command "mark")
    ("a" beginning-of-line "beginning")
    ("b" switch-to-buffer "switch buffer")
    ("i" imenu "imenu")
    ("I" consult-imenu "consult-imenu")
    ("q" nil "quit" :exit t))

  (defhydra hydra-org (:columns 4 :color blue)
    "Org Mode Commands"
    ("h" org-insert-heading "headline")
    ("k" org-cut-subtree "kill")
    ("p" org-priority "priority")
    ("t" org-todo "cycle todo")
    ("s" org-schedule "schedule")
    ("d" org-deadline "deadline")
    ("i" org-clock-in "clock in")
    ("o" org-clock-out "clock out")
    ("r" org-clock-report "report")
    ("l" org-insert-link "link")
    ("T" org-time-stamp "timestamp")
    ("P" org-set-property "property")
    ("q" nil "quit"))
  
  ;; Configure settings
  (general-setq auto-revert-interval 10)
  (general-setq vertico-count 15)
  (general-setq completion-styles '(orderless basic))
  (general-setq orderless-component-separator #'orderless-escapable-split-on-space)
  (general-setq enable-recursive-minibuffers t)
  (general-setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

;; ============================
;; Incremental fuzzy completion
;; ============================

;; Vertical interactive completion
(use-package vertico
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode)
  :config
  (define-key vertico-map (kbd "C-.") #'embark-act))

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
	 ;("C-c h" . consult-history)
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

;; ===================
;;      Projects
;; ===================

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code") ; set default directory for projects
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired ; start projectile in dired view
	projectile-tags-command "ctags-exuberant -Re -f %s %s"
	projectile-cleanup-known-projects t))

(use-package ripgrep)

;; incorporate projectile into consult
(use-package consult-projectile)

;; ================
;;   Context Menu
;; =================

;; Choose command based on what's near point, both in multibuffer completion and normal buffers
(use-package embark
  :bind
  (("M-." . embark-act)
   ("M-o" . embark-act)
   ("M-;" . embark-dwim) ; good alternative to M-.
   ("C-h B" . embark-bindings) ; alternative for describe-bindin
   :map minibuffer-local-map
   ("C-@" . embark-select) ; C-SPC shows up as C-@ in the minibuffer
   ("C-SPC" . embark-select)
   ("C-." . embark-act)
   ("M-o" . embark-act)
   :map embark-org-table-cell-map
   ("M-b" . org-table-move-cell-left)
   ("M-f" . org-table-move-cell-right)
   ("M-p" . org-table-move-cell-up)
   ("M-n" . org-table-move-cell-down)
   :map embark-heading-map
   ("M-b" . org-do-promote)
   ("M-f" . org-do-demote)
   :map embark-org-item-map
   ("M-b" . org-outdent-item)
   ("M-f" . org-indent-item)
   ("M-p" . org-move-item-up)
   ("M-n" . org-move-item-down))
  :init
  (setq prefix-help-command #'embark-prefix-help-command) ;replace key help with completing read interface
  :config
  (define-key minibuffer-local-map (kbd "C-.") #'embark-act)
  (define-key embark-org-table-cell-map (kbd "M-b") #'embark-org-metaleft)
  (define-key embark-org-table-cell-map (kbd "M-f") #'embark-org-metaright)
  (define-key embark-org-table-cell-map (kbd "M-p") #'embark-org-metaup)
  (define-key embark-org-table-cell-map (kbd "M-n") #'embark-org-metadown)
  (define-key embark-org-table-cell-map (kbd "M-n") #'embark-org-metadown)
  (define-key embark-region-map (kbd "M-b") #'indent-rigidly)
  (define-key embark-region-map (kbd "M-f") #'indent-rigidly)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ==========================
;;   Completion Popup Frame
;; ==========================

;; Enhance in buffer completion with small completion popup (child frame)
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (setq nerd-icons-corfu-mapping
      '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
        (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
        (class :style "cod" :icon "symbol_class" :face font-lock-type-face)
        (color :style "cod" :icon "symbol_color" :face success)
        (constant :style "cod" :icon "symbol_constant" :face font-lock-constant-face)
        (constructor :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
        (enum :style "cod" :icon "symbol_enum" :face font-lock-builtin-face)
        (enum-member :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
        (event :style "cod" :icon "symbol_event" :face font-lock-warning-face)
        (field :style "cod" :icon "symbol_field" :face font-lock-variable-name-face)
        (file :fn nerd-icons-icon-for-file :face font-lock-string-face)
        (folder :fn nerd-icons-icon-for-dir :face font-lock-doc-face)
        (function :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
        (interface :style "cod" :icon "symbol_interface" :face font-lock-type-face)
        (keyword :style "cod" :icon "symbol_keyword" :face font-lock-keyword-face)
        (method :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
        (module :style "cod" :icon "symbol_namespace" :face font-lock-preprocessor-face)
        (numeric :style "cod" :icon "symbol_numeric" :face font-lock-builtin-face)
        (operator :style "cod" :icon "symbol_operator" :face font-lock-comment-delimiter-face)
        (param :style "cod" :icon "symbol_parameter" :face font-lock-builtin-face)
        (property :style "cod" :icon "symbol_property" :face font-lock-variable-name-face)
        (reference :style "cod" :icon "symbol_ruler" :face font-lock-variable-name-face)
        (snippet :style "cod" :icon "symbol_snippet" :face font-lock-string-face)
        (string :style "cod" :icon "symbol_string" :face font-lock-string-face)
        (struct :style "cod" :icon "symbol_structure" :face font-lock-variable-name-face)
        (text :style "cod" :icon "symbol_key" :face font-lock-doc-face)
        (unit :style "cod" :icon "symbol_misc" :face font-lock-constant-face)
        (value :style "cod" :icon "symbol_field" :face font-lock-builtin-face)
        (variable :style "cod" :icon "symbol_variable" :face font-lock-variable-name-face)
        (t :style "cod" :icon "code" :face font-lock-warning-face))))

;; =====================
;;  Completion at point
;; ====================

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("M-p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (advice-add 'cape-file :around #'cape-wrap-annotate)
  (advice-add 'cape-dabbrev :around #'cape-wrap-annotate))

;; useful configurations for cape
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

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

;; ===============
;;      llm
;; ===============

(use-package chatgpt-shell
  :config
  (setq chatgpt-shell-google-key
	(auth-source-pick-first-password :host "api.gemini.com")))

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

;; Enable built-in tab bar with Tokyo Night colors
(setq tab-bar-show t)

;; Style tab-bar to match Tokyo Night theme
(custom-set-faces
 '(tab-bar ((t (:background "#16161e" :foreground "#565f89"))))
 '(tab-bar-tab ((t (:background "#1a1b26" :foreground "#c0caf5" :box (:line-width 2 :color "#1a1b26")))))
 '(tab-bar-tab-inactive ((t (:background "#16161e" :foreground "#565f89" :box nil)))))

;; Optional: Add some padding to tab-bar for better aesthetics
(setq tab-bar-new-button "➕"
      tab-bar-close-button "✕"
      tab-bar-auto-width-max '(200 20)) ;; Adjust max width for tabs

(use-package all-the-icons)

;; Add icons to tab-bar-tabs
(defun mars/tab-bar-tab-name-function ()
  (let* ((tab-name (tab-bar-tab-name-current))
         (icon (all-the-icons-octicon "file-directory" 
                :v-adjust 0.0 
                :height 0.9 
                :face 'all-the-icons-blue)))
    (format " %s %s " icon tab-name)))

(setq tab-bar-tab-name-function #'mars/tab-bar-tab-name-function)
(tab-bar-mode t)

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "wave"
	centaur-tabs-set-icons t
	centaur-tabs-icon-type 'nerd-icons
	centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-set-bar 'over
	centaur-tabs-set-close-button nil
	centaur-tabs-set-modified-marker t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

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
