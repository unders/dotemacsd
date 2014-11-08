(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Pallet keeps the Cask file in sync with the packages
;; that you install/uninstall via M-x package-list-packages
(require 'pallet)

;; Move the stuff below to other files.

;; Larger fonts
(set-face-attribute 'default nil :height 140)
(set-frame-font "Menlo-14")

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-splash-screen t)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(set-face-attribute 'region nil :background "#3e5656")


;; Add line numbers
;(global-linum-mode 1)

;; Add the cursor's column position
(column-number-mode 1)

;; Highlights the matching paren
(show-paren-mode 1)

;; Git gutter
(when (window-system)
(require 'git-gutter-fringe))

(global-git-gutter-mode +1)
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; A smart M-x enhancement for Emacs
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Interactively Do Things
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; ag
(require 'wgrep)
(autoload 'wgrep-ag-setup "wgrep-ag")
(setq wgrep-auto-save-buffer t)
(setq wgrep-enable-key "r")

(add-hook 'ag-mode-hook 'unders/ag-mode)

(defun unders/ag-mode ()
  (wgrep-ag-setup)
  (define-key (current-local-map) (kbd "o") 'unders/open-but-dont-move))

;; open search result but stay in ag buffer
(defun unders/open-but-dont-move ()
  (interactive)
  (let ((buffer (current-buffer)))
    (compile-goto-error)
    (pop-to-buffer buffer)))

(require 'ag)

;; Projectile
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(projectile-global-mode)
(global-set-key (kbd "M-e") 'projectile-recentf)

;; Display ido results vertically, rather than horizontally
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  (defun ido-define-keys () ;; C-j/k is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-k") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys)

(add-hook 'projectile-mode-hook 'projectile-rails-on)


;; A GNU Emacs library to setup environment variables from the user's shell.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

 (require 'yaml-mode)
    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Prefix buffer names with its directory name
;; when two files have the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Git
(require 'magit)
(eval-after-load 'magit
  (progn '(global-set-key (kbd "C-x g") 'magit-status)))

;; Evil
(require 'evil)
(require 'evil-leader)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-mode 1)

;; Disable this so that C-k will work for paredit.
;; evil-insert-digraph
(define-key evil-insert-state-map (kbd "C-k") nil)
;; evil-shift-left-line
(define-key evil-insert-state-map (kbd "C-d") nil)


;; ace-jump-char-mode, ace-jump-line-mode, ace-jump-mode
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-word-mode)

(evil-leader/set-key
  ;;"x"  'execute-extended-command
  ;"s"   run single test
  "s" 'racer-find-definition ;; racer find definition
  "c"  'compile
  "d"  'dash-at-point
  "1"  'delete-other-windows
  "m"  'projectile-ag
  "f"  'projectile-find-file
  "t"  'projectile-toggle-between-implementation-and-test
  "gl" 'ispell-region
  "gb" 'ispell-buffer
  "gr" 'projectile-rails-rake
  "gg" 'projectile-rails-generate
  "gm" 'projectile-rails-find-model
  "ge" 'ielm
  "r"  'minitest-rerun
  "v"  'minitest-verify
  "a"  'minitest-verify-all
  "p"  'projectile-switch-project
  "gf" 'projectile-rails-goto-file-at-point
  "u"  'undo-tree-visualize
  "e"  'eshell
  "b"  'switch-to-buffer
  "k"  'kill-buffer)

(global-set-key (kbd "C-x f") 'projectile-find-file)

;; Don't backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; turn on auto complete
(require 'auto-complete-config)
(ac-config-default)

;; Rust start
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(require 'flymake-rust)
(add-hook 'rust-mode-hook 'flymake-rust-load)
(add-hook 'rust-mode-hook 'global-company-mode)
(add-to-list 'load-path "/Users/unders/Projects/racer/editors") (require 'racer)

(require 'toml-mode)

;; Rust end

;; rbenv to manage your Ruby versions within Emacs
(require 'rbenv)
(global-rbenv-mode)

;; Ruby mode
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(defun ri-bind-key ()
  (local-set-key (kbd "C-h r") 'yari))

(defun my-ruby-hook ()
  ;; (flymake-ruby-load)
  (minitest-mode)
  (ri-bind-key)
  (modify-syntax-entry ?: ".")
  (modify-syntax-entry ?@ "w")
  (modify-syntax-entry ?$ "w")
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?? "w")
  (modify-syntax-entry ?- "w"))

(add-hook 'ruby-mode-hook 'my-ruby-hook)

(setq-default minitest-keymap-prefix (kbd "C-c C-c ,"))
(setq-default minitest-use-zeus-when-possible nil)
(setq-default minitest-use-bundler nil)
(setq-default minitest-default-env "export $(cat .env | xargs) &&")
(require 'minitest)
(require 'rspec-mode)

;; when yasnippet installed
; (eval-after-load 'rspec-mode
; '(rspec-install-snippets))

;; Flymake-ruby TODO: Maybe replace Flymake
;; (require 'flymake-ruby)

(setq ruby-deep-indent-paren nil)
(global-set-key (kbd "C-c r r") 'inf-ruby)

;; inf-ruby
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; robe-mode
(add-hook 'ruby-mode-hook 'robe-mode)

;; auto-complete
(add-hook 'ruby-mode-hook
          (lambda ()
            (make-local-variable 'ac-ignores)
            (add-to-list 'ac-ignores "end")))

;; Smartparens
(require 'smartparens)
(require 'smartparens-ruby)
(add-hook 'ruby-mode-hook
          (lambda ()
            (smartparens-mode +1)
            (show-smartparens-mode)))

(add-hook 'ruby-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;; elisp
(setq paredit-awesome-p t)
(autoload 'enable-paredit-mode "paredit")
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; TODO: Need help with this!
;; (defun my-lisp-hook ()
;;   (modify-syntax-entry ?- "w"))
;; (add-hook 'lisp-mode-hook       'my-lisp-hook)

;; elisp

;; Clojure

;; Clojure

;; Theme
;; https://github.com/bbatsov/zenburn-emacs
;; (load-theme 'zenburn t)

;; http://chapter31.com/2013/06/01/base16-colour-scheme-for-hackers/
;; https://github.com/chriskempson/base16
;; https://github.com/neil477/base16-emacs
;; (load-theme 'base16-default t)
(load-theme 'base16-tomorrow t)

;; https://github.com/stafu/noctilux-theme
;; (load-theme 'noctilux t)
;; (load-theme 'mccarthy t)
;; (load-theme 'wilson t)



;; (set-cursor-color "firebrick")

;; Use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; When you visit a file, point goes to the last
;; place where it was when you previously visited it
(require 'saveplace)
(setq-default save-place t)

;; Fix encoding
(setq default-process-coding-system '(utf-8 . utf-8))

;; hunspell
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

;; Smooth scrolling
;; From http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
      mouse-wheel-progressive-speed nil
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)


;; Disable ring bell
(setq ring-bell-function 'ignore)

;; Remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;
(setq
 mac-option-modifier nil
 mac-command-modifier 'meta
 x-select-enable-clipboard t)

;; change window keybingings
(global-set-key (kbd "C-c C-<right>") 'windmove-right)
(global-set-key (kbd "C-c C-<up>")    'windmove-up)
(global-set-key (kbd "C-c C-<left>")  'windmove-left)
(global-set-key (kbd "C-c C-<down>")  'windmove-down)

;; Move the stuff above to other files.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ignore-case nil)
 '(ag-arguments (quote ("--smart-case" "--nogroup" "--column" "--ignore=log" "--ignore=app/sass/bourbon" "--ignore=bower_components" "--ignore=node_modules" "--ignore=app/sass/neat")))
 '(current-language-environment "UTF-8")
 '(feature-cucumber-command "zeus cucumber CUCUMBER_OPTS=\"{options}\" FEATURE=\"{feature}\"")
 '(ido-auto-merge-delay-time 10)
 '(magit-set-upstream-on-push t)
 '(rspec-spec-command "bin/rspec")
 '(rspec-use-bundler-when-possible nil)
 '(rspec-use-rake-when-possible nil)
 '(rspec-use-spring-when-possible nil)
 '(tree-widget-image-enable nil))

(defalias 'yes-or-no-p 'y-or-n-p)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
