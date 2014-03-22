(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Pallet keeps the Cask file in sync with the packages
;; that you install/uninstall via M-x package-list-packages
;; this does not work for the moment
;; install with command "cask install" instead
;; (require 'pallet)

;; Move the stuff below to other files.

;; Larger fonts
(set-face-attribute 'default nil :height 140)
(set-frame-font "Menlo-14")

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-splash-screen t)

;; Highligt current line
(global-hl-line-mode 1)

;; Add line numbers
;(global-linum-mode 1)

;; Add the cursor's column position
(column-number-mode 1)

;; Highlights the matching paren
(show-paren-mode 1)

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

;; rbenv to manage your Ruby versions within Emacs
(require 'rbenv)
(global-rbenv-mode)

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

;; ace-jump-char-mode, ace-jump-line-mode, ace-jump-mode
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-word-mode)

(evil-leader/set-key
  ;;"x"  'execute-extended-command
  ;"s"   run single test
  "m"  'projectile-ag
  "f"  'projectile-find-file
  "t"  'projectile-toggle-between-implementation-and-test
  "v"  'minitest-verify
  "r"  'minitest-rerun
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

;; Ruby mode


;; Flymake-ruby TODO: Maybe replace Flymake
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

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



;; Theme
;; https://github.com/bbatsov/zenburn-emacs
(load-theme 'zenburn t)
(set-cursor-color "firebrick")

;; Use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; When you visit a file, point goes to the last
;; place where it was when you previously visited it
(require 'saveplace)
(setq-default save-place t)

;; Fix encoding
(setq default-process-coding-system '(utf-8 . utf-8))


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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments (quote ("--smart-case" "--nogroup" "--column" "--ignore=log" "--ignore=app/sass/bourbon" "--ignore=bower_components" "--ignore=node_modules" "--ignore=app/sass/neat")))
 '(current-language-environment "UTF-8")
 '(feature-cucumber-command "zeus cucumber CUCUMBER_OPTS=\"{options}\" FEATURE=\"{feature}\"")
 '(magit-set-upstream-on-push t)
 '(tree-widget-image-enable nil))

(defalias 'yes-or-no-p 'y-or-n-p)
