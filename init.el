(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Pallet keeps the Cask file in sync with the packages
;; that you install/uninstall via M-x package-list-packages
;; this does not work for the moment
;; install with command "cask install" instead
;; (require 'pallet)

;; Move the stuff below to other files.

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-splash-screen t)

;; Highligt current line
(global-hl-line-mode 1)

;; Add line numbers
(global-linum-mode 1)

;; Highlights the matching paren
(show-paren-mode 1)

;; Larger fonts
(set-face-attribute 'default nil :height 140)
;(set-face-attribute 'default nil :height 160)

;; Interactively Do Things
(ido-mode t)

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

(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)

;; Theme
;; https://github.com/bbatsov/zenburn-emacs
(load-theme 'zenburn t)
(set-cursor-color "firebrick")

;; Fix encoding
(setq default-process-coding-system '(utf-8 . utf-8))

;; Disable ring bell
(setq ring-bell-function 'ignore)

;; Remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;
(setq
 mac-option-modifier nil
 mac-command-modifier 'meta
 x-select-enable-clipboard t)
