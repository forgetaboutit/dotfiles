;;; init.el --- Forgetaboutit's Emacs config

;; Base directory of this file
(defvar config-dir (file-name-directory load-file-name))

;; Don't accidentially use outdated files
(setq load-prefer-newer t)

;; Please don't pollute my directories!  This could be dangerous, however,
;; in a multi-user environment where multiple users edit the same file.
(setq create-lockfiles nil)

;; Store backup and autosave files in temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Better buffer cycling
(global-set-key [C-tab] 'previous-buffer)
(global-set-key [C-S-iso-lefttab] 'next-buffer)

;; Overwrite marked region
(pending-delete-mode 1)

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; Disable tool-, menu-, and scrollbars
(when (fboundp 'tool-bar-mode)
  (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode -1)))

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't use tabs to indent
(setq-default indent-tabs-mode nil)

;; ... but maintain correct appearance
(setq-default tab-width 2)

;; Newline at file end
(setq require-final-newline t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;;;
;;; Packages
;;;
(require 'cl)
(require 'package)

;; Add melpa package repository
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Define directory for packages
(setq package-user-dir (expand-file-name "elpa" config-dir))
(package-initialize)

;; Packages to auto-install
(defvar muhbaasu-packages
  '(ace-jump-mode
    ace-jump-buffer
    ace-window
    magit
    multiple-cursors
    paredit
    rainbow-delimiters
    smartparens
    solarized-theme
    zop-to-char))

(defun muhbaasu-packages-installed? ()
  (every #'package-installed-p muhbaasu-packages))

(defun muhbaasu-require-package (package)
  (unless (memq package muhbaasu-packages)
    (add-to-list 'muhbaasu-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun muhbaasu-require-packages (packages)
  (mapc #'muhbaasu-require-package packages))

(defun muhbaasu-install-packages ()
  (unless (muhbaasu-packages-installed?)
    (message "%s" "Refreshing the package database ...")
    (package-refresh-contents)
    (message "%s" "Package database refreshed.")
    (muhbaasu-require-packages muhbaasu-packages)))

(muhbaasu-install-packages)

;; Styling
(load-theme 'solarized-dark t)

(defvar solarized-base03 "#002b36")
(defvar solarized-base02 "#073652")
(defvar solarized-base01 "#586e75")
(defvar solarized-base00 "#657b83")
(defvar solarized-base0 "#839496")
(defvar solarized-base1 "#93a1a1")
(defvar solarized-base2 "#eee8d5")
(defvar solarized-base3 "#fdf6e3")

(defvar whitespace-color "#3c4f56")

;; Setup whitespace mode
(require 'whitespace)

(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])      ; space | middle dot | full stop
        (newline-mark 10 [8617 10])     ; line feed | downwards arrow with hook
        (tab-mark 9 [8677 9])))         ; tab | rightwards arrow to bar

;; Adjust colors for whitespace highlighting
(loop for face in '(whitespace-space
                    whitespace-newline
                    whitespace-tab) do
      (set-face-attribute face
                          nil
                          :foreground whitespace-color
                          :background nil))

(add-hook 'prog-mode-hook 'whitespace-mode)

;; Paredit
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Zop to char
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

;; Ace jump
(global-set-key (kbd "C-c j c") 'ace-jump-char-mode)
(global-set-key (kbd "C-c j l") 'ace-jump-line-mode)

;; Ace jump buffer
(require 'ace-jump-buffer)
(global-set-key (kbd "C-c j b") 'ace-jump-buffer)
(global-set-key (kbd "C-c j s") 'ace-jump-same-mode-buffers)

;; Ace window
(global-set-key (kbd "M-i") 'ace-window)

;; Multiple cursors
(global-set-key (kbd "C-'") 'mc/mark-next-like-this)
(global-set-key (kbd "C-\"") 'mc/mark-all-like-this)

;; Don't ask for confirmation on symlinks to versioned files
(setq vc-follow-symlinks nil)

;; Join lines
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Magit
(require 'magit)

(global-set-key (kbd "C-c m s") 'magit-status)

;; Explicitly silence warning about `magit-auto-revert-mode`
(setq magit-last-seen-setup-instructions "1.4.0")
