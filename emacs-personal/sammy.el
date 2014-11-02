;;; Sammy.el --- summary

;;; Commentary:

;;; Code:

;; Ensure all the modules are available.
(prelude-require-packages '(paredit
                            cljsbuild-mode
                            solarized-theme
                            multiple-cursors
                            auto-complete
                            expand-region
                            fiplr
                            emmet-mode
                            cider
                            haml-mode
                            sass-mode
                            shm))

;; Use a proper font
(set-frame-font "Fantasque Sans Mono 13")

;; Gurus don't need no scrollbars.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No flyspell anymore (for now)
(setq prelude-flyspell nil)

;; Please don't pollute my directories! However, this could be dangerous.
(setq create-lockfiles nil)

;; No flycheck for SCSS
(eval-after-load 'flycheck
  '(setq-default flycheck-disabled-checkers '(scss sass)))

;; Use solarized
(load-theme 'solarized-dark t)

;;;
;;; Standard-deviating keybindings
;;;

;; Better buffer cycling
(global-set-key [C-tab] 'previous-buffer)
(global-set-key [C-S-iso-lefttab] 'next-buffer)
(global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "M-s l") 'sort-lines)

(global-unset-key (kbd "C-z"))          ; Prevent accidential suspending

;; Better killing
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Zap zap zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "s-z") (lambda (char)
                              (interactive "cZap up to char backwards: ")
                              (zap-up-to-char -1 char)))

(global-set-key (kbd "M-Z") (lambda (char)
                              (interactive "cZap to char: ")
                              (zap-to-char 1 char)))
(global-set-key (kbd "s-Z") (lambda (char)
                              (interactive "cZap to char backwards: ")
                              (zap-to-char -1 char)))

;;;
;;; Fiplr
;;;
(require 'fiplr)

(setq fiplr-root-markers '(".gitmodules" ".git" "generated" "out"))
(setq fiplr-ignored-globs '((directories (".git"))
                            (files ("*.png"))))
(global-set-key (kbd "C-x f") 'fiplr-find-file)

;;;
;;; Dart mode
;;;
(require 'dart-mode)

;; indent 2 spaces
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

;;;
;;; Haskell mode
;;;
;; Use Haskell-mode specific save
(setq exec-path (append exec-path '("~/.cabal/bin")))

;; Use cabal repl to support sandboxes
(setq haskell-program-name "cabal repl")

;; Automatic formatting with stylish-haskell
(setq haskell-stylish-on-save t)

;;;
;;; Structured Haskell mode
;;;
;; Load SHM properly
(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; Use SHM highlighting, so disable global line highlighting.
(eval-after-load 'haskell-mode
  '(global-hl-line-mode 0))

;; Configure SHM executable location
(setq shm-program-name "~/.cabal/bin/structured-haskell-mode")

;; Configure SHM colors for solarized-dark
(set-face-background 'shm-current-face "#073642") ; solarized-base02

;;;
;;; Paredit
;;;
(add-hook 'clojure-mode-hook 'paredit-mode)

;;;
;;; Clojure mode
;;;
;; Prevent excessive indentation.
(setq clojure-defun-style-default-indent t)

;;;
;;; Cider REPL
;;;
;; Enhance REPL with Paredit and rainbow delimiters
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;;;
;;; Ace-jump-mode
;;;
(global-set-key (kbd "C-c l") 'ace-jump-line-mode)
(global-set-key (kbd "C-c c") 'ace-jump-char-mode)

;;;
;;; multiple-cursors
;;;
(global-set-key (kbd "C-'") 'mc/mark-next-like-this)
(global-set-key (kbd "C-\"") 'mc/mark-all-like-this)

;;;
;;; Pending delete mode
;;;
(pending-delete-mode 1)

;;;
;;; expand-region-mode
;;;
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;;;
;;; auto-complete
;;;
(require 'auto-complete-config)
(ac-config-default)

(dolist (mode '(coffee-mode scss-mode))
  (add-to-list 'ac-modes mode))

;;;
;;; dired
;;;
(global-set-key (kbd "C-x C-d") 'ido-dired)

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-l") 'switch-to-haskell)))

;;;
;;; haml-mode
;;;
(require 'haml-mode)

;;;
;;; sass-mode
;;;
(require 'sass-mode)

;;;
;;; coffee-mode
;;;
(custom-set-variables '(coffee-tab-width 2))

;;;
;;; Inline Elisp
;;;

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-x C-e") 'eval-and-replace)

(provide 'sammy)
;;; sammy.el ends here
