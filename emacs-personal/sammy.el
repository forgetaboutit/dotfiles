;;; Sammy.el --- summary

;;; Commentary:

;;; Code:

;; Ensure all the modules are available.
(prelude-require-packages '(paredit
                            cljsbuild-mode
                            solarized-theme
                            multiple-cursors))

;; No flyspell anymore (for now)
(setq prelude-flyspell nil)

;; Use solarized
(load-theme 'solarized-dark t)

;;;
;;; Standard-deviating keybindings
;;;

;; Better buffer cycling
(global-set-key [C-tab] 'previous-buffer)
(global-set-key [C-S-iso-lefttab] 'next-buffer)
(global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)

;;;
;;; Structured Haskell mode
;;;

;; Load SHM properly
(add-to-list 'load-path "/home/sammy/git/structured-haskell-mode/elisp")
(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; Configure SHM executable location
(setq shm-program-name
      "/home/sammy/git/structured-haskell-mode/dist/build/structured-haskell-mode/structured-haskell-mode")

;; Configure SHM colors for solarized-dark
(set-face-background 'shm-current-face "#073642") ; solarized-base02

;;;
;;; Paredit
;;;
(add-hook 'clojure-mode-hook 'paredit-mode)

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
;;; dirtree
;;;
(require 'dirtree)

(provide 'sammy)
;;; sammy.el ends here
