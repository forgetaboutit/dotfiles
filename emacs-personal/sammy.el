;;; Sammy.el --- summary

;;; Commentary:

;;; Code:

;; Ensure all the modules are available.
(prelude-require-packages '(paredit
                            cljsbuild-mode
                            solarized-theme
                            multiple-cursors
                            auto-complete))

;; No flyspell anymore (for now)
(setq prelude-flyspell nil)

;; No flycheck for SCSS
(eval-after-load 'flycheck
  '(setq-default flycheck-disabled-checkers '(scss)))

;; Use solarized
(load-theme 'solarized-dark t)

;;;
;;; Standard-deviating keybindings
;;;

;; Better buffer cycling
(global-set-key [C-tab] 'previous-buffer)
(global-set-key [C-S-iso-lefttab] 'next-buffer)
(global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c i") 'magit-stash)

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
;;; auto-complete
;;;
(require 'auto-complete-config)
(ac-config-default)

(dolist (mode '(coffee-mode scss-mode))
  (add-to-list 'ac-modes mode))

;;;
;;; dirtree
;;;
(require 'dirtree)

(defun dirtree-for-buffer ()
  "Open dirtree in the current buffer's directory or the default directory."
  (interactive)
  (let (current-file (buffer-file-name))
    (if current-file
        (dirtree (file-name-directory current-file) nil)
      (dirtree default-directory nil))))

;; Dirtree instead of dired
(global-set-key (kbd "C-x d") 'dirtree)
(global-set-key (kbd "C-x C-j") 'dirtree-for-buffer)

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
