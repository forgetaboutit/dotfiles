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
                            cider))

;; Use a proper font
(set-default-font "Anonymous Pro Bold 13")

;; No flyspell anymore (for now)
(setq prelude-flyspell nil)

;; Please don't pollute my directories! However, this could be dangerous.
(setq create-lockfiles nil)

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
(global-set-key (kbd "M-s l") 'sort-lines)

(global-unset-key (kbd "C-z"))          ; Prevent accidential suspending

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

;;;
;;; Emmet-mode
;;;
(require 'emmet-mode)

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; indent 2 spaces
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

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

;;;
;;; coffee-mode
;;;
(custom-set-variables '(coffee-tab-width 2))

;;;
;;; ido-imenu (emacs rocks #10), easy jump to search result
;;;
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (cat symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(global-set-key (kbd "C-x b") 'ido-imenu)

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
