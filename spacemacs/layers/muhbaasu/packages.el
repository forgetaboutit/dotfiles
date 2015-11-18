;;; packages.el --- muhbaasu Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq muhbaasu-packages
      '(
        ;; package names go here
        ace-jump-mode
        ace-jump-buffer
        ace-window
        magit
        multiple-cursors
        whitespace
        zop-to-char
      ))

;; List of packages to exclude.
(setq muhbaasu-excluded-packages '())

;; For each package, define a function muhbaasu/init-<package-name>
;;
;; (defun muhbaasu/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun muhbaasu/init-ace-jump-mode ()
  (use-package ace-jump
    :bind
    (("C-c j c" . ace-jump-char-mode)
     ("C-c j l" . ace-jump-line-mode))))

(defun muhbaasu/init-ace-jump-buffer ()
  (use-package ace-jump-buffer
    :bind
    (("C-c j b" . ace-jump-buffer)
     ("C-c j s" . ace-jump-same-mode-buffers))))

(defun muhbaasu/init-ace-window ()
  (use-package ace-window
    :bind
    ("M-i" . ace-window)))

(defun muhbaasu/init-magit ()
  (use-package magit
    :bind
    ("C-c m s" . magit-status)))

(defun muhbaasu/init-multiple-cursors ()
  (use-package multiple-cursors
    :bind
    (("C-'" . mc/mark-next-like-this)
     ("C-\"" . mc/mark-all-like-this))))

(defun muhbaasu/init-whitespace ()
  (use-package whitespace
    :init
    (add-hook 'text-mode-hook 'whitespace-mode)
    (add-hook 'prog-mode-hook 'whitespace-mode)
    (add-hook 'before-save-hook 'whitespace-cleanup)

    :config
    (setq whitespace-display-mappings
          '(
            ;; space | middle dot | full stop
            (space-mark 32 [183] [46])
            ;; line feed | downwards arrow with hook
            (newline-mark 10 [8617 10])
            ;; tab | rightwards arrow to bar
            (tab-mark 9 [8677 9])))

    ;; Adjust colors for whitespace highlighting
    (loop for face in '(whitespace-space
                        whitespace-newline
                        whitespace-tab) do
                        (set-face-attribute face
                                            nil
                                            :foreground "#3c4f56"
                                            :background nil))))

(defun muhbaasu/init-zop-to-char ()
  (use-package zop-to-char
    :bind
    (("M-z" . zop-up-to-char)
     ("M-Z" . zop-to-char))))
