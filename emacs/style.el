;;; package -- Summary

;;; Commentary:
;;; This package contains stylistic configuration lik e

;;; Code:

(prelude-require-packages '(solarized-theme))

;; Use a proper font
(set-frame-font "Fantasque Sans Mono 13")

;; Use solarized theme
(load-theme 'solarized-dark t)

;; Gurus don't need no scrollbars.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(provide 'style)
;;; style.el ends here
