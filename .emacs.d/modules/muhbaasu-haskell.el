;;; Haskell

;; Add Cabal executables to path
(setq exec-path (append exec-path '("~/.cabal/bin")))

;; Use advanced indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Clean file after save using stylish-haskell
(setq haskell-stylish-on-save t)

(provide 'muhbaasu-haskell)
;;; muhbaasu-haskell.el ends here
