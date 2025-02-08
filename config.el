;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Marcus"
       user-mail-address "marcus@r38.se")

;;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-nord-aurora)

(setq display-line-numbers-type t)


(setq org-directory "~/Org/")

;; Setup size of frame on startup
(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 170))

;; Elfeed
(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-feeds (quote
                    (("https://hackaday.com/blog/feed/" hackaday linux)
                     ("https://planet.emacslife.com/atom.xml" emacs)
                     )))
(global-set-key (kbd "C-x w") 'elfeed)

(use-package! rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)))
:config
;;
(setq rustic-format-on-save t)
(add-hook 'rustic-mode-hook 'my/rustic-mode-hook)

(defun my/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))
