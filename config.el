;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my personal Emacs configuration. The file is tangled from a
;; literate org document where I try to document what each piece is for.
;; If you found my configurations you are free to use as you please, but
;; please read the whole thing before you do. I regulary commit my
;; configuration even though it is not working. Yes, I know, you shouldn't
;; do that but you have been warned.

;; Here be dragons

;; Code:

(setq user-full-name "Marcus"
       user-mail-address "marcus@r38.se")

(use-package! git-gutter
  :hook (after-init . global-git-gutter-mode))

;;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-nord-aurora)

(setq display-line-number-type t)

;; Setup size of frame on startup
(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 170))

(setq org-directory "~/Org/"
      my-agenda-dirs '("~/.doom.d" "~/Org") ; add more directories to look for agenda entries
      org-agenda-files (mapcan (lambda (x) (directory-files-recursively
                                            (expand-file-name x org-directory)
                                            "\.org$"))
                               my-agenda-dirs))

(use-package! org-tempo
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("p" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  )

(use-package! org-agenda
  :bind (("C-c a" . org-agenda))
  :hook ((org-agenda-finalize . hl-line-mode)
         (org-agenda-finalize . org-agenda-entry-text-mode))
  :custom
  (org-agenda-current-time-string (if (and (display-graphic-p)
                                           (char-displayable-p ?←)
                                           (char-displayable-p ?-))
                                      "⬅️ now"
                                    "now - - - - - - - - - - - - - - - - - - - - - - - - -"))
  (org-agenda-timegrid-use-ampm nil)
  (org-agenda-tags-column 0)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-log-mode-items '(closed clock state))
  (org-agenda-time-grid '((daily today require-timed)
                          (600 800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-start-on-weedkay nil)
  )

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

;; Elfeed
(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-feeds (quote
                    (("https://hackaday.com/blog/feed/" hackaday linux)
                     ("https://planet.emacslife.com/atom.xml" emacs)
                     )))
(global-set-key (kbd "C-x w") 'elfeed)
