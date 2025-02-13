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

;;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-nord-aurora)

(setq display-line-number-type t)

(setq blink-matching-paren 'jump
      show-paren-style 'mixed
      blink-matching-delay 1)

(use-package! pulse
  :init
  (defun pulse-line (&rest _)
    "Pulse the current line"
    (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(scroll-up-command
                     scroll-down-command
                     windmove-left
                     windmove-right
                     windmove-up
                     windmove-down
                     move-to-window-line-top-bottom
                     recenter-top-bottom
                     other-window))
    (advice-add command :after #'pulse-line)))

(use-package! autorevert
  :custom
  (global-auto-revert-mode))

(map! :leader
      "W" #'my/windows-transient-window)

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

(use-package! transient
  :defer
  :bind ("C-M-o" . my/windows-transient-window)
  :init
  (transient-define-prefix my/windows-transient-window ()
   "Display a transient buffer showing useful window manipulation bindings."
    [["Resize"
     (")" "h+" enlarge-window-horizontally :transient t)
     ("(" "h-" shrink-window-horizontally :transient t)
     ("K" "v+" enlarge-window :transient t)
     ("J" "v-" shrink-window :transient t)]
     ["Split"
    ("v" "vertical" (lambda ()
       (interactive)
       (split-window-right)
       (windmove-right)) :transient t)
    ("x" "horizontal" (lambda ()
       (interactive)
       (split-window-below)
       (windmove-down)) :transient t)
    ("wv" "win-vertical" (lambda ()
       (interactive)
       (select-window (split-window-right))
       (windows-transient-window)) :transient nil)
    ("wx" "win-horizontal" (lambda ()
       (interactive)
       (select-window (split-window-below))
       (windows-transient-window)) :transient nil)]
    ["Misc"
     ("B" "switch buffer" (lambda ()
                            (interactive)
                            (consult-buffer)
                            (windows-transient-window)))
     ("z" "undo" (lambda ()
                  (interactive)
                  (winner-undo)
                  (setq this-command 'winner-undo)) :transient t)
    ("Z" "redo" winner-redo :transient t)
    ("q" "exit menu" (lambda ()
                       (interactive)
                       (doom/escape)) :transient nil)
    ]]
    [["Move"
    ("h" "←" windmove-left :transient t)
    ("j" "↓" windmove-down :transient t)
    ("l" "→" windmove-right :transient t)
    ("k" "↑" windmove-up :transient t)]
    ["Swap"
    ("sh" "←" windmove-swap-states-left :transient t)
    ("sj" "↓" windmove-swap-states-down :transient t)
    ("sl" "→" windmove-swap-states-right :transient t)
    ("sk" "↑" windmove-swap-states-up :transient t)]
    ["Delete"
    ("dh" "←" windmove-delete-left :transient t)
    ("dj" "↓" windmove-delete-down :transient t)
    ("dl" "→" windmove-delete-right :transient t)
    ("dk" "↑" windmove-delete-up :transient t)
    ("D" "This" delete-window :transient t)]
    ]))
