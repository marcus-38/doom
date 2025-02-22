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

(use-package! winner
  :hook after-init
  :commands (winner-undo winner-redo)
  :custom
  (winner-boring-buffers '("*Completions*" "*Help*" "*Apropos*" "*Buffer List*" "*info*" "*Compile-Log*")))

(use-package! window
  :defer
  :custom
  (recenter-positions '(middle top bottom)))

(use-package! autorevert
  :custom
  (global-auto-revert-mode))

(use-package! whitespace
  :hook (before-save . whitespace-cleanup))

(map! :leader
      :desc "Structure block"
      "i b" #'org-insert-structure-template)

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

(use-package! dired
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :custom
  (dired-do-revert-buffer t)
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash t)
  (dired-mouse-drag-files t)
  (dired-dwim-target t)
  (dired-listing-switches "-AlhoF --group-directories-first"))
(use-package! all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))
(use-package! files
  :custom
  (insert-directory-program "gls"))

(use-package! all-the-icons-ibuffer
  :custom
  (all-the-icons-ibuffer-formats
   ´((mark modified read-only locked vc-satus-mini
           " " ,(if all-the-icons-ibuffer-icon
                    ´(icon 2 2 :left :elide)
                    "")
           ,(if all-the-icons-ibuffer-icon
                (propertize " " 'display ´(space :align-to 8))
              "")
           (name 18 18 :left :elide)
           " " (size-h 9 -1 :right)
           " " (mode+ 16 16 :right)
           " " (vc-status 16 16 :left)
           " " vc-relative-file)
     (mark " " (name 16 -1) " " filename)))
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package! ibuffer-vc
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-vc-status)
                       )
                     )))
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (mode . org-mode))
               ("magit" (name ."^magit"))
               ("planner" (or (name . "^\\*Calendar\\*$")
                              (name . "^\\*Org Agenda\\*")))
               ("emacs" (or (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$")))))))
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

(use-package! lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-enable-folding nil)
  (lsp-enable-links nil)
  (lsp-enable-snippet nil)
  (lsp-keymap-prefix "C-c ;")
  (lsp-session-file (expand-file-name (format "%s/emacs/lsp-session-v1" xdg-data)))
  (read-process-output-max (* 1024 1024)))

(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package! consult-lsp
  :commands (consult-lsp-diagnostics consult-lsp-symbols))

(use-package! dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package! ccls
  :after projectile
  :hook ((c-mode c++-mode objc-mode cuda-mode) . lsp-deferred)
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("complie_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(use-package! google-c-style
  :hook (((c-mode c++-mode ) . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

(use-package! cmake-mode
  :hook (cmake-mode . lsp-deferred)
  :mode ("CMakeLists\\.txt'" "\\.cmake\\'"))

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

(use-package! eww
  :init (add-hook 'eww-after-render-hook #'shrface-mode))

(use-package! shrface
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings)
  (setq shrface-href-versatile t))
