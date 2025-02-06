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
