;; Setup Package.el
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")))

;; Setup use-package.er
(package-initialize)

(when (not (require 'use-package nil 'noerror))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Visuals
(use-package molokai-theme
  :config
  (load-theme 'molokai t))

(use-package telephone-line
  :config
  (telephone-line-mode t))

;; Window config
(use-package switch-window
  :config
  (setq switch-window-shortcut-style 'qwerty)
  :bind ("C-x o" . switch-window))

;; MPC setup
(use-package mpc
  :config
  (setq mpc-browser-tags '(Artist Album|Playlist))
  :bind (:map mpc-mode-map
              ("f" . mpc-ffwd)
              ("b" . mpc-rewind)
              ("SPC" . mpc-play)))

;; Avy setup
(use-package avy
  :bind
  (("M-SPC" . 'avy-goto-word-or-subword-1)
   ("C-M-;" . 'avy-goto-char)
   ("C-c SPC" . 'avy-goto-char-2)
   ("C-c C-s" . 'avy-goto-char-timer)
   ("M-'" . 'avy-goto-line))
  :config
  (setq avy-background t)
  (setq avy-lead-faces '(highlight highlight highlight highlight)))

;; Ido setup
(use-package ido
  :init
  (ido-mode t))

;; Rainbow delimiters setup
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; company-mode setup
(use-package company
  :custom
  (company-idle-delay 0.5)
  :config
  (defun mewa/push-company-backend (backend)
    "Add company BACKEND as a buffer-local"
    (make-local-variable 'company-backends)
    (push backend company-backends))
  (global-company-mode t))

(use-package company-flx
  :after
  (company)
  :config
  (company-flx-mode t))

;; Scala setup
(use-package ensime)

(use-package sbt-mode)

(use-package scala-mode)

;; Haskell setup
(use-package haskell-mode
  :config
  (interactive-haskell-mode t))

(use-package intero
  :after (haskell company)
  :hook (haskell-mode . intero-mode))

;; Lisp setup
(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :after (slime))

;; Clojure setup
(use-package clojure-mode)

(use-package cider
  :after (clojure-mode company)
  :hook (clojure-mode . cider-mode)
  :config
  (setq cider-default-cljs-repl 'figwheel-main))

;; Elm setup
(use-package elm-mode
  :hook
  (elm-mode . (lambda () (mewa/push-company-backend 'company-elm))))

;; Go setup
(use-package go-mode)

(use-package company-go
  :after (go-mode company)
  :hook
  (go-mode . (lambda () (mewa/push-company-backend 'company-go))))

;; JavaScript setup
(use-package js2-mode
  :mode ("\\.js$" . js2-mode))

(use-package company-tern
  :hook
  (js2-mode . (lambda () (mewa/push-company-backend 'company-tern)))
  (js2-mode . tern-mode))

(defun ruby-reload ()
  (interactive)
  (progn
    (let ((buffer-modified-p "*pry*")
          (kill-buffer-query-functions nil))
      (kill-buffer "*pry*"))
    (inf-ruby-console-auto)))

;; Ruby setup
(use-package robe
  :after (company)
  :hook
  ((ruby-mode . robe-mode)
   (ruby-mode . (lambda () (mewa/push-company-backend 'company-robe))))
  :bind ("C-c M-l" . ruby-reload))

;; YAML setup
(use-package yaml-mode)

;; Rust setup
(use-package rust-mode)

;; Front-end
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; Indent with spaces
(setq-default indent-tabs-mode nil)

;; Setup line numbering
(defun linum-format-func (line)
  (let ((width (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format "%%%dd\u200a" width) line) 'face 'linum)))
(setq linum-format 'linum-format-func)
(global-linum-mode 1)

;; Disable menu bar
(menu-bar-mode 0)

;; Life's too short
(fset 'yes-or-no-p 'y-or-n-p)

;; Setup parenthesis
(show-paren-mode 1)
(electric-pair-mode 1)

;; Enable subword-mode globally
(global-subword-mode t)

;; Bindings

;; Switch between header/implementation
(global-set-key (kbd "C-c f") 'ff-find-other-file)


;; Kill word backward when no region is active,
;; otherwise kill region
(global-set-key "\C-w" (lambda (arg)
                         (interactive "*p")
                         (message "%s" arg)
                         (if (use-region-p)
                             (kill-region (region-beginning) (region-end))
                           (backward-kill-word arg))))

;; Enable disabled-by-default functions
(setq disabled-command-function nil)

(provide 'init)
;;; init.el ends here
