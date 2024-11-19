;; Setup Package.el
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "https://melpa.org/packages/")))

;; Setup use-package.er
(package-initialize)

(when (not (require 'use-package nil 'noerror))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Visuals
(use-package color-theme-sanityinc-tomorrow
  :config
  (setq theme 'sanityinc-tomorrow-eighties)

  (defun frame-load-theme (frame)
    (select-frame frame)
    (load-theme theme t))

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'frame-load-theme)
    (load-theme theme t)))

;; Indent highlight setup
(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 10))

;; Telephone line
(use-package telephone-line
  :config
  (telephone-line-mode t))

;; Translations
(use-package po-mode
  :config
  (setq po-auto-update-file-header nil))

;; Window config
(use-package switch-window
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts '("a" "r" "s" "t" "n" "e" "i" "o" "g" "m" "f" "u"))
  :bind
  ("C-x o" . switch-window)
  ("C-x 1" . switch-window-then-maximize)
  ("C-x 2" . switch-window-then-split-below)
  ("C-x 3" . switch-window-then-split-right)
  ("C-x 0" . switch-window-then-delete))

;; Use rofi as buffer switcher when available
(when (executable-find "rofi")
  (defun rofi-switch-buffer ()
    (interactive)
    (let*
        ((buffers (mapcar #'buffer-name (buffer-list)))
         (other (buffer-name (other-buffer)))
         (buffers (-filter #'(lambda (a) (not (eq a other))) buffers))
         (buf (shell-command-to-string
               (concat "echo " "\"" (string-join (cons other buffers) "\n") "\" | rofi -dmenu -i -p \"Buffer\" | tr -d '\n'"))))
      (switch-to-buffer buf)))

  (global-set-key (kbd "C-x b") 'rofi-switch-buffer))

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
  (("C-c c" . 'avy-goto-word-or-subword-1)
   ("M-SPC" . 'avy-goto-char)
   ("C-c SPC" . 'avy-goto-char-2)
   ("C-c C-s" . 'avy-goto-char-timer)
   ("M-'" . 'avy-goto-line))
  :config
  (setq avy-background t))

;; Ido setup
(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  (ido-mode t))

;; Helm setup
(use-package helm)

(use-package smex
  :bind
  ("M-x" . 'smex)
  ("M-X" . 'smex-major-mode-commands))

;; Rainbow delimiters setup
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; AI

;;; gptel

(use-package gptel
  :config
  (setq
   gptel-model "deepseek-coder-v2"
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '("deepseek-coder-v2"))))

(defun mewa/add-defs-in-region-to-context (start end)
  "Find definitions for all symbols in region between START and END."
  (save-excursion
    (goto-char start)
    ;; (cl-loop for (file) in gptel-context--alist
    ;;          collect (gptel-add-file file))
    (setq gptel-context--alist nil)
    (while (< (point) end)
      (when-let* ((symbol (thing-at-point 'symbol))
                  (bounds (bounds-of-thing-at-point 'symbol))
                  (locs (lsp-request "textDocument/definition"
                        (lsp--text-document-position-params))))
        (when locs
          (gptel-add-file (lsp--uri-to-path (gethash "uri" locs))))
        (goto-char (cdr bounds)))
      (forward-char 1))))

;; For current defun/block:
(defun add-defs-in-current-block-to-context ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (when bounds
      (mewa/add-defs-in-region-to-context (car bounds) (cdr bounds)))))

;; aider

;; Emacs < v30
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package aider-mode
  :vc (:fetcher  github :repo mewa/aider.el))

;; Emacs >= v30
;; (use-package aider-mode
;;   :vc (:url "https://github.com/mewa/aider.el"))


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

;; git integration
(use-package diff-hl
  :init
  (custom-set-faces
  '(diff-hl-change ((t (:background "#a18330"))))
  '(diff-hl-insert ((t (:background "#528e52"))))
  '(diff-hl-delete ((t (:background "#a03b3b"))))
  )
  :custom
  (diff-hl-margin-symbols-alist '((insert . " ") (delete . " ") (change . " ") (unknown . " ") (ignored . " ")))
  :config
  (diff-hl-margin-mode)
  (global-diff-hl-mode))

(use-package magit)

;;

(add-to-list 'auto-mode-alist '("\\.env.*\\'" . conf-mode))

;; Liquid mode
(use-package liquid-mode
  :vc (:url "https://github.com/doitalldev/liquid-mode"))

;; Scala setup
(use-package sbt-mode)

(use-package scala-mode)

;; Haskell setup
(use-package haskell-mode
  :config
  (interactive-haskell-mode t))

;; Lisp setup
(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :after (slime))

;; Elixir setup

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(elixir-ts-mode . "elixir")))

(use-package elixir-ts-mode
  :custom
  (lsp-elixir-ls-version "v0.23.0")
  (lsp-elixir-ls-download-url "https://github.com/elixir-lsp/elixir-ls/releases/download/v0.23.0/elixir-ls-v0.23.0.zip")
  :hook (elixir-ts-mode . lsp-deferred)
  :config
  (cl-letf (
            ((symbol-function 'y-or-n-p) (lambda (p) t))
            )
    (when (not (treesit-language-available-p 'elixir t))
      (elixir-ts-install-grammar))))


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

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; JavaScript setup
(use-package js2-mode
  :mode ("\\.js$" . js2-mode))

(use-package vue-mode
  :config
  (setq indent-tabs-mode nil js-indent-level 2)
  (setq css-indent-offset 2))


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


;;; Infra setup

;; Terraform setup
(use-package terraform-mode)

;; Dockerfile setup
(use-package dockerfile-mode)

;; Kubernetes + LSP setup
(defun mewa/enable-kubernetes-yaml ()
  (interactive)
  (lsp-yaml-set-buffer-schema "Kubernetes"))

(use-package yaml-mode
  :custom
  ('lsp-yaml-schemas '("https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.22.9/all.json" "*.yaml"))
  :hook
  ((yaml-mode . lsp-deferred)
   (lsp-deferred . mewa/enable-kubernetes-yaml))
  :bind
  (:map yaml-mode-map
        ("C-M-i" . #'company-complete-common)))

;; Rust setup
(use-package rust-mode)

;; Front-end
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; Indent with spaces
(setq-default indent-tabs-mode nil)

;; Setup line numbering
(global-display-line-numbers-mode t)
(display-line-numbers-mode t)

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
