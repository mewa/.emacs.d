
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq package-list '(company company-flx monokai-theme haskell-mode company-ghc rainbow-mode markdown-mode markdown-preview-mode rainbow-delimiters))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.5)
 '(custom-safe-themes
   (quote
    ("3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" default)))
 '(package-selected-packages
   (quote
    (elm-mode yaml-mode company-ghc jsx-mode flx-isearch monokai-theme company-flx org company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'after-init-hook 'global-company-mode)
(company-flx-mode 1)

;; Haskell company-mode setup
(add-hook 'haskell-mode-hook 'ghc-comp-init)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'company-mode-hook
	  (lambda () (add-to-list 'company-backends 'company-ghc)))
(setq haskell-process-type 'stack-ghci)

(defun my-elm-oracle-complete (arg)
  (let* (
	 (json-object-type 'plist)
	 (default-directory (elm--find-dependency-file-path))
	 (file (elm--buffer-local-file-name))
	 (command (s-join " " (list "elm-oracle" file arg)))
	 (json (json-read-from-string (shell-command-to-string command)))
	 (getName (lambda (e) (plist-get e :name)))
	 )
    (mapcar getName json)
   ))

(defun my-comp-elm (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'my-comp-elm))
    (prefix (when (eq major-mode 'elm-mode) (company-grab-symbol)))
    (candidates (my-elm-oracle-complete arg))
    (meta (elm-oracle--completion-signature arg))
    )
  )


(with-eval-after-load 'company
  (add-to-list 'company-backends '(company-dabbrev-code :with my-comp-elm)))
;; (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

(load-theme 'monokai)

;; set up line numbering
(setq linum-format "%d ")
(global-linum-mode 1)
(subword-mode 1)
(menu-bar-mode 0)

(fset 'yes-or-no-p 'y-or-n-p)

(show-paren-mode 1)
(electric-pair-mode 1)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(global-set-key (kbd "C-c f") 'ff-find-other-file)

(add-hook 'org-mode-hook
	  (lambda () (require 'ox-md)))
