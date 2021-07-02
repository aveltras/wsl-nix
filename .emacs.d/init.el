(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq default-directory "~/")

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq-default
 inhibit-startup-screen t
 auto-save-visited-interval 1
 make-backup-files nil
 epa-pinentry-mode 'loopback
 scroll-margin 10
 scroll-step 1
 scroll-conservatively 10000
 scroll-preserve-screen-position 1
 create-lockfiles nil)

(load-library "iso-transl")
(toggle-scroll-bar -1)
(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode -1)
(column-number-mode t)
(delete-selection-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode)
;; (global-tab-line-mode)

(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-16"))
(setq-default left-margin-width 1 right-margin-width 1)
(setq-default line-spacing 5)
(setq header-line-format " ")
(setq dired-listing-switches "-lAXGhv --group-directories-first")
;; (setq tab-always-indent 'complete)
(setq visible-bell 1)

(setq project-switch-commands
      '((magit-project-status "Magit" nil)
	(project-find-file "Find file" nil)
	(project-find-regexp "Find regexp" nil)
	(project-dired "Dired" nil)
	(project-vc-dir "VC-Dir" nil)
	(project-eshell "Eshell" nil)))

(global-set-key (kbd "C-x é") 'split-window)
(global-set-key (kbd "C-x \"") 'split-window-horizontally)
(global-set-key (kbd "C-x &") 'delete-other-windows)
(global-set-key (kbd "C-x à") 'delete-window)
(global-set-key (kbd "M-o") 'other-window)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Custom commands

(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun my/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun my/dired-subtree-toggle ()
  (interactive)
  (dired-subtree-toggle)
  (revert-buffer))

(defun my/pop-local-mark-ring ()
  (interactive)
  (set-mark-command t))

(defun my/dired-find-file ()
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (ido-find-file)))

(global-set-key (kbd "C-<tab>") 'my/switch-to-last-buffer)
(global-set-key (kbd "C-<home>") 'pop-global-mark)
(global-set-key (kbd "<home>") 'my/pop-local-mark-ring)

;; Third party packages

(use-package ace-window)

(use-package all-the-icons
  :if (display-graphic-p)
  
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package bazel)

(use-package consult
  :bind (("C-x b" . consult-buffer)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s i" . consult-imenu)
	 ("M-g g" . consult-goto-line))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-root-function #'projectile-project-root))

(use-package consult-flycheck
  :after consult flycheck
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package consult-lsp
  :after consult lsp-mode
  :bind ("M-²" . consult-lsp-symbols))

(use-package corfu
  ;; :config (global-set-key (kbd "M-i") 'corfu-complete)
  ;; :bind (:map corfu-map
	      ;; ("M-i" . corfu-complete))
  :config
  (corfu-global-mode)
  (setq corfu-cycle t))

(use-package ctrlf
  :config
  (ctrlf-mode +1))

(use-package dired-filter)

(use-package dired-subtree
  :bind (:map dired-mode-map
	      ("<tab>" . 'my/dired-subtree-toggle)
	      ("C-x C-f" . my/dired-find-file))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.95))))
  :config
  (setq doom-modeline-height 18)
  (setq doom-modeline-lsp t))

(use-package doom-themes
  :config
  (load-theme 'doom-shades-of-purple t))

(use-package embark
  :bind (("C-S-a" . embark-act)
	 ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after consult embark
  :demand t
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package emmet-mode
  :hook (web-mode css-mode html-mode))

(use-package envrc
  :init (envrc-global-mode))

(use-package expand-region
  :bind (("C-+" . er/contract-region)
         ("C-=" . er/expand-region)))

(use-package flycheck)

(use-package forge
  :after magit)

;; (use-package git-gutter
;;   :config
;;   (global-git-gutter-mode t))

(use-package graphql-mode)

(use-package haskell-mode
  :config (setq haskell-process-type 'ghci))

(use-package json-mode
  :config (setq js-indent-level 2))

;; (use-package lsp-haskell
;;   :hook
;;   (haskell-mode . lsp)
;;   (haskell-literate-mode . lsp)
;;   (haskell-cabal-mode . cabal-fmt-on-save-mode)
;;   :config
;;   (setq lsp-haskell-formatting-provider "ormolu")
;;   (setq lsp-haskell-server-path "haskell-language-server-wrapper")
;;   (setq lsp-haskell-process-args-hie '()))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (before-save . lsp-format-buffer)
  :config (setq lsp-modeline-diagnostics-scope :project)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t))

(use-package lsp-ui
  :config (global-set-key (kbd "M-p") 'lsp-ui-sideline-apply-code-actions)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package magit
  :hook (magit-mode . (lambda()
			(local-unset-key (kbd "<C-tab>"))))
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(use-package marginalia
  :config (marginalia-mode))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(use-package nix-mode
  :hook (nix-mode . nixpkgs-fmt-on-save-mode)
  :mode "\\.nix\\'")

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Code")))

(use-package rainbow-mode)

(use-package reformatter
  :config
  (reformatter-define cabal-fmt
    :program "cabal-fmt"
    :lighter " CabalFmt")
  (reformatter-define nixpkgs-fmt
    :program "nixpkgs-fmt"
    :lighter " NixpkgsFmt")
  (reformatter-define prettier-fmt-js
    :program "prettier"
    :args (list "--stdin-filepath" "file.js")
    :lighter " PrettierFmtJs")
  (reformatter-define prettier-fmt-tsx
    :program "prettier"
    :args (list "--stdin-filepath" "file.tsx")
    :lighter " PrettierFmtTsx")
  (reformatter-define prettier-fmt-css
    :program "prettier"
    :args (list "--stdin-filepath" "file.css")
    :lighter " PrettierFmtCss")
  )

(use-package restclient)

(use-package rustic
  :config
  (setq rustic-format-on-save t))

(use-package savehist
  :init (savehist-mode))

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package terraform-mode)

(use-package tide
  :after typescript-mode flycheck
  :hook ((typescript-mode . tide-setup)
	 (typescript-mode . tide-hl-identifier-mode)
	 (before-save . tide-format-before-save)))

(use-package typescript-mode)

(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.hbs\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :hook
  (css-mode . prettier-fmt-css-on-save-mode)
  (web-mode . (lambda ()
		(when (string-equal "tsx" (file-name-extension buffer-file-name))
		  (tide-setup))))
    (web-mode . (lambda ()
		(when (string-equal "tsx" (file-name-extension buffer-file-name))
		  (prettier-fmt-tsx-on-save-mode))))
  ;; (web-mode . prettier-fmt-js-on-save-mode)
  :commands web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-part-face t)
  (setq web-mode-content-types-alist
	'(("jsx" . "\\.js[x]?\\'")))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(use-package which-key)

(use-package whole-line-or-region
  :init (whole-line-or-region-global-mode))

(use-package yaml-mode)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets)
