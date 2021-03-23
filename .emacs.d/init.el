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

(add-to-list 'default-frame-alist '(font . "Iosevka-18"))
(setq-default left-margin-width 1 right-margin-width 1)
(setq-default line-spacing 1)
(setq header-line-format " ")
(setq dired-listing-switches "-lXGh --group-directories-first")

(global-set-key (kbd "C-x é") 'split-window)
(global-set-key (kbd "C-x \"") 'split-window-horizontally)
(global-set-key (kbd "C-x &") 'delete-other-windows)
(global-set-key (kbd "C-x à") 'delete-window)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Custom commands

(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun my/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-key (kbd "C-<tab>") 'my/switch-to-last-buffer)

(defun my/dired-subtree-toggle ()
  (interactive)
  (dired-subtree-toggle)
  (revert-buffer))

;; Third party packages

(use-package ace-window)

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package chocolate-theme
  :config (load-theme 'chocolate t))

(use-package company
  :hook (after-init . global-company-mode)
  :config (setq company-minimum-prefix-length 1
		company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :after company prescient
  :config (company-prescient-mode))

(use-package counsel
  :config
  (ivy-mode 1)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-h l") 'counsel-find-library)
  (global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "C-h u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-c n") 'counsel-fzf)
  (global-set-key (kbd "C-c J") 'counsel-file-jump)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel-projectile
  :after projectile counsel
  :config
  (counsel-projectile-mode 1))

(use-package dired-subtree
  :bind (:map dired-mode-map
	      ("<tab>" . 'my/dired-subtree-toggle)))

(use-package direnv
  :config (direnv-mode))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; (setq doom-themes-treemacs-enable-variable-pitch nil)

  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.95))))
  :config
  (setq doom-modeline-height 18)
  (setq doom-modeline-lsp t))

(use-package emmet-mode
  :hook (web-mode css-mode html-mode))

(use-package expand-region
  :bind (("C-+" . er/contract-region)
         ("C-=" . er/expand-region)))

(use-package flycheck)

(use-package graphql-mode)

(use-package haskell-mode
  :config (setq haskell-process-type 'ghci))

(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(use-package ivy-prescient
  :after counsel prescient
  :config (ivy-prescient-mode))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package json-mode
  :config (setq js-indent-level 2))

(use-package lsp-haskell
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp)
  :config
  (setq lsp-haskell-formatting-provider "ormolu")
  (setq lsp-haskell-server-path "haskell-language-server")
  (setq lsp-haskell-process-args-hie '()))

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

(use-package lsp-ivy
  :after counsel
  :commands lsp-ivy-workspace-symbol)

(use-package magit
  :config (global-set-key (kbd "C-x g") 'magit-status))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package prescient
  :config (prescient-persist-mode))

(use-package prettier
  :hook (after-init . global-prettier-mode))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Code")))

(use-package rainbow-mode)

(use-package restclient)

(use-package rustic
  :config
  (setq rustic-format-on-save t))

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package typescript-mode)

(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.hbs\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :hook (web-mode . (lambda ()
		      (when (string-equal "tsx" (file-name-extension buffer-file-name))
			(tide-setup))))
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
