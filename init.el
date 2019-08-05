(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;   '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package' and friends
(unless  (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'delight))

(eval-when-compile
  (require 'use-package))

(setq x-underline-at-descent-line t)

(setq indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(setq enable-recursive-minibuffers t)
(setq column-number-mode t)

(setq confirm-kill-processes nil)
(setq confirm-kill-emacs #'y-or-n-p)

(setq enable-local-variables :safe)

(setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)

(set-face-attribute 'default nil :height 120)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
  
  \(fn arg char)"
  'interactive)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-auto-revert-mode 1)
(show-paren-mode 1)
(electric-indent-mode 1)
(delete-selection-mode 1)
(winner-mode 1)
(global-hl-line-mode 1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Useless command and easy to type by accident
(define-key global-map (kbd "C-x ;") #'ignore)

(define-key global-map (kbd "M-z") #'zap-up-to-char)
(define-key global-map (kbd "C-x C-b") #'ibuffer)

(define-key global-map (kbd "C-c o") #'mode-line-other-buffer)
(define-key global-map (kbd "C-c u") #'browse-url-at-point)

(windmove-default-keybindings)

(setq help-window-select t)

(defun my-inhibit-electric-pair-mode (_char)
  (minibufferp))

(defun my-reload-init-file ()
  (interactive)
  (load-file user-init-file))

(define-key global-map (kbd "C-c y") #'my-reload-init-file)

(defun my-find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(define-key global-map (kbd "C-c u") #'my-find-user-init-file)

(defun my-mark-line ()
  "Mark the current line or extend the region to next EOL"
  (interactive)
  (unless (region-active-p)
    (push-mark (line-beginning-position) nil t))
  (if (= (point) (line-end-position))
    (end-of-line 2)
    (end-of-line)))

(define-key global-map (kbd "C-;") #'my-mark-line)

;; Put autosave files in their own directory
(setq backup-directory-alist
  `(("." . ,(concat user-emacs-directory "backups"))))

(define-key global-map (kbd "C-c d") #'dired)

(use-package shackle
  :ensure t
  :config (shackle-mode 1))

(defun my-projectile-after-switch-handler ()
  (interactive)
  (save-selected-window
    (treemacs-add-and-display-current-project)))

(use-package projectile
  :ensure t
  :demand t
  :delight
  :bind (("C-x p" . projectile-command-map)
          ("C-c f" . projectile-find-file))
  :init
  (if (string-equal system-type "windows-nt")
    (setq projectile-indexing-method 'native)
    (setq projectile-indexing-method 'alien))
  :config
  (projectile-mode 1)
  (add-hook 'projectile-after-switch-project-hook #'my-projectile-after-switch-handler)
  (setq projectile-switch-project-action #'helm-projectile))

(use-package helm-projectile
  :ensure t
  :demand t
  :bind (("C-c p" . helm-projectile-switch-project))
  :after (projectile)
  :config
  (helm-projectile-on))

(use-package restart-emacs
  :ensure t)

(use-package helm
  :ensure t
  :demand t
  :delight helm-mode
  :after (shackle)
  :bind (("M-x" . helm-M-x)
	  ("C-c i" . helm-imenu)
	  ("C-c r" . helm-recentf)
	  ("C-x C-f" . helm-find-files)
	  ("C-x b" . helm-mini)
	  ("M-y" . helm-show-kill-ring))
  :init
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-buffer-max-length 50)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-mode-handle-completion-in-region nil)
  (setq helm-display-function #'pop-to-buffer) ; For Shackle compatibility
  :config
  (add-to-list 'shackle-rules '("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 20))
  (define-key helm-find-files-map
    [(control backspace)] #'helm-find-files-up-one-level) 
  (define-key helm-read-file-map 
    [(control backspace)] #'helm-find-files-up-one-level))

(use-package helm-config
  :ensure helm
  :config
  (helm-mode 1))

(use-package intero
  :ensure t
  :after (flycheck company)
  :hook (haskell-mode . intero-mode))

(use-package undo-tree
  :ensure t
  :delight undo-tree-mode
  :config
  (global-undo-tree-mode 1))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package company
  :ensure t
  :init
  (setq company-tooltip-align-annotations t)
  :config
  (add-hook 'after-init-hook #'global-company-mode))

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package racer
  :ensure t
  :hook (rust-mode . racer-mode)
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package aggressive-indent
  :ensure t
  :hook (scheme-mode . aggressive-indent-mode))

(use-package fill-column-indicator
  :ensure t
  :init
  (setq fci-rule-column 80)
  ;; (setq fci-rule-width 2)
  :config
  (add-hook 'prog-mode-hook #'turn-on-fci-mode))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))

;;; Scheme stuff

(use-package rainbow-delimiters
  :ensure t
  :hook (scheme-mode . rainbow-delimiters-mode))

(setq scheme-program-name "csi -:c")
(setq lisp-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))

(defun my-init-scheme-mode ()
  (setq indent-tabs-mode nil))

(add-hook 'scheme-mode-hook #'my-init-scheme-mode)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  ;; https://github.com/magit/magit/wiki/Pushing-with-Magit-from-Windows
  (if (string-equal system-type "windows-nt")
    (setenv "GIT_ASKPASS" "git-gui--askpass")))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         t ; Default nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (treemacs--find-python3))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
        (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(defun my-shell-wsl ()
  (interactive)
  (let ((shell-file-name "C:\\Windows\\System32\\bash.exe" ))
    (setenv "EMACS" "1")
    (setenv "WSLENV" "EMACS")
    (shell "*wsl*")))

(if (string-equal system-type "windows-nt")
  (define-key global-map (kbd "C-c w") #'my-shell-wsl))

(setq sj-shell-clear-regex "clear")

(defun sj-shell-clear-next-output (output)
  "Clear the next output from ComInt and remove this hook."
  (remove-hook 'comint-preoutput-filter-functions #'sj-shell-clear-next-output)
  (recenter-top-bottom 0) output)

(defun sj-shell-clear-listener (input)
  (when (string-match-p sj-shell-clear-regex (string-trim input))
    (add-hook 'comint-preoutput-filter-functions #'sj-shell-clear-next-output)))

(use-package comint
  :init
  (setq comint-prompt-read-only t)
  (setq comint-scroll-show-maximum-output nil))

(use-package shell
  :after comint
  :config
  (add-hook 'shell-mode-hook
    #'(lambda ()
                (add-hook 'comint-input-filter-functions
                  #'sj-shell-clear-listener nil t))))

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-strict-mode)
  :hook (smartparens-mode . sp-use-smartparens-bindings)
  :bind (:map smartparens-mode-map
	  ("M-(" . sp-wrap-round)))

(use-package smartparens-config
  :ensure smartparens)

;; JavaScript and friends

(defun my-init-js-mode ()
  (interactive)
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2))

(add-hook 'js-mode-hook #'my-init-js-mode)

(use-package avy
  :ensure t
  :bind (("M-g w" . avy-goto-word-1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(ansi-color-faces-vector
     [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (zenburn)))
  '(custom-safe-themes
     (quote
       ("c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f" default)))
 '(helm-mode t)
  '(package-selected-packages
     (quote
       (aggressive-indent minions nyan-mode fill-column-indicator zenburn-theme rainbow-delimiters racer racer-mode rust-mode treemacs-magit magit treemacs-icons-dired treemacs-projectile treemacs helm-projectile projectile flycheck undo-tree intero scheme-complete restart-emacs helm shackle delight use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
