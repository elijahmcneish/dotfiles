;; Load paths
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "~/.emacs.d/lisp/init_org_mode.emacs") ;; org-mode settings

;; Package management 
(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ;; Redundant in emacs >= 28?
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(use-package auto-package-update)
(auto-package-update-now-async)


;; Backup files
(setq
    backup-by-copying t
    backup-directory-alist
    '(("." . "~/.emacs.bak"))
    delete-old-versions t
    kept-new-versions 6
    version-control t)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Desktop settings
(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq desktop-buffers-not-to-save
      (concat "\\("
	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	      "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
;;; desktop-override-stale-locks.el begins here
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t)
          pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))


;; Change yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)


;; Don't confirm on exit
(setq confirm-kill-emacs nil)


;; Basic visual settings
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      mouse-autoselect-window t
      x-underline-at-descent-line t
      visible-bell t
      )
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(global-prettify-symbols-mode 1)
;; (set-frame-parameter nil 'background-mode 'dark)
;; (set-terminal-parameter nil 'background-mode 'dark)
;; (custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))


;; Theme
(use-package solarized-theme)
(setq
 solarized-scale-org-headlines nil
 solarized-use-variable-pitch nil
 )

(load-theme 'solarized-dark t)

;; Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse t) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


;; Display line numbers
(global-display-line-numbers-mode)


;; Word-count mode--need to enable with M-x wc-mode
(use-package wc-mode)


;; Highlight entire line in dired
(use-package stripe-buffer)
(add-hook 'dired-mode-hook 'stripe-listify-buffer)


;; Terminal settings
;; (setq explicit-shell-file-name "/usr/bin/fish")
;; (setq mm-external-terminal-program "urxvt")


;; ;; zsh-like minibuffer completion
;; (use-package zlc)
;; (zlc-mode t)


(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  )

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (setq enable-recursive-minibuffers t)
  )

(use-package marginalia
  :bind (("M-A" . marginalia-cycle))
  :custom
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '(file (styles basic basic-remote partial-completion)))
  :init
 )

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :if (display-graphic-p)
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode)
  )

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :init
  )

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  )

(use-package company
  :init
  (global-company-mode)
  )

;; Custom keybindings
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
;; (global-set-key (kbd "<f8>") 'ucs-insert)    ;; Insert Unicode character.
(global-set-key (kbd "<C-tab>") 'switch-to-buffer)
(global-set-key (kbd "<backtab>") 'other-window)
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
;; (keyboard-translate ?\C-h ?\C-?) ;; C-h to backspace. F1 for help.


;; Insert newline at end of buffer with C-n
(setq next-line-add-newlines t)


;; Allow opening files with sudo
(setq tramp-default-method "ssh")


;; Web browser settings
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)


;; Line wrapping
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)


;; Enable mouse when running with "emacs -nw"
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)


;; Show week numbers in calendar
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))


;; MacOS settings
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Hack Nerd Font Mono")
  (set-face-attribute 'default nil :height 140)
  (setq mac-command-modifier 'control
	mac-option-modifier 'meta
	mac-pass-command-to-system nil
	mac-pass-control-to-system nil
	explicit-shell-file-name "/opt/homebrew/bin/fish"
	)
  )


(use-package which-key)
(use-package lsp-mode
  :after which-key
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company company-mode orderless doom-modeline spaceline-all-the-icons spaceline marginalia lsp-mode which-key zlc use-package stripe-buffer solarized-theme)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
