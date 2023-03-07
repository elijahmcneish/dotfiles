;; Load paths
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "~/.emacs.d/lisp/init_org_mode.emacs") ;; org-mode settings


;; Package management 
(require 'package)
(setq package-archives '(;; ("gnu" . "https://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)


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
      solarized-scale-org-headlines nil
      solarized-use-variable-pitch nil
      )
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(load-theme 'solarized-dark t)
;; (set-frame-parameter nil 'background-mode 'dark)
;; (set-terminal-parameter nil 'background-mode 'dark)
;; (custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))


;; Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse t) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


;; Display line numbers
(global-linum-mode)


;; Word-count mode--need to enable with M-x wc-mode
(require 'wc-mode)


;; Highlight entire line in dired
(require 'stripe-buffer)
(add-hook 'dired-mode-hook 'stripe-listify-buffer)


;; Terminal settings
;; (setq explicit-shell-file-name "/usr/bin/fish")
;; (setq mm-external-terminal-program "urxvt")


;; zsh-like minibuffer completion
(require 'zlc)
(zlc-mode t)


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
	mac-pass-command-to-system 'nil
	mac-pass-control-to-system 'nil
	explicit-shell-file-name "/opt/homebrew/bin/fish"
	)
  )


(provide 'init)
;;; init.el ends here
