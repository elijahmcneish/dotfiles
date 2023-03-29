;; Org-Mode settings


;; Keybindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)


;; TODO states
(setq org-todo-keywords
      (quote ((sequence "TODO(t!)" "STARTED(s!)" "WAITING(w!)" "APPT(a!)" "WORK(k!)" "CLASS(x!)" "FIXME(e!)"
			"DELEGATED(l!)" "INFO(i)" "|" "DONE(d!)" "CANCELLED(c!)" "DEFERRED(f!)" "WONTFIX(n!)")
)))


;; Capture templates
(setq org-capture-templates
      '(("t" "Task" entry (file+headline org-default-notes-file "TASKS")
	 "* TODO %?\n  - Captured %U\n")
      ("a" "Appointment" entry (file+headline org-default-notes-file "APPOINTMENTS")
	 "* APPT %?\n  - Captured %U\n")
      ("w" "Work" entry (file+headline org-default-notes-file "WORK")
       "* WORK %?\n")))


;; Org variables
(setq org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-default-notes-file "~/Dropbox/org/todo.org")
(setq
 ;; org-default-notes-file '(concat org-directory "/todo.org")
 ;; org-default-notes-file '("~/Dropbox/org/todo.org")
 org-agenda-ndays-to-span '7
 org-deadline-warning-days '0
 org-agenda-show-all-dates 't
 org-agenda-skip-deadline-if-done 't
 org-agenda-skip-scheduled-if-done 't
 org-agenda-skip-deadline-prewarning-if-scheduled 't
 org-agenda-start-on-weekday 'nil ;; Start on current day
 org-startup-folded 't
 org-tab-follows-link 't
 calendar-christian-all-holidays-flag 't
 holiday-bahai-holidays 'nil
 holiday-hebrew-holidays 'nil
 holiday-islamic-holidays 'nil
 )

;; Google Calendar syncing
(use-package oauth2)
(use-package org-caldav
 :after oauth2
 :init
 (load "~/.emacs.d/lisp/org_caldav_secrets.emacs")
;;  (org-caldav-sync)
 )

;; (use-package request)
;; (use-package alert)
;; (use-package persist)
;; (use-package aio)
;; (load "emacs-oauth2-auto")
;; (require 'oauth2-auto)
;; (use-package org-gcal
;;   :after (request alert persist aio)
;;   :init
;;  (setq plstore-cache-passphrase-for-symmetric-encryption t)
;;   (load "~/.emacs.d/lisp/org_caldav_secrets.emacs")
;;   )
