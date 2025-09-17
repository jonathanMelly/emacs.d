;;; Elpaca bootstrap
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;uncomment to update...
;(setq elpaca-lock-file (expand-file-name "elpaca.lock" user-emacs-directory))


;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
  
;;; Magit
;/!\ early as magit is using a new transient version
(use-package transient :ensure (:wait t))
(use-package magit 
    :ensure t
    :config
    (setq magit-define-global-key-bindings "recommended")
    ;(setq magit-auto-fetch t) does it exist ?
    
    ;; Store the hooks that are REMOVED for fast mode
    (defvar magit-removed-hooks-for-fast
      '(magit-insert-head-branch-header
        magit-insert-upstream-branch-header
        magit-insert-push-branch-header
        magit-insert-tags-header
        magit-insert-status-headers
        magit-insert-unpushed-to-pushremote
        magit-insert-unpushed-to-upstream-or-recent
        magit-insert-unpulled-from-pushremote
        magit-insert-unpulled-from-upstream))
    
    ;; Start in fast mode
    (dolist (hook magit-removed-hooks-for-fast)
      (remove-hook 'magit-status-sections-hook hook))
    
    ;; Toggle and refresh function
    (defun magit-toggle-fast-status-and-refresh ()
      "Toggle between fast and full magit status, then refresh."
      (interactive)
      (if (memq 'magit-insert-head-branch-header magit-status-sections-hook)
          ;; Currently full, switch to fast
          (progn
            (dolist (hook magit-removed-hooks-for-fast)
              (remove-hook 'magit-status-sections-hook hook))
	    (setq magit-commit-show-diff nil)
            (message "Magit fast status enabled"))
        ;; Currently fast, switch to full  
        (progn
          (dolist (hook magit-removed-hooks-for-fast)
            (add-hook 'magit-status-sections-hook hook))
	  (setq magit-commit-show-diff t)
          (message "Magit full status enabled")))
      ;; Always refresh after toggling
      (magit-refresh))

    ;; Add key binding in magit-status-mode
    (with-eval-after-load 'magit
      (define-key magit-status-mode-map (kbd "a") #'magit-toggle-fast-status-and-refresh))
    
    :bind
    (("C-x g" . magit-status)
     ("C-c g" . magit-dispatch)
     ("C-c f" . magit-file-dispatch)))

;;; VC custom with treemacs
(defun my/treemacs-refresh-current-file ()
  "Refresh current file's VC state in treemacs."
  (when (and (treemacs-get-local-window) buffer-file-name)
    (treemacs-refresh-single-node buffer-file-name)))

(add-hook 'vc-checkin-hook #'my/treemacs-refresh-current-file)

;;source : flykeys.el
;; UTF-8 as default encoding
;(set-language-environment 'utf-8)
;(set-default-coding-systems 'utf-8)
;(set-keyboard-coding-system 'utf-8-unix)
;; add this especially on Windows, else python output problem
;(set-terminal-coding-system 'utf-8-unix)

;(set-language-environment "UTF-8")
;(setq locale-coding-system 'utf-8)
;(set-terminal-coding-system 'utf-8)
;(set-keyboard-coding-system 'utf-8)
;(set-selection-coding-system 'utf-8)
;(prefer-coding-system 'utf-8)

;; This is the main one - makes UTF-8 the preferred encoding
(prefer-coding-system 'utf-8)

;; On Windows, ensure new files default to UTF-8 (not latin1)
(set-default-coding-systems 'utf-8)



;;; Calendar
;;Add week number to calendar
(setq calendar-week-start-day 1          ; Week starts on Monday
      calendar-intermonth-text           ; Display week numbers
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-function-name-face))


;;; Org download
;; used for pasting image (download to file and add org link)
(use-package org-download :ensure t
 :bind (("C-c y" . org-download-clipboard))
 :config
 (setq org-download-method 'directory)
 (setq org-download-image-dir "./images/")
 (setq org-download-heading-lvl nil)
 (setq org-download-image-attr-list nil)
 (setq org-download-annotate-function (lambda (link) ""))
 
 (defun org-download-file-format-default (filename)
   "Generate filename with heading and timestamp."
   (let* ((heading (or (org-get-heading t t t t) "image"))
          (clean-heading (replace-regexp-in-string "[^a-zA-Z0-9]" "_" heading))
          (timestamp (format-time-string "%Y%m%d_%H%M%S"))
          (extension (file-name-extension filename t)))
     (format "%s_%s%s" clean-heading timestamp extension)))
 
 (setq org-download-file-format-function 'org-download-file-format-default))


;;; Org Capture
(defun my/coaching-file ()
  (setq my/coaching-student (read-string "Élève: "))
  (format "c:/Users/jonmelly.DGEP/OneDrive - Education Vaud/02-jmy/coaching/coaching-%s.org" my/coaching-student))
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file "~/org/inbox.org")
         "* TODO %^{Title}\nSCHEDULED: %^{Scheduled}t\n%i%?")
        ("c" "Coaching ETML" plain
         (file my/coaching-file)
         (file "c:/Users/jonmelly.DGEP/OneDrive - Education Vaud/02-jmy/coaching/coaching-etml-template.org")
	 :no-save t
         :jump-to-captured t
	 :hook (org-fold-hide-drawer-all flyspell-mode)
	 :after-finalize (lambda () 
		(run-with-timer 0.5 nil 'org-fold-hide-drawer-all)
		(run-with-timer 0.5 nil 'flyspell-mode)
		)
	 )
	 
        ("n" "New note (with Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save nil
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured nil)
      )
)

(global-set-key (kbd "C-c c") 'org-capture)


;;; Org Agenda

(with-eval-after-load 'org
;; org todo keyword
  (setq org-todo-keywords
        '((sequence "TODO" "WIP" "|" "DONE" "DROP"))))

(setq org-refile-targets '(
			   (nil :maxlevel . 9);; nil means any header in the current file
                           (org-agenda-files :maxlevel . 9)
			   ;;maybe needed later (directory-files "~/org" t ".*\\.org$")
			   )
) ;; look for all agenda files

;;org files/folders defined in that file AND that file is relative to org-directory which by default is ~/org
(setq org-agenda-files "~/org/agenda-files.txt")

;;default ((agenda habit-down time-up urgency-down category-keep) (todo urgency-down category-keep) (tags urgency-down category-keep) (search category-keep))
(setq org-agenda-sorting-strategy '(deadline-up scheduled-up priority-down))

;; for column view mode, default ?
(setq org-columns-default-format "%50ITEM %TODO %SCHEDULED %DEADLINE %TAGS")

;;Shortcut
(global-set-key (kbd "C-c a") 'org-agenda)

;;; Org indent
(setq org-startup-indented t)



;;; Windows
(when (eq system-type 'windows-nt)
  (setq find-program "gfind") ;;use scoop unix tools shim...
  ;;on windows default emacs restart does not work and this only works for non daemon
  (use-package restart-emacs :ensure t)
  )


;;; Ripgrep
;; Basic rg.el setup without redundant default settings
(use-package rg :ensure t
  :config
  ;; Enable default keybindings under C-c s prefix
  ;;(rg-enable-default-bindings)
  (setq rg-custom-type-aliases
      (append rg-custom-type-aliases
              '(("denoteExts" . "*.md *.txt *.org"))))
  (rg-define-search rg-org
	:dir "~/org"
	:files "denoteExts"
	:format literal
	:flags ("--word-regexp")
	:menu ("Custom" "o" "Org"))

  ;;use ripgrep to enhance xref-search perf...
  (setq xref-search-program 'ripgrep)
  
  ;;:bind
)


;;; Denote
;;https://protesilaos.com/emacs/denote#h:998ae528-9276-47ec-b642-3d7355a38f27
;; Remember that the website version of this manual shows the latest
;; developments, which may not be available in the package you are
;; using.  Instead of copying from the web site, refer to the version
;; of the documentation that comes with your package.  Evaluate:
;;
;;     (info "(denote) Sample configuration")
(use-package denote :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ;("C-c n g" . denote-grep) ;replaced by consult-denote...
   )
  :config
  (setq denote-directory (expand-file-name "~/org/kb"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))


;;; denote explore
(use-package denote-explore
  ;;:ensure (:host github :repo "" :remotes ("fork" :repo "jonathanMelly/denote-explore"))
  :ensure t
  ;;custom:
  :bind
  (;; Statistics
   ("C-c e s n" . denote-explore-count-notes)
   ("C-c e s k" . denote-explore-count-keywords)
   ("C-c e s e" . denote-explore-barchart-filetypes)
   ("C-c e s w" . denote-explore-barchart-keywords)
   ("C-c e s t" . denote-explore-barchart-timeline)
   ;; Random walks
   ("C-c e w n" . denote-explore-random-note)
   ("C-c e w r" . denote-explore-random-regex)
   ("C-c e w l" . denote-explore-random-link)
   ("C-c e w k" . denote-explore-random-keyword)
   ;; Denote Janitor
   ("C-c e j d" . denote-explore-duplicate-notes)
   ("C-c e j D" . denote-explore-duplicate-notes-dired)
   ("C-c e j l" . denote-explore-missing-links)
   ("C-c e j z" . denote-explore-zero-keywords)
   ("C-c e j s" . denote-explore-single-keywords)
   ("C-c e j r" . denote-explore-rename-keywords)
   ("C-c e j y" . denote-explore-sync-metadata)
   ("C-c e j i" . denote-explore-isolated-files)
   ;; Visualise denote
   ("C-c e n" . denote-explore-network)
   ("C-c e r" . denote-explore-network-regenerate)
   ("C-c e d" . denote-explore-barchart-degree)
   ("C-c e b" . denote-explore-barchart-backlinks)))


;;; Notification
;;notifications
;;Currently using external ps1 script using BurntToast... + org wild notifier
;;Could be simplified (but losing os style notification) using cross-platform org-notify [https://elpa.gnu.org/packages/org-notify.html]     

(defun my/toast (info)
  "Display a PowerShell BurntToast notification using toast.ps1 script."
  (let* ((title (plist-get info :title))
         (message (plist-get info :message))
         (toast-script-path "c:\\ws\\code\\powershell\\toaster\\toast.ps1")
         (emacs-exe-path "C:\\ws\\apps\\scoop\\apps\\emacs\\current\\bin\\runemacs.exe"))
    
    ;; Check if paths exist
    (if (and (file-exists-p toast-script-path)
             (file-exists-p emacs-exe-path))
        ;; Paths exist, proceed with toast notification
        (let ((ps-command (format 
                          "powershell -Command \"& '%s' -Title '%s' -Message '%s' -AppID '%s'\""
                          toast-script-path
                          (replace-regexp-in-string "'" "''" title)
                          (replace-regexp-in-string "'" "''" message)
                          emacs-exe-path)))
          ;;(message ps-command)
          (start-process-shell-command "toast-notification" nil ps-command))
      
      ;; One or both paths don't exist, show prominent error
      (let ((missing-paths (cond
                           ((not (file-exists-p toast-script-path))
                            (if (not (file-exists-p emacs-exe-path))
                                "toast.ps1 and runemacs.exe"
                              "toast.ps1"))
                           ((not (file-exists-p emacs-exe-path))
                            "runemacs.exe")
                           (t ""))))
        ;; Display error in both message area and popup dialog
        (let ((error-msg (format "Cannot display toast notification!\nMissing path(s): %s" missing-paths)))
          (message "%s" error-msg)
          ;; Create a visible error popup
          (with-output-to-temp-buffer "*Toast Error*"
            (princ error-msg))
          ;; Make sure the buffer is displayed
          (display-buffer "*Toast Error*")
          ;; Also show a dialog box
          (x-popup-dialog
           t
           `("Toast Notification Error" 
             ("OK" . nil)
             nil
             "Missing required file(s) for toast notifications:"
             ,(format "Missing: %s" missing-paths)
             "Please check the file paths and try again.")))))))

;;set alert style (used by org wild notifier)
(use-package org-alert :ensure t
  :config
  (alert-define-style 'win-toast
                  :title "Persistent Windows Toast Notification"
                  :notifier #'my/toast)

  (setq alert-default-style 'win-toast)
  
)

;;activate notification of todos items
(use-package org-wild-notifier :ensure t
  :config
  (setq org-wild-notifier-alert-time '(1440 600 60 5 3))
  (setq org-wild-notifier-day-wide-alert-times '("07:00" "10:00" "12:00" "15:00" "17:30" "20:00"))
  (org-wild-notifier-mode 1)
  )


;;; Completion / Search / Minibuffer...

;;; Vertico
(use-package vertico :ensure (:wait t :files (:defaults "extensions/*"))
  :init
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (setq vertico-count 20) ;; Show more candidates
  (setq vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (setq vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  (vertico-mode)

  (vertico-multiform-mode 1)

  :config
  ;;Mimic which-key but with C-h
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
)

;;; Consult
;https://github.com/minad/consult?tab=readme-ov-file#narrowing-and-grouping
;DOC: ~/.emacs.d/elpaca/repos/consult/README.org
(use-package consult :ensure t
  :bind (
	 ;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
	 
	 ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s f" . consult-fd)                  ;; Alternative: consult-find
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
		 ("M-s R" . rg-menu)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
	 ("M-s s" . consult-eglot-symbols)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)
	 )
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  :config
  (setq consult-narrow-key "C-+");or < or ?
  ;consult - capf replacement
  (setq completion-in-region-function #'consult-completion-in-region)
  ;; Make TAB smart: indent first, then complete
  (setq tab-always-indent 'complete)
  ;; Make complete-symbol also use consult
  (advice-add 'complete-symbol :override #'completion-at-point)
  
  ;modified buffers (unsaved)
  (setq consult--source-modified-buffer
        `(:name "Modified"
          :narrow ?M
          :category buffer
	  :state consult--buffer-state
          :items ,(lambda ()
                    (mapcar #'buffer-name
                            (seq-filter 
                             (lambda (buf)
                               (and (buffer-modified-p buf)
                                    (buffer-file-name buf)))
                             (buffer-list))))))
  
  (add-to-list 'consult-buffer-sources 'consult--source-modified-buffer)

  ; Not realy checked if realy better
  ;but often rg crashes / blocks emacs
  (setq consult-ripgrep-args
	"rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number -M 500"); it 
  
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  )

(use-package consult-denote
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

;;; Orderless matching
(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex orderless-initialism))
  ;; Smart case: case-insensitive unless you type uppercase
  (orderless-smart-case t)  ; This makes it case-insensitive unless you type uppercase
  )

;to make eglot also ignore case...
(setq completion-ignore-case  t)

;;; Marginalia
(use-package marginalia :ensure t
  :init
  (marginalia-mode))

;;; Embark 
(use-package embark :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ;;The suggested alternative of M-. for embark-dwim is bound by default to xref-find-definitions. That is a very useful command but overwriting it with embark-dwim is sensible since in Embark’s default configuration, embark-dwim will also find the definition of the identifier at point. (Note that xref-find-definitions with a prefix argument prompts you for an identifier, embark-dwim does not cover this case).
;;   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("M-." . embark-dwim)        ;; 
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
 
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  
  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;;; Imenu
;;Customize imenu for easy navigation in this file
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local imenu-generic-expression
                        (append imenu-generic-expression
                                '(("Sections" "^;;; \\(.*\\)$" 1))))))


;;; Biome weather
(use-package biome :ensure t
:config
(setq biome-query-coords
    '(("Nyon, Suisse" 46.37962 6.25126)
      ("Dullive, Suisse" 46.42153, 6.30486)
      ("Lac de Joux, Suisse" 46.62324, 6.26139)))

(add-to-list 'biome-presets-alist
	     '("nyon" :normal
	       ((:name . "MeteoFrance") (:group . "hourly")
		(:params ("wind_speed_unit" . "kn")
			 ("longitude" . 6.25126)
			 ("latitude" . 46.37962) ("forecast_days" . 5)
			 ("hourly" "terrestrial_radiation_instant"
			   "cloud_cover" "rain"
			  "apparent_temperature"
			  "wind_direction_10m" "wind_gusts_10m" "wind_speed_10m")))))

(add-to-list 'biome-presets-alist
	     '("dullive" :normal
	       ((:name . "MeteoFrance") (:group . "hourly")
		(:params ("wind_speed_unit" . "kn")
			 ("longitude" . 6.30486)
			 ("latitude" . 46.42153) ("forecast_days" . 5)
			 ("hourly" "terrestrial_radiation_instant"
			   "cloud_cover" "rain"
			  "apparent_temperature"
			  "wind_direction_10m" "wind_gusts_10m" "wind_speed_10m")))))

(add-to-list 'biome-presets-alist
	     '("lac de joux" :normal
	       ((:name . "MeteoFrance") (:group . "hourly")
		(:params ("wind_speed_unit" . "kn")
			 ("longitude" . 6.26139)
			 ("latitude" . 46.62324) ("forecast_days" . 5)
			 ("hourly" "terrestrial_radiation_instant"
			   "cloud_cover" "rain"
			  "apparent_temperature"
			  "wind_direction_10m" "wind_gusts_10m" "wind_speed_10m")))))
)


;;; Desktop session save
(require 'desktop)
(setq desktop-globals-to-save
      (append '(file-name-history
                extended-command-history
                query-replace-history
                shell-command-history)
              desktop-globals-to-save))
;;(desktop-save-mode 1);;do it manually : desktop-save / desktop-read


;;; History
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))


;;; Winner mode (ease of use with windows)
(winner-mode 1)

;;; Emacs
;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))


;;; Theme
(load-theme 'modus-vivendi-tritanopia t)

;; hack font by default
(customize-set-variable 'default-frame-alist 
                       '((font . "Hack-12")))

;; symbols (for mood-line) and emojis
(set-fontset-font
 t 'symbol (font-spec :family "Segoe UI Symbol") nil 'append)

					;Just install Noto Emoji(https://fonts.google.com/noto/specimen/Noto+Emoji)  and it works out of the box!

;; minimal UI
(menu-bar-mode -1) ;; disables menubar
(tool-bar-mode -1) ;; disables toolbar (icons for open/save...)
;;(scroll-bar-mode -1) ;; disables scrollbar
(pixel-scroll-precision-mode 1) ;; enable smooth scrolling

;;(setq inhibit-splash-screen t ;; no thanks
        ;;use-file-dialog nil ;; don't use system file dialog
        ;;tab-bar-new-button-show nil ;; don't show new tab button
        ;;tab-bar-close-button-show nil ;; don't show tab close button
        ;;tab-line-close-button-show nil ;; don't show tab close button
;;)


;;; Modline Moodline

;; display time+load average...
;(display-time-mode 1)

;;https://gitlab.com/jessieh/mood-line
(use-package mood-line
  :ensure t
  :demand t
  :config 
	(mood-line-mode)
	;(setq mood-line-format mood-line-format-default-extended)
					;(setq mood-line-format mood-line-format-default)
	
	;; LEFT SIDE elements:
	;; - mood-line-segment-modal: Modal editing state (evil-mode, etc.)
	;; - mood-line-segment-buffer-status: Buffer read-only/modified status (* %)
	;; - mood-line-segment-client: Emacsclient indicator  
	;; - mood-line-segment-project: Current project name
	;; - mood-line-segment-buffer-name: Name of current buffer
	;; - mood-line-segment-anzu: Search match info ([3/12] style)
	;; - mood-line-segment-multiple-cursors: Multiple cursors count
	;; - mood-line-segment-cursor-position: Line and column numbers (L:C)
	;; - mood-line-segment-cursor-point: Buffer position in characters
	;; - mood-line-segment-region: Selected region info
	;; - mood-line-segment-scroll: Scroll position percentage

	;; RIGHT SIDE elements:
	;; - mood-line-segment-indentation: Indentation settings (spaces/tabs)
	;; - mood-line-segment-eol: End-of-line format (LF/CRLF)
	;; - mood-line-segment-encoding: File encoding (utf-8, etc.)
	;; - mood-line-segment-vc: Version control branch/status
	;; - mood-line-segment-major-mode: Current major mode
	;; - mood-line-segment-misc-info: Miscellaneous mode line info
	;; - mood-line-segment-checker: Syntax checker status (flycheck/flymake)
	;; - mood-line-segment-process: Running processes

	(setq mood-line-format
      '((
         (or (mood-line-segment-buffer-status) (mood-line-segment-client) " ")
         " " (mood-line-segment-project) "/"
         (mood-line-segment-buffer-name) " " (mood-line-segment-anzu) " "
         (mood-line-segment-cursor-position) ""
         #(":" 0 1 (face mood-line-unimportant))
         (mood-line-segment-cursor-point) " " (mood-line-segment-region)
         " " (mood-line-segment-scroll) "")
        (
	 " " (mood-line-segment-encoding) "/" (mood-line-segment-eol) " "
          " " (mood-line-segment-vc) "  "
         (mood-line-segment-major-mode) " " (mood-line-segment-misc-info)
         "  " (mood-line-segment-checker) "  " (mood-line-segment-process)
         " ")))
		 )

;;; Anzu to show matching count with isearch
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1)
  )
;(use-package minions
;  :ensure t
;  :demand t
;  :config
;  (minions-mode 1))

  
  
(setq show-trailing-whitespace t) ;;
(setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.
(electric-pair-mode 1)
(electric-quote-mode 1)


;;; Key bindings
;(global-set-key (kbd "<f1>") 'org-agenda-list)
;(global-set-key (kbd "<f2>") 'org-todo-list)
(global-set-key (kbd "<f3>") 'ace-window)
;(global-set-key (kbd "M-o") 'other-window) ;; tentative ?
(global-set-key (kbd "S-<f4>") 'kill-buffer)
(global-set-key (kbd "S-<f5>") 'diff-buffer-with-file)
;(global-set-key (kbd "S-<f6>") 'rg-menu) ;; 
;(global-set-key (kbd "S-<f10>") 'save-buffer)
(global-set-key (kbd "S-<f11>") 'toggle-frame-fullscreen) ;; as f11 is for @...


(global-set-key (kbd "C-c l") 'org-toggle-link-display)

;; Try to facilitate using emacs default keybindings with my custom bépo+kana in
;; which all these characters are typed with vkoem8 "kana" key which does not seem
;; to work with modifier keys...
(define-key key-translation-map [f5] (kbd "["))
(global-set-key (kbd "C-<f5>") (key-binding (kbd "C-[")))
(global-set-key (kbd "M-<f5>") (key-binding (kbd "M-[")))
(global-set-key (kbd "C-M-<f5>") (key-binding (kbd "C-M-[")))

(define-key key-translation-map [f6] (kbd "]"))
(global-set-key (kbd "C-<f6>") (key-binding (kbd "C-]")))
(global-set-key (kbd "M-<f6>") (key-binding (kbd "M-]")))
(global-set-key (kbd "C-M-<f6>") (key-binding (kbd "C-M-]")))

(define-key key-translation-map [f7] (kbd "{"))
(global-set-key (kbd "C-<f7>") (key-binding (kbd "C-}")))
(global-set-key (kbd "M-<f7>") (key-binding (kbd "M-}")))
(global-set-key (kbd "C-M-<f7>") (key-binding (kbd "C-M-}")))

(define-key key-translation-map [f8] (kbd "}"))
(global-set-key (kbd "C-<f8>") (key-binding (kbd "C-}")))
(global-set-key (kbd "M-<f8>") (key-binding (kbd "M-}")))
(global-set-key (kbd "C-M-<f8>") (key-binding (kbd "C-M-}")))

(define-key key-translation-map [f9] (kbd "<"))
(global-set-key (kbd "C-<f9>") (key-binding (kbd "C-<")))
(global-set-key (kbd "M-<f9>") (key-binding (kbd "M-<")))
(global-set-key (kbd "C-M-<f9>") (key-binding (kbd "C-M-<")))

(define-key key-translation-map [f10] (kbd ">"))
(global-set-key (kbd "C-<f10>") (key-binding (kbd "C->")))
(global-set-key (kbd "M-<f10>") (key-binding (kbd "M->")))
(global-set-key (kbd "C-M-<f10>") (key-binding (kbd "C-M->")))

(define-key key-translation-map [f11] (kbd "@"))
(global-set-key (kbd "C-<f11>") (key-binding (kbd "C-@")))
(global-set-key (kbd "M-<f11>") (key-binding (kbd "M-@")))
(global-set-key (kbd "C-M-<f11>") (key-binding (kbd "C-M-@")))

(define-key key-translation-map [f12] (kbd "%"))
(global-set-key (kbd "C-<f12>") (key-binding (kbd "C-%")))
(global-set-key (kbd "M-<f12>") (key-binding (kbd "M-%")))
(global-set-key (kbd "C-M-<f12>") (key-binding (kbd "C-M-%")))

;;;; Avoid issues with M-_ with custom kana altgr...
(keymap-global-set "C-!" 'undo-redo)


;;; Custom functions
;; dired filter by date range
(defun dired-filter-by-date-range (start-date end-date)
  "Show only files modified between START-DATE and END-DATE in dired."
  (interactive
   (list
    (read-string "Start date (YYYY-MM-DD): ")
    (read-string "End date (YYYY-MM-DD): ")))
  (let ((cmd (format "%s %s -type f -newermt \"%s\" ! -newermt \"%s\" | sort"
                    find-program default-directory start-date end-date)))
    (dired-other-window (concat "files from " start-date " to " end-date))
    (dired-mode)
    (setq-local dired-actual-switches cmd)
    (setq-local mode-line-process (concat " [" cmd "]"))
    (dired-readin)))

;; clean buffers (bad old reflex ?)
(defun my/clean-buffers ()
  "Kill all buffers except for *scratch*, *Messages*, and any other essential buffers."
  (interactive)
  (when (yes-or-no-p "Kill all buffers except essential ones? ")
  (let ((keep-buffers '("*scratch*" "*Messages*" "*dashboard*")))
    (dolist (buf (buffer-list))
      (let ((buf-name (buffer-name buf)))
        (unless (or (member buf-name keep-buffers)
                    (string-prefix-p " " buf-name)) ; Skip special buffers that start with space
          (kill-buffer buf)))))))

(defun my/toggle-org-fringe-arrows ()
  "Toggle fringe continuation arrows in org-mode buffer."
  (interactive)
  (if (and (derived-mode-p 'org-mode)
           (assq 'continuation fringe-indicator-alist))
      (progn
        (setq-local fringe-indicator-alist 
                    (assq-delete-all 'continuation fringe-indicator-alist))
        (message "Org fringe arrows disabled"))
    (if (derived-mode-p 'org-mode)
        (progn
          (setq-local fringe-indicator-alist
                      (cons '(continuation right-arrow left-arrow)
                            fringe-indicator-alist))
          (message "Org fringe arrows enabled"))
      (message "Not in org-mode buffer"))))


;;; Wakatime
(use-package wakatime-mode :ensure (:wait t))
(global-wakatime-mode)
(when (eq system-type 'windows-nt)
  (setq wakatime-cli-path "C:\\ws\\apps\\scoop\\apps\\python\\current\\Scripts\\wakatime.exe")
  )


;;; Graphviz
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))



;;; Aspell
(setq ispell-program-name "aspell")
;;(setq ispell-extra-args '("--sug-mode=ultra"));;best match (more cpu), others are fast (default) and normal
(setq ispell-dictionary "fr_CH")  ; default

;; Switch between English and French
(defun my/switch-lang ()
  (interactive)
  (let ((new (if (string-match "fr_CH" (or ispell-current-dictionary ispell-dictionary))
                 "en_US" "fr_CH")))
    (ispell-change-dictionary new)
    (message "Dictionary: %s" new)))

;;(add-hook 'text-mode-hook 'flyspell-mode)  
;;(add-hook 'org-mode-hook 'flyspell-mode)

;; Optional: Auto language detection
(use-package guess-language
  :ensure t
  :config
  (setq guess-language-languages '(en fr))
  ;; Map detected languages to your actual dictionaries
  (setq guess-language-langcodes
        '((en . ("en_US" "English"))
          (fr . ("fr_CH" "Français"))))  ; Map fr detection to fr_CH dictionary
  ;;(add-hook 'text-mode-hook #'guess-language-mode)
  ;;(add-hook 'org-mode-hook #'guess-language-mode) ;;deactivated too slow ?
  )

(defun consult-dictionary ()
  "Simple dictionary chooser between English US and French CH."
  (interactive)
  (let* ((dictionaries '("en_US" "fr_CH"))
         (choice (consult--read dictionaries
                               :prompt "Dictionary: "
                               :require-match t)))
    (ispell-change-dictionary choice)
    (message "Dictionary: %s" choice)))

;;; Flyspell consult/vertico integration

; this adds candidates + dictionary options (auto through vertico)
(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ;; Replace flyspell's built-in correction functions with flyspell-correct
              ("C-." . flyspell-correct-at-point)        ; was flyspell-auto-correct-word
              ("C-M-i" . flyspell-correct-at-point)      ; was flyspell-auto-correct-word (M-TAB)  
              ("C-c $" . flyspell-correct-wrapper))      ; was flyspell-correct-word-before-point
)

(use-package consult-flyspell
  :ensure t  
  :after (consult flyspell)
  :bind (:map flyspell-mode-map
              ("C-c C-s" . consult-flyspell))
  :config
  (setq consult-flyspell-select-function 'flyspell-correct-at-point
        ;consult-flyspell-set-point-after-word t ; this is default
        ;consult-flyspell-always-check-buffer nil) ; this is also default
	)
 )


;;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 64)


;;; Treesitter
;Language support with treesitter or regex mode
;; Built-in tree-sitter mode preferences (Emacs 29+)
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  ;; Set up language sources for built-in modes
  (setq treesit-language-source-alist
        '((rust "https://github.com/tree-sitter/tree-sitter-rust")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  
  ;; Prefer tree-sitter modes for supported languages
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))
  
;;; Markdown
;https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  
  :init
	(setq markdown-command '("pandoc" "--from=gfm" "--to=html5"))
	;(setq markdown-command "multimarkdown")
	(setq markdown-split-window-direction 'below)
	(setq markdown-live-preview-delete-export 'delete-on-export)
	
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)
			  ("C-c C-v d" . markdown-insert-gfm-code-block)
	          ("C-c C-v c" . markdown-insert-code)
	          ("C-<return>" . markdown-insert-header-respect-content)
              ("M-<left>" . markdown-promote)
              ("M-<right>" . markdown-demote)
              ("M-<up>" . markdown-move-up)
              ("M-<down>" . markdown-move-down)

	      ("C-c y" . markdown-paste-clipboard-image)
              ("C-c C-x C-v" . markdown-toggle-inline-images)
	      )
  ;; try to remove backtick electric as it prints weird chars...
  :hook (markdown-mode . (lambda () 
                           (setq-local electric-pair-pairs 
                                       (remove '(?` . ?`) electric-pair-pairs))))
									   
  :config
 
  (defun markdown-insert-header-respect-content ()
    "Insert a new header after the current section, respecting content like org-mode C-RET."
    (interactive)
    (let ((current-level (markdown-outline-level)))
      (if current-level
          (progn
            ;; Move to end of current section
            (markdown-end-of-subtree)
            (unless (bolp) (newline))
            (newline)
            ;; Insert header at same level
            (insert (make-string current-level ?#) " "))
        ;; If not in a header, just insert a level 1 header
        (end-of-line)
        (newline 2)
        (insert "# "))))
  )

(use-package denote-markdown :ensure t)

;;; markdown-smart-links.el --- Autocompletion intelligente de liens markdown

(defvar markdown-smart-links-search-paths
  '("." "~/Documents" "~/Notes")
  "Chemins de recherche pour les fichiers markdown.")

(defun markdown-smart-links--collect-md-files ()
  "Collecte tous les fichiers .md disponibles."
  (let ((files '()))
    ;; Projet actuel
    (when (and (fboundp 'project-current) (project-current))
      (setq files (append files 
                          (directory-files-recursively 
                           (project-root (project-current)) "\\.md$"))))
    ;; Buffers ouverts
    (dolist (buffer (buffer-list))
      (when (and (buffer-file-name buffer)
                 (string-match-p "\\.md$" (buffer-file-name buffer)))
        (push (buffer-file-name buffer) files)))
    ;; Chemins configurés
    (dolist (path markdown-smart-links-search-paths)
      (when (file-directory-p (expand-file-name path))
        (setq files (append files 
                            (directory-files-recursively 
                             (expand-file-name path) "\\.md$")))))
    ;; Historique récent
    (when (bound-and-true-p recentf-list)
      (dolist (file recentf-list)
        (when (string-match-p "\\.md$" file)
          (push file files))))
    (delete-dups files)))

(defun markdown-smart-links--extract-headings (file)
  "Extrait les headings d'un fichier markdown."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((headings '()))
        (goto-char (point-min))
        (while (re-search-forward "^\\(#+\\)\\s-+\\(.+\\)$" nil t)
          (let ((level (length (match-string 1)))
                (title (string-trim (match-string 2))))
            (push (cons (format "%s %s" (make-string level ?#) title) title) 
                  headings)))
        (nreverse headings)))))

(defun markdown-smart-links--format-candidate (file)
  "Formate un fichier pour l'affichage."
  (let* ((relative (file-relative-name file))
         (basename (file-name-nondirectory file))
         (dir (file-name-directory relative)))
    (concat basename
            (when dir (propertize (concat " (" dir ")") 'face 'font-lock-comment-face))
            (when (get-file-buffer file) 
              (propertize " [ouvert]" 'face 'success)))))

(defun markdown-smart-links--choose-heading (file)
  "Choisit un heading dans un fichier."
  (let ((headings (markdown-smart-links--extract-headings file)))
    (when headings
      (cdr (assoc (completing-read "Heading (optionnel): " 
                                  (cons '("" . "") headings) nil nil) 
                 headings)))))

(defun markdown-smart-links--get-link-at-point ()
  "Retourne les données du lien markdown à la position du curseur.
Retourne une plist avec :text, :url, :start, :end ou nil si pas de lien."
  (save-excursion
    (let ((pos (point))
          (result nil))
      ;; D'abord vérifier si on est dans un lien selon markdown-mode
      (when (markdown-link-p)
        ;; Chercher le lien qui contient le curseur
        (beginning-of-line)
        (while (and (not result) 
                    (re-search-forward "\\[\\([^]]*\\)\\](\\([^)]*\\))" (line-end-position) t))
          (let ((link-start (match-beginning 0))
                (link-end (match-end 0))
                (text (match-string 1))
                (url (match-string 2)))
            ;; Si le curseur est dans ce lien, on l'a trouvé
            (when (and (>= pos link-start) (<= pos link-end))
              (setq result (list :text (substring-no-properties text) 
                                :url (substring-no-properties url) 
                                :start link-start 
                                :end link-end)))))
        result))))

(defun markdown-smart-insert-link ()
  "Version intelligente de markdown-insert-link avec autocomplétion."
  (interactive)
  (let* ((link-data (markdown-smart-links--get-link-at-point))
         (existing-text (plist-get link-data :text))
         (existing-url (plist-get link-data :url))
         (link-start (plist-get link-data :start))
         (link-end (plist-get link-data :end))
         (files (markdown-smart-links--collect-md-files))
         (candidates (mapcar (lambda (f) 
                              (cons (markdown-smart-links--format-candidate f) f))
                            files))
         ;; Pré-remplir le champ avec l'URL existante pour permettre la modification
         (choice (completing-read "Lien vers: " candidates nil nil existing-url))
         (selected (cdr (assoc choice candidates)))
         ;; Si pas trouvé dans les candidats, vérifier si c'est un fichier existant
         (is-file (when (and (not selected) (not (string-empty-p choice)))
                   (let ((abs-path (if (file-name-absolute-p choice)
                                      choice
                                    (expand-file-name choice (file-name-directory (buffer-file-name))))))
                     (and (file-exists-p abs-path) 
                          (string-match-p "\\.mdx?$" abs-path)
                          abs-path)))))
    
    ;; Procéder seulement si on a une sélection valide
    (when (or selected is-file (not (string-empty-p choice)))
      
      (if (or selected is-file)
          ;; Fichier sélectionné - demander heading et texte
          (let* ((target-file (or selected is-file))
                 (heading (markdown-smart-links--choose-heading target-file))
                 (current-file (buffer-file-name))
                 (link-target (if current-file
                                 (file-relative-name target-file (file-name-directory current-file))
                               target-file))
                 (anchor (if (and heading (not (string-empty-p heading)))
                            (concat "#" (replace-regexp-in-string 
                                        "[^a-zA-Z0-9-]" "-" (downcase heading)))
                          ""))
                 ;; Pré-remplir le texte avec l'existant ou une valeur par défaut
                 (default-text (or existing-text
                                  (if (string-empty-p anchor)
                                      (file-name-base target-file) 
                                    heading)))
                 (text (read-string "Texte du lien (optionnel): " default-text))
                 (final-text (if (string-empty-p text)
                                default-text
                              text)))
            ;; Tout s'est bien passé, on peut maintenant supprimer l'ancien lien et insérer le nouveau
            (when (and link-start link-end)
              (delete-region link-start link-end))
            (insert (format "[%s](%s%s)" final-text link-target anchor)))
        
        ;; Saisie libre - insérer manuellement
        (let* ((text (read-string "Texte du lien (optionnel): " 
                                 (or existing-text choice)))
               (final-text (if (string-empty-p text) choice text)))
          ;; Tout s'est bien passé, on peut maintenant supprimer l'ancien lien et insérer le nouveau
          (when (and link-start link-end)
            (delete-region link-start link-end))
          (insert (format "[%s](%s)" final-text choice)))))))


(defun markdown-smart-links-setup ()
  "Configure l'autocomplétion pour markdown-mode."
  ;; Remplacer le raccourci natif par notre version intelligente
  (define-key markdown-mode-map (kbd "C-c C-l") #'markdown-smart-insert-link))

(defun markdown-smart-links-remove ()
  "Supprime l'autocomplétion personnalisée."
  (interactive)
  ;; Restaurer le raccourci original
  (define-key markdown-mode-map (kbd "C-c C-l") #'markdown-insert-link))

;; Auto-setup
(with-eval-after-load 'markdown-mode
  (markdown-smart-links-setup))

;; mimic org-download-paste...
(defun markdown-paste-clipboard-image ()
  "Save clipboard image to images folder and insert markdown link.
Uses org-download configuration but works in markdown-mode."
  (interactive)
  (let* ((image-dir (if (boundp 'org-download-image-dir)
                        (expand-file-name org-download-image-dir 
                                          (file-name-directory (buffer-file-name)))
                      (expand-file-name "images" 
                                        (file-name-directory (buffer-file-name)))))
         (timestamp (if (boundp 'org-download-timestamp)
                        org-download-timestamp
                      "_%Y%m%d_%H%M%S"))
         (filename (format "screenshot%s.png" 
                          (format-time-string timestamp)))
         (filepath (expand-file-name filename image-dir)))
    
    ;; Create images directory if it doesn't exist
    (unless (file-exists-p image-dir)
      (make-directory image-dir t))
    
    ;; Save clipboard image (cross-platform)
    (cond
     ((eq system-type 'darwin)  ; macOS
      (if (zerop (shell-command (format "pngpaste '%s'" filepath)))
          (message "Image saved: %s" filename)
        (error "Failed to save image. Is pngpaste installed?")))
     
     ((eq system-type 'gnu/linux)  ; Linux  
      (if (zerop (shell-command (format "xclip -selection clipboard -t image/png -o > '%s'" filepath)))
          (message "Image saved: %s" filename)
        (error "Failed to save image. Is xclip installed?")))
     
     ((eq system-type 'windows-nt)  ; Windows
      (if (zerop (shell-command 
                  (format "powershell -command \"Add-Type -AssemblyName System.Windows.Forms; $img = [System.Windows.Forms.Clipboard]::GetImage(); if ($img) { $img.Save('%s', [System.Drawing.Imaging.ImageFormat]::Png); 'Success' } else { exit 1 }\""
                          filepath)))
          (message "Image saved: %s" filename)
        (error "Failed to save image from clipboard")))
     
     (t (error "Unsupported operating system")))
    
    ;; Insert markdown link
    (insert (format "![Screenshot](%s)" 
                    (file-relative-name filepath)))
    (newline)))

;;; project.el
(use-package project
  :config
  ;; Comprehensive project detection function (if no .git is detected )
  (defun project-find-by-markers (dir)
  "Find project root by looking for marker files.
Patterns starting with a dot are treated as extensions (matches *.ext).
Returns a transient project for project.el compatibility."
  (let ((markers '(;; .NET
                   ".sln"
                   ".csproj"
                   ".fsproj"
                   ".vbproj"
                   ;; JavaScript/TypeScript
                   "package.json"
                   "tsconfig.json"
                   ;; Rust
                   "Cargo.toml"
                   ;; Go
                   "go.mod"
                   ;; Java/JVM
                   "pom.xml"
                   "build.gradle"
                   "build.gradle.kts"
                   ;; PHP
                   "composer.json"
                   "artisan"
                   "symfony.lock"
                   ;; PowerShell
                   ".psd1"
                   ".psm1"
                   "PSScriptAnalyzerSettings.psd1"
                   ;; Python
                   "requirements.txt"
                   "Pipfile"
                   "pyproject.toml"
                   "setup.py"
                   ;; Ruby
                   "Gemfile"
                   "Rakefile"
                   ;; Elixir
                   "mix.exs"
                   ;; Clojure
                   "project.clj"
                   "deps.edn"
                   ;; Generic VCS (as fallback)
                   ".git"
                   ".hg"
                   ".svn")))
    (cl-loop for pattern in markers
             for root = (cond
                         ;; Pattern starts with dot = extension pattern
                         ;; e.g., ".csproj" matches "MyProject.csproj"
                         ((and (string-match "^\\.[^./]+$" pattern)
                               ;; But not .git, .hg, .svn (those are directories)
                               (not (member pattern '(".git" ".hg" ".svn"))))
                          (locate-dominating-file 
                           dir
                           (lambda (parent)
                             (directory-files parent nil
                                            (concat ".*" (regexp-quote pattern) "$")
                                            t))))  ; t = just check existence, don't return list
                         ;; Exact filename or directory
                         (t
                          (locate-dominating-file dir pattern)))
             when root
             return (cons 'transient root))))


  ;; Add to project finders
  (add-hook 'project-find-functions #'project-find-by-markers)

  ;; Mappings des commandes run par mode majeur
  (setq project-run-commands
      '((csharp-mode . "dotnet run")
        (php-mode . "php index.php")
        (web-mode . "php index.php")
        (powershell-mode . "pwsh ./main.ps1")
        (javascript-mode . "npm start")
        (js-mode . "npm start")
        (js-ts-mode . "npm start")
        (typescript-mode . "npm start")
        (typescript-ts-mode . "npm start")
        (rust-mode . "cargo run")
        (rust-ts-mode . "cargo run")
        (go-mode . "go run .")
        (go-ts-mode . "go run .")
        (java-mode . "mvn exec:java")
        (kotlin-mode . "gradle run")
        (python-mode . "python main.py")
        (python-ts-mode . "python main.py")))

  ;; Mappings des commandes test par mode majeur  
  (setq project-test-commands
      '((csharp-mode . "dotnet test")
        (php-mode . "php -l index.php")
        (web-mode . "php -l index.php")
        (powershell-mode . "pwsh -File ./main.ps1")
        (javascript-mode . "npm test")
        (js-mode . "npm test")
        (js-ts-mode . "npm test")
        (typescript-mode . "npm test")
        (typescript-ts-mode . "npm test")
        (rust-mode . "cargo test")
        (rust-ts-mode . "cargo test")
        (go-mode . "go test")
        (go-ts-mode . "go test")
        (java-mode . "mvn test")
        (kotlin-mode . "gradle test")
        (python-mode . "python -m pytest")
        (python-ts-mode . "python -m pytest")))

  (defun project-execute-command (command-type &optional prompt-user)
  "Execute project command of COMMAND-TYPE ('run or 'test).
If PROMPT-USER is non-nil, let user edit the command."
  (if-let ((project (project-current)))
      (let* ((default-directory (project-root project))
             ;; Variable spécifique au projet (.dir-locals.el)
             (var-name (intern (format "project-%s-command" command-type)))
             ;; Ordre de priorité :
             ;; 1. Variable locale au projet (.dir-locals.el ou hook)
             ;; 2. Mapping standard par mode majeur
             ;; 3. Chaîne vide en fallback
             (suggested-command (or (when (boundp var-name) (symbol-value var-name))
                                    (cdr (assq major-mode 
                                               (if (eq command-type 'run)
                                                   project-run-commands
						 project-test-commands)))
                                    ""))
             (command (if prompt-user
                          (read-string (format "%s command: " (capitalize (symbol-name command-type)))
                                       suggested-command)
			(or suggested-command
                            (user-error "Don't know how to %s projects for %s" 
					command-type major-mode)))))
        (compile command))
    (user-error "Not in a project")))

  ;; Fonctions d'interface
  (defun project-run () 
    "Run project with appropriate command." 
    (interactive) 
    (project-execute-command 'run))

  (defun project-run-with-command () 
    "Run project with user-specified command."
    (interactive) 
    (project-execute-command 'run t))

  (defun project-test () 
    "Test project with appropriate command."
    (interactive) 
    (project-execute-command 'test))

  (defun project-test-with-command () 
    "Test project with user-specified command."
    (interactive) 
    (project-execute-command 'test t))

  ;; Bind to project keymap
  (define-key project-prefix-map "r" #'project-run)           ; C-x p r
  (define-key project-prefix-map "t" #'project-test)          ; C-x p t
  (define-key project-prefix-map "R" #'project-run-with-command)) ; C-x p R

;; overridable with .dir-locals.el					;
;;; For a PHP Laravel project (.dir-locals.el)
;((php-mode . ((project-run-command . "php artisan serve")
;              (project-test-command . "php artisan test"))))

;;; PHP
;(with npm language server)
(use-package php-mode
  :ensure t
  :hook (php-mode . (lambda () (eglot-ensure) (flymake-mode 1)))
  :config
  ;; Configure eglot to use Intelephense
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(php-mode . ("intelephense" "--stdio"))))
  
  :bind (:map php-mode-map
              ("C-." . nil))  ; Unbind php-show-arglist
  )

;;; WEB (also for blade template)
(use-package web-mode
  :ensure t)

;;; CSharp
;; built-in package, no :ensure t
(use-package csharp-mode
  :hook (csharp-mode . eglot-ensure)
  :hook ((csharp-mode . (lambda ()
			  (when (project-current)
;			  (setq-local compile-command (csharp-detect-project-command 'build))
			  (setq-local compile-command "dotnet build")
                          (setq-local project-run-command 
                                     (csharp-detect-project-command 'run))
                          (setq-local project-test-command 
                                      (csharp-detect-project-command 'test))))))
  
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(csharp-mode . ("omnisharp" "-lsp"))))

  ;; Détection spéciale C# pour les projets avec .csproj dans sous-dossier
  (defun csharp-detect-project-command (command-type)
    "Detect C# project command, handling .csproj in subdirectories."
    (let ((root (project-root (project-current))))
      (cond
       ;; .csproj direct dans root -> commande standard
       ((directory-files root nil "\\.csproj$")
        (format "dotnet %s" command-type))
       ;; .csproj dans sous-dossier -> utiliser --project
       ((cl-some (lambda (subdir)
                   (when (and (file-directory-p (expand-file-name subdir root))
                             (not (string-prefix-p "." subdir))
                             (directory-files (expand-file-name subdir root) nil "\\.csproj$"))
                     subdir))
                 (directory-files root nil nil t))
        (format "dotnet %s --project %s" command-type
                (cl-some (lambda (subdir)
                           (when (and (file-directory-p (expand-file-name subdir root))
                                     (not (string-prefix-p "." subdir))
                                     (directory-files (expand-file-name subdir root) nil "\\.csproj$"))
                             subdir))
                         (directory-files root nil nil t))))
       ;; Fallback sur les commandes standards
       (t (format "dotnet %s" command-type)))))
  )

;;; Powershell
; TreeSitter and Eglot
;; powershell handles file associations automatically (eglot with npm lang server)
(use-package powershell
  :ensure t
  :hook (powershell-mode . (lambda () (eglot-ensure) (flymake-mode 1))))



;;; EGLOT LSP language server
(use-package eglot
  :hook (yaml-ts-mode . (lambda ()
			  (eglot-ensure)
			  (flymake-mode 1)
			  ;; YAML-specific settings
                          (setq-local tab-width 2)
                          (setq-local indent-tabs-mode nil)
                          (setq-local standard-indent 2)))
  :bind (:map eglot-mode-map
         ;; Navigation
         ("C-c l d" . eglot-find-declaration)
         ("C-c l i" . eglot-find-implementation)
         ("C-c l t" . eglot-find-typeDefinition)
         
         ;; Code actions & refactoring
         ("C-c l r" . eglot-rename)
         ("C-c l a" . eglot-code-actions)
         ("C-c l o" . eglot-code-action-organize-imports)
         ("C-c l q" . eglot-code-action-quickfix)
         
         ;; Formatting
         ("C-c l f" . eglot-format)
         ("C-c l F" . eglot-format-buffer)
         
         ;; Documentation & help
         ("C-c l h" . eldoc)
         ("C-c l H" . eldoc-doc-buffer)
         
         ;; Server management
         ("C-c l s" . eglot-shutdown)
         ("C-c l S" . eglot-shutdown-all)
         ("C-c l R" . eglot-reconnect)
         
         ;; Workspace
         ("C-c l w r" . eglot-workspace-restart)
         ("C-c l w s" . eglot-signal-didChangeConfiguration)
         
         ;; Diagnostics
         ("C-c l e" . eglot-stderr-buffer)
         ("C-c l E" . eglot-events-buffer)
	 )
  )

(use-package consult-eglot
  :ensure t)
;(use-package eglot
;  :defer t
;  :hook ((rust-ts-mode . eglot-ensure)
;         (python-ts-mode . eglot-ensure)
;         (go-ts-mode . eglot-ensure)
;         (js-ts-mode . eglot-ensure)
;         (typescript-ts-mode . eglot-ensure)
;         (c-ts-mode . eglot-ensure)
;         (java-ts-mode . eglot-ensure)
;         (php-mode . eglot-ensure)
;         (csharp-mode . eglot-ensure)))

;performance tips https://emacs-lsp.github.io/lsp-mode/page/performance/

;;realy needed ?
;(setenv "LSP_USE_PLISTS" "true") ONLY for LSP-MODE

;it worked with that, lets try without
;(setq gc-cons-threshold 1600000) ;default 800000
;(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;Debug
;(use-package dap-mode
;  :config
;  ;; Enable all language modules
;  (require 'dap-lldb)    ; Rust, C++
;  (require 'dap-python)  ; Python
;  (require 'dap-node)    ; JavaScript/Node.js
;  (require 'dap-go)      ; Go
;  (require 'dap-php)     ; PHP
; 
;  (dap-auto-configure-mode))



;;;YaSnippet
(use-package yasnippet 
	:ensure t 
	:config 
	(yas-global-mode 1)
	;(setq yas-snippet-dirs
    ;  '("~/.emacs.d/snippets"                 ;; personal snippets
        ;"/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
        ;"/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
    ;    ))
)
(use-package yasnippet-snippets :ensure t)



;;; Browse at remote (github ...)
(use-package browse-at-remote :ensure t)


;;; TODOs handling Hl-TODO
(use-package hl-todo
  :ensure t
  :config
  ;; Enable hl-todo with flymake...
  ;alternative to not well working (global-hl-todo-mode 1)
  (add-hook 'flymake-mode-hook
            (lambda ()
              ;; Enable hl-todo-mode when flymake starts
              (hl-todo-mode 1)
              ;; Add it to flymake immediately
              (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake nil t))))

;;; Magit delta
(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode))

;;; Magit todo

; sort by todo number
(defun my/magit-todos--sort-by-number (a b)
  "Sort todos by number prefix in the item text."
  (let* ((desc-a (magit-todos-item-description a))
         (desc-b (magit-todos-item-description b))
         (num-a (when (string-match "^\\([0-9]+\\)" desc-a)
                  (string-to-number (match-string 1 desc-a))))
         (num-b (when (string-match "^\\([0-9]+\\)" desc-b)
                  (string-to-number (match-string 1 desc-b)))))
    (cond
     ;; Both have numbers - compare numerically
     ((and num-a num-b) (< num-a num-b))
     ;; Only A has number - A comes first
     (num-a t)
     ;; Only B has number - B comes first  
     (num-b nil)
     ;; Neither has number - fallback to string comparison
     (t (string< desc-a desc-b)))))


;; Append todos in magit status
(use-package magit-todos
  :ensure t
  :after (magit hl-todo)
  :config
  ;(magit-todos-mode 1) ;inactive by default
  (when (eq system-type 'windows-nt) ;windows rg issue https://github.com/alphapapa/magit-todos/issues/29
					;(setq magit-todos-scanner 'magit-todos--scan-with-grep)
    (setq magit-todos-nice nil)
    (setq magit-todos-scanner 'magit-todos--scan-with-git-grep)
    )
  (setq magit-todos-sort-order '(my/magit-todos--sort-by-number
                                 magit-todos--sort-by-filename
                                 magit-todos--sort-by-position))
  (setq magit-todos-scan-untracked t)
  )

;;; Treemacs 
; https://github.com/Alexander-Miller/treemacs?tab=readme-ov-file#installation
(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("C-x t s"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  
  :config
  (define-key treemacs-mode-map (kbd "<right>") #'treemacs-TAB-action)
  (define-key treemacs-mode-map (kbd "<left>") #'treemacs-TAB-action)
  )
; Allow having files in workspaces !!! ;-)
(with-eval-after-load 'treemacs
  (defadvice treemacs-toggle-node (around open-file-or-toggle activate)
    (let* ((node (treemacs-node-at-point))
           (path (treemacs-button-get node :path)))
      (if (file-regular-p path)
          (find-file path)
        ad-do-it))))
		
(use-package treemacs-magit
:after (treemacs magit)
:ensure t)

;; Enable outline-minor-mode where needed
(add-hook 'text-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'outline-minor-mode)

;; Universal outline functions (minimal)
(defun show-all-universal ()
  (interactive)
  (if (derived-mode-p 'org-mode) (org-show-all) (outline-show-all)))

(defun hide-levels-universal (n)
  (interactive "p")
  (if (derived-mode-p 'org-mode) (org-content n) (outline-hide-sublevels n)))

;; Universal key bindings
(global-set-key (kbd "C-c 0") 'show-all-universal)
(global-set-key (kbd "C-c 1") (lambda() (interactive) (hide-levels-universal 1)))
(global-set-key (kbd "C-c 2") (lambda() (interactive) (hide-levels-universal 2)))
(global-set-key (kbd "C-c 3") (lambda() (interactive) (hide-levels-universal 3)))
(global-set-key (kbd "C-c 4") (lambda() (interactive) (hide-levels-universal 4)))

;;; Git Version control
;add files to log
(setq vc-git-log-switches '("--name-status"))


;;; Command frequencies (stats) not well working ?
;; (defvar my-command-frequencies (make-hash-table :test 'equal))
;; (defvar my-command-frequency-file (expand-file-name "command-frequencies.el" user-emacs-directory))

;; (defun my-record-command-advice (orig-fun &rest args)
;;   "Record command frequency."
;;   (when (commandp this-command)
;;     (let ((cmd (symbol-name this-command)))
;;       (puthash cmd (1+ (gethash cmd my-command-frequencies 0)) my-command-frequencies)))
;;   (apply orig-fun args))

;; (defun my-save-frequencies ()
;;   "Save frequencies to file."
;;   (let ((frequencies '()))
;;     (maphash (lambda (cmd count) (push (cons cmd count) frequencies)) my-command-frequencies)
;;     (with-temp-file my-command-frequency-file
;;       (insert (format "%S" frequencies)))))

;; (defun my-load-frequencies ()
;;   "Load frequencies from file."
;;   (when (file-exists-p my-command-frequency-file)
;;     (with-temp-buffer
;;       (insert-file-contents my-command-frequency-file)
;;       (let ((data (read (buffer-string))))
;;         (clrhash my-command-frequencies)
;;         (dolist (item data)
;;           (puthash (car item) (cdr item) my-command-frequencies))))))

;; (defun my-show-top-commands ()
;;   "Show top 100 commands."
;;   (interactive)
;;   (let ((frequencies '()))
;;     (maphash (lambda (cmd count) (push (cons cmd count) frequencies)) my-command-frequencies)
;;     (setq frequencies (sort frequencies (lambda (a b) (> (cdr a) (cdr b)))))
;;     (with-current-buffer (get-buffer-create "*Top Commands*")
;;       (erase-buffer)
;;       (insert "Top 100 Commands:\n\n")
;;       (dotimes (i (min 100 (length frequencies)))
;;         (let ((item (nth i frequencies)))
;;           (insert (format "%5d  %s\n" (cdr item) (car item)))))
;;       (goto-char (point-min))
;;       (display-buffer (current-buffer)))))

;(advice-add 'call-interactively :around #'my-record-command-advice)
;(add-hook 'kill-emacs-hook #'my-save-frequencies)
;(my-load-frequencies)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(safe-local-variable-directories '("c:/ws/home/org/")))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


;;; AI  Minimal gptel with Claude as default
(use-package gptel
  :ensure (:host github :repo "karthink/gptel")
  :config
  ;; Set Claude as default backend
  ;; (setq gptel-backend (gptel-make-anthropic "Claude"
  ;;                       :stream t
  ;;                       :key #'gptel-api-key-from-auth-source))
  ;; Set Claude Sonnet 4 as default backend and model
  (setq gptel-model 'claude-sonnet-4-20250514  ; Claude Sonnet 4
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key #'gptel-api-key-from-auth-source)
	gptel-default-mode #'org-mode
	gptel-org-branching-context t
	)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  
  ;; Add other backends here as needed:
  ;; (gptel-make-openai "OpenAI" :stream t :key #'gptel-api-key-from-auth-source)
  ;; (gptel-make-gemini "Gemini" :stream t :key #'gptel-api-key-from-auth-source)
  ;; (gptel-make-ollama "Ollama" :stream t :host "localhost:11434")
  
  ;; gptel doesn't set global bindings by default, so these are safe to add
  :bind (("C-c g" . gptel)
         ("C-c G" . gptel-send)
	 ("C-c M-g" . gptel-menu)
	 ("C-c C-g" . gptel-request)
	 ))

(use-package drag-stuff
  :ensure t

  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys)

  ;; Disable in org-mode to avoid conflicts
  :hook (org-mode . (lambda () (drag-stuff-mode -1)))
  ;; Disable in markdown-mode to avoid conflicts
  :hook (markdown-mode . (lambda () (drag-stuff-mode -1)))
)

