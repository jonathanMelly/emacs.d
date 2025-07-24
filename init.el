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
;/!\ early as using a new transient version
(use-package transient :ensure (:wait t))
(use-package magit 
	:ensure t
	:config (setq magit-define-global-key-bindings "recommended")
	:bind
	(
		("C-x g" . magit-status)
		("C-c g" . magit-dispatch)
		("C-c f" . magit-file-dispatch)
	)
	)

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
        '((type "TODO" "WIP" "|" "DONE" "DROP"))))

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
  ;(setq consult-ripgrep-args
;	"rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number -M 500")
  
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
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

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
;(add-hook 'after-make-frame-functions
;          (lambda (frame)
;            (with-selected-frame frame
;              (set-face-attribute 'default nil :font "Hack" :height 120))))
;; Set for initial frame too
;;(set-face-attribute 'default nil :font "Hack" :height 120)

(customize-set-variable 'default-frame-alist 
                       '((font . "Hack-12")))

;; symbols (for mood-line) and emojis
(set-fontset-font
 t 'symbol (font-spec :family "Segoe UI Symbol") nil 'append)

;(set-fontset-font
;    t 'emoji (font-spec :family "Noto Emoji") nil 'prepend)

;;; Modline

;; display time+load average...
;(display-time-mode 1)

;;https://gitlab.com/jessieh/mood-line
(use-package mood-line
  :ensure t
  :demand t
  :config 
	(mood-line-mode)
	;(setq mood-line-format mood-line-format-default-extended)
	(setq mood-line-format mood-line-format-default)
	(setq mood-line-glyph-alist mood-line-glyphs-unicode)
	)
;(use-package minions
;  :ensure t
;  :demand t
;  :config
;  (minions-mode 1))

;force symbol compatible font for range (used in mood-line)
;(set-fontset-font t '(#x2691 . #x1F2FF) "Segoe UI Symbol" nil 'append)
					;(set-fontset-font t '(#x1F300 . #x1F6FF) "Noto Color Emoji" nil 'append) ; Emoji range
  
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
  
(setq show-trailing-whitespace t) ;;
(setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.
(electric-pair-mode 1)
(electric-quote-mode 1)


;;; Key bindings
(global-set-key (kbd "<f1>") 'org-agenda-list)
(global-set-key (kbd "<f2>") 'org-todo-list)
(global-set-key (kbd "<f3>") 'other-window)
(global-set-key (kbd "S-<f4>") 'kill-buffer)
(global-set-key (kbd "S-<f5>") 'diff-buffer-with-file)
(global-set-key (kbd "S-<f6>") 'rg-menu) ;; 
(global-set-key (kbd "S-<f10>") 'save-buffer)
(global-set-key (kbd "S-<f11>") 'toggle-frame-fullscreen) ;; as f11 is for @...

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
  
;;;Markdown
;https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  
  :mode ("README\\.md\\'" . gfm-mode)
  
  :init
	(setq markdown-command '("pandoc" "--from=gfm" "--to=html5"))
	;(setq markdown-command "multimarkdown")
	(setq markdown-split-window-direction 'below)
	(setq markdown-live-preview-delete-export 'delete-on-export)
	
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)
	      ("C-c C-v d" . markdown-insert-fenced-code-block)
	      ("C-c C-v c" . markdown-insert-code)
	      ("M-<up>" . markdown-move-subtree-up)
	      ("M-<down>" . markdown-move-subtree-down)
	      ("M-<left>" . markdown-promote-subtree)
	      ("M-<right>" . markdown-demote-subtree)
	      )
  ;; try to remove backtick electric as it prints weird chars...
  :hook (markdown-mode . (lambda () 
                           (setq-local electric-pair-pairs 
                                       (remove '(?` . ?`) electric-pair-pairs))))
  
)
		 
(use-package denote-markdown :ensure t)

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

;;; Powershell
; TreeSitter and Eglot
;; powershell handles file associations automatically (eglot with npm lang server)
(use-package powershell
  :ensure t
  :hook (powershell-mode . (lambda () (eglot-ensure) (flymake-mode 1))))



;;; EGLOT
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
;; Append todos in magit status
(use-package magit-todos
  :ensure t
  :after (magit hl-todo)
  :config
  (magit-todos-mode 1)
  (when (eq system-type 'windows-nt) ;windows rg issue https://github.com/alphapapa/magit-todos/issues/29
					;(setq magit-todos-scanner 'magit-todos--scan-with-grep)
    (setq magit-todos-nice nil)
    (setq magit-todos-scanner 'magit-todos--scan-with-git-grep)
    )
  )
