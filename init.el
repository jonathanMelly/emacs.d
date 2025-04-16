;jmy
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(load-theme 'wheatgrass)
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;frame size
(setq initial-frame-alist '( (width . 121) (height . 40) ) )
(setq default-frame-alist '( (width . 121) (height . 40) ) )



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(setq calendar-week-start-day 1          ; Week starts on Monday
      calendar-intermonth-text           ; Display week numbers
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-function-name-face))


(electric-pair-mode 1)


(global-set-key (kbd "M-o") 'ace-window)

;; UTF-8 as default encoding
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)

;; add this especially on Windows, else python output problem
(set-terminal-coding-system 'utf-8-unix)


(use-package diminish
  :ensure t)


;; Install packages if you haven't already
(use-package helm
  :ensure t
  :diminish
  :config
  (helm-mode 1))

(use-package projectile
  :ensure t
  :diminish
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on))

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
        
(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))

(use-package doom-modeline :ensure t :hook (after-init . doom-modeline-mode))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(setq org-agenda-files "~/org/agenda-files.txt")
