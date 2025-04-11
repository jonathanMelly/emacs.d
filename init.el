;jmy
(load-theme 'wheatgrass)
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;frame size
(setq initial-frame-alist '( (width . 121) (height . 40) ) )
(setq default-frame-alist '( (width . 121) (height . 40) ) )

;; Save sessions history
; (setq comint-input-ring-size 10000)
; (setq comint-input-ring-file-name (expand-file-name "shell-history" user-emacs-directory))
; (comint-read-input-ring t)
; (add-hook 'shell-mode-hook 'comint-read-input-ring)
; (add-hook 'kill-buffer-hook 'comint-write-input-ring)
; (add-hook 'kill-emacs-hook (lambda ()
                             ; (comint-write-input-ring)
                             ; (comint-read-input-ring)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ace-window dash yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;(require 'server)
;(if (not (server-running-p)) (server-start))


;(load (expand-file-name "fly.el" user-emacs-directory))
;(add-to-list 'load-path "~/.emacs.d/lisp/")

;(require 'xah-fly-keys)
;(xah-fly-keys-set-layout "bepo")
;(xah-fly-keys 1)

;1
;2
;3
;(define-key key-translation-map (kbd "C-(") (kbd "C-4"))
;(define-key key-translation-map (kbd "C-)") (kbd "C-5"))
;6
(define-key key-translation-map (kbd "C-+") (kbd "C-7"))
;8 is -
;9 is /

;1
;2
;3
;(define-key key-translation-map (kbd "M-(")   (kbd "M-4")); disabled as M-( may be useful
;(define-key key-translation-map (kbd "M-)")   (kbd "M-5")); disabled as M-( may be useful
;6
(define-key key-translation-map (kbd "M-+")   (kbd "M-7"))
;8 is -
;9 is /

;1
;2
;3
;(define-key key-translation-map (kbd "C-M-(")   (kbd "C-M-4")); disabled as C-M-( may be useful
;(define-key key-translation-map (kbd "C-M-)")   (kbd "C-M-5")); disabled as C-M-( may be useful
;6
(define-key key-translation-map (kbd "C-M-+")   (kbd "C-M-7"))

(setq calendar-week-start-day 1          ; Week starts on Monday
      calendar-intermonth-text           ; Display week numbers
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-function-name-face))


(electric-pair-mode 1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(global-set-key (kbd "M-o") 'ace-window)

;; UTF-8 as default encoding
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)

;; add this especially on Windows, else python output problem
(set-terminal-coding-system 'utf-8-unix)