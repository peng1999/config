;; behave as o in vim
(defun open-next-line (arg)
  "Move to the next line and then opens a line. See also `newline-and-indent`"
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
;; behave as O in vim
(defun open-previous-line (arg)
  "Move to the next line and then opens a line. See also `newline-and-indent`"
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(defvar newline-and-indent t)

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "C-S-o") 'open-previous-line)


;; setup melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))

  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(package-initialize)
(unless (require 'use-package nil 'noerror)
  (message "Reached use-package non nil")
  (package-refresh-contents)
  ;; (package-initialize)
  (package-install 'use-package)
  (require 'use-package))

;; load packages
(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ;;(evil-set-initial-state 'org-mode 'emacs)
  (evil-set-initial-state 'term-mode nil))

(use-package magit
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :config
  ;; enable rust mode lsp
  (lsp-define-stdio-client
   lsp-rust-mode
   "rust"
   (lambda () default-directory)
   '("rustup" "run" "nightly" "rls"))

  ;; enable ccls mode
  (lsp-define-stdio-client
   lsp-ccls-mode
   "c++"
   (lambda () default-directory)
   '("ccls"))

  :hook
  (c++-mode . lsp-ccls-mode-enable))
(use-package lsp-rust
  :after lsp-mode
  :init (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  ;; :hook (rust-mode . lsp-rust-mode-enable))
  )
(use-package flycheck
  :ensure t)
(use-package lsp-ui
  :ensure t
  :after (lsp-mode flycheck)
  :hook
  ((lsp-mode . lsp-ui-mode)
   (lsp-mode . flycheck-mode)))
(use-package company-lsp
  :ensure t)


;; org-mode config
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; electric pair mode
(add-hook 'after-init-hook 'electric-pair-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(backup-directory-alist (quote ((".*" . "~/.local/share/emacs/backup"))))
 '(c-basic-offset 4)
 '(c-default-style
   (quote
    ((c-mode . "java")
     (c++-mode . "java")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(column-number-mode t)
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 1)
 '(custom-enabled-themes (quote (manoj-dark)))
 '(gdb-show-main t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(org-M-RET-may-split-line nil)
 '(org-agenda-files nil)
 '(org-hide-leading-stars t)
 '(package-selected-packages
   (quote
    (magit use-package smex rust-mode lsp-ui evil el-get company-lsp)))
 '(url-proxy-services (quote (("http" . "localhost:1080"))))
 '(xterm-mouse-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:background "gold3" :foreground "black"))))
 '(company-preview-common ((t (:background "gold3" :foreground "grey20"))))
 '(company-preview-search ((t (:background "green4" :foreground "green"))))
 '(company-scrollbar-bg ((t (:background "#303030"))))
 '(company-scrollbar-fg ((t (:background "#404040"))))
 '(company-tooltip ((t (:background "#202020" :foreground "grey"))))
 '(company-tooltip-annotation ((t (:foreground "gold"))))
 '(company-tooltip-annotation-selection ((t (:foreground "white"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :foreground "white"))))
 '(company-tooltip-common-selection ((t (:foreground "white"))))
 '(company-tooltip-selection ((t (:background "red3" :foreground "white")))))
