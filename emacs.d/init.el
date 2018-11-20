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


;; load el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;; load packages
(el-get-bundle smex
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(el-get-bundle company-mode
  (add-hook 'after-init-hook 'global-company-mode))

(el-get-bundle evil
  (evil-mode 1)
  (evil-set-initial-state 'org-mode 'emacs))

(el-get-bundle rust-mode)

(el-get-bundle lsp-mode)

;; enable rust mode lsp
(require 'lsp-mode)
(lsp-define-stdio-client
 lsp-rust-mode
 "rust"
 (lambda () default-directory)
 '("rustup" "run" "nightly" "rls"))

(lsp-define-stdio-client
 lsp-ccls-mode
 "c++"
 (lambda () default-directory)
 '("ccls"))

(add-hook 'rust-mode-hook #'lsp-rust-mode-enable)
(add-hook 'c++-mode-hook #'lsp-ccls-mode-enable)

(el-get-bundle flycheck)
  ;;(global-flycheck-mode))
(el-get-bundle lsp-ui
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'lsp-mode-hook 'flycheck-mode))
(el-get-bundle company-lsp)
;; ;; rust
;; (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
;; (el-get-bundle lsp-rust)

(package-initialize)


;; org-mode config
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(evil-default-state (quote emacs))
 '(indent-tabs-mode nil)
 '(org-M-RET-may-split-line nil)
 '(org-agenda-files (quote ("~/documents/note.org")))
 '(package-selected-packages (quote (smex)))
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
