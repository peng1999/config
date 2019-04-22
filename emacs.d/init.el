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

(package-initialize)
(unless (require 'use-package nil 'noerror)
  (message "Reached use-package non nil")
  (package-refresh-contents)
  ;; (package-initialize)
  (package-install 'use-package)
  (require 'use-package))

;; load packages
(use-package smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package company
  :hook (after-init . global-company-mode))

(use-package evil
  :config
  (evil-mode 1)
  ;; (evil-set-initial-state 'org-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'comint-mode 'emacs))

(use-package fcitx
  :config
  (fcitx-default-setup))

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
   '("rls"))

  ;; enable ccls mode
  (lsp-define-stdio-client
   lsp-ccls-mode
   "c++"
   (lambda () default-directory)
   '("ccls"))

  :hook
  ((rust-mode . lsp-rust-mode-enable)
   (c++-mode . lsp-ccls-mode-enable)))
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
;; (use-package lsp-rust
;;   :after (lsp-mode company-lsp flycheck)
;;   :init
;;   (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
;;   :hook
;;   (rust-mode . lsp-rust-enable))

(use-package pdf-tools
  :ensure t
  :hook (doc-view-mode . pdf-tools-install))

(use-package auctex
  :hook (LaTeX-mode . LaTeX-math-mode))
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)


;; setup maxima
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))


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
 '(LaTeX-command "xelatex")
 '(TeX-engine (quote xetex))
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open"))))
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
 '(company-global-modes (quote (not org-mode)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 1)
 '(custom-enabled-themes (quote (manoj-dark)))
 '(doc-view-continuous t)
 '(doc-view-resolution 300)
 '(eww-search-prefix "https://www.google.com/search?q=")
 '(imaxima-fnt-size "huge")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(org-M-RET-may-split-line nil)
 '(org-agenda-files (quote ("~/documents/note.org")))
 '(org-export-backends (quote (ascii html latex md odt)))
 '(org-hide-leading-stars t)
 '(org-latex-classes
   (quote
    (("ctexart" "\\documentclass[UTF8]{ctexart}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(org-latex-default-class "ctexart")
 '(org-pretty-entities t)
 '(package-selected-packages
   (quote
    (lsp-rust auctex fcitx magit use-package smex rust-mode lsp-ui evil company-lsp flycheck)))
 '(url-proxy-services (quote (("socks5" . "localhost:1080"))))
 '(xterm-mouse-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "DAMA" :slant normal :weight normal :height 163 :width normal)))))
