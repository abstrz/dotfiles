(defun pkg-init ()
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
(package-initialize))
(defun install (p)
  (unless (package-installed-p p)
    (package-install p))
  (require p))

(defun install-packages (pkgs)
  (when (car pkgs)
    (progn (install (car pkgs))
	   (install-packages (cdr pkgs)))))

(defun linum-init ()
  (require 'linum-relative)
  (global-linum-mode 1)
  (linum-relative-on)
  (setq linum-relative-current-symbol ""))

(defun graphics ()
  (if (display-graphic-p)
      (setq initial-frame-alist
	    '(
	      (font . "iosevka")
	      (alpha . 90)
	      (menu-bar-lines . 0)
	      (tool-bar-lines . 0)
	      (width . 106)
	      (height . 60)
	      (left . 50)
	      (top . 50)))
    (setq initial-frame-alist '( (tool-bar-lines . 0))))
  (setq default-frame-alist initial-frame-alist))

(defun hooks ()
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

(defun misc-options ()
  (setq inhibit-startup-screen t)
  (ac-config-default)
  (show-paren-mode 1)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (projectile-mode +1)
  (helm-mode 1)
  (desktop-save-mode 1)
  ;;Scroll one line at a time
  (setq scroll-step 1)
  )

(pkg-init)
(install-packages '(multiple-cursors magit haskell-mode lsp-mode lsp-ui lsp-haskell racket-mode helm projectile ibuffer-projectile auto-complete org-bullets tablist))
(graphics)
(hooks)
(linum-init)
(misc-options)

;; AUTO GENERATED 


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes '(wombat))
 '(package-selected-packages '(magit ##)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
