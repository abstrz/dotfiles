(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.

There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(setq package-archives 
      (cons '("org" . "http://orgmode.org/elpa/")
	    package-archives))


(package-initialize)
(setq package-enable-at-startup nil)

(add-to-list 'load-path (concat user-emacs-directory "config"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (kaolin-dark)))
 '(custom-safe-themes
   (quote
    ("a70b47c87e9b0940f6fece46656200acbfbc55e129f03178de8f50934ac89f58" "b11699e28cc2f6c34fa6336e67d443be89fadb6a9b60de0b1594f31340ea87e4" "ac2ca460db1668a48c35c4d0fd842e5d2ce2d4e8567a7903b76438f2750826cd" "c19e5291471680e72d8bd98f8d6e84f781754a9e8fc089536cda3f0b7c3550e3" default)))
 '(fci-rule-color "#dedede")
 '(line-spacing 0.2)
 '(package-selected-packages
   (quote
    (auto-complete which-key kaolin-themes poet-theme ## magit use-package evil-leader linum-relative elisp-slime-nav evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;initialize eldoc-mode
(require 'elisp-slime-nav)
(defun my-lisp-hook ()
  (elisp-slime-nav-mode)
  (turn-on-eldoc-mode))
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)


;;initialize line numbers
(require 'linum-relative)
(setq linum-relative-current-symbol "")
(add-hook 'find-file-hook 'linum-relative-mode)

(use-package evil-leader
  :commands (evil-leader-mode)
  :ensure evil-leader
  :demand evil-leader
  :init (global-evil-leader-mode)
  :config (progn
	    (evil-leader/set-leader "<SPC>")
	    (evil-leader/set-key "w" 'save-buffer)
	    (evil-leader/set-key "q" 'kill-buffer-and-window)
	    (evil-leader/set-key "h" 'dired-jump)
	    (evil-leader/set-key "v" 'split-window-right)
	    (evil-leader/set-key "e" 'pp-eval-last-sexp)
	    (evil-leader/set-key "<SPC>" 'other-window)
	    (evil-leader/set-key "b" 'ibuffer)
	    (evil-leader/set-key "x" 'helm-M-x)
	    (evil-leader/set-key "<RET>" 'bookmark-bmenu-list)
	    (evil-leader/set-key "n" 'bookmark-set)))

;;intialize git package
(use-package magit
  :ensure magit
  :config (progn
	    (evil-set-initial-state 'magit-mode 'normal)
	    (evil-set-initial-state 'magit-status-mode 'normal)
	    (evil-set-initial-state 'magit-diff-mode 'normal)
	    (evil-set-initial-state 'magit-log-mode 'normal)
	    (evil-define-key 'normal magit-mode-map
	      "j" 'magit-goto-next-section
	      "k" 'magit-goto-previous-section)
	    (evil-define-key 'normal magit-log-mode-map
	      "j" 'magit-goto-next-section
	      "k" 'magit-goto-previous-section)
	    (evil-define-key 'normal magit-diff-mode-map
	      "j" 'magit-goto-next-section
	      "k" 'magit-goto-previous-section)))

;;improves word wrapping
(visual-line-mode 1)

;;some software breaks if you don't do this
(setq require-final-newline t)

;;hide toolbar
(tool-bar-mode -1)

;;turn on highlighting matching brackets 
(show-paren-mode 1)

;;turn on auto-complete
(global-auto-complete-mode t)


(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
  'elisp-slime-nav-describe-elisp-thing-at-point)

(require 'my-ibuffer)

(require 'my-dired)
(require 'dired-x)


(put 'dired-find-alternate-file 'disabled nil)
