;;Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives 
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;;Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
;;Enable Evil
(require 'evil)
(evil-mode 1)

(defun my-df ()
  (interactive)
  (let* ((initial-key ?d)
         (final-key ?f)
         (timeout 0.5)
         (event (read-event nil nil timeout)))
    (if event
        ;; timeout met
        (if (and (characterp event) (= event final-key))
            (evil-normal-state)
          (insert initial-key)
          (push event unread-command-events))
      ;; timeout exceeded
      (insert initial-key))))

(define-key evil-insert-state-map (kbd "d") 'my-df)


;;Linum
(require 'linum-relative)
(global-linum-mode 1)
(linum-relative-on)
(setq linum-relative-current-symbol "")


(custom-set-variables
 '(package-selected-packages (quote (linum-relative evil))))
(custom-set-faces)

;;graphics init
(if (display-graphic-p)
    (setq initial-frame-alist
          '(
            (font . "iosevka")
            (menu-bar-lines . 0)
            (tool-bar-lines . 0)
            (width . 106)
            (height . 60)
            (background-color . "black")
            (left . 50)
            (top . 50)))
  (setq initial-frame-alist '( (tool-bar-lines . 0))))
(setq default-frame-alist initial-frame-alist)


;;show matching paren
(show-paren-mode 1)
