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

(defun keys ()
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-c C-k") 'copy-line)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

  
(defun hooks ()
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'latex-mode-hook 'prettify-symbols-mode)
  (add-hook 'haskell-literate-mode-hook #'lsp))

(defun misc-options ()
  (setq inhibit-startup-screen t)
  (ac-config-default)

  (show-paren-mode 1)
  (menu-bar-mode -1)
;;  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (projectile-mode +1)
  (helm-mode 1)
  (desktop-save-mode 1)
  ;;Scroll one line at a time
  (setq scroll-step 1))

(pkg-init)
(install-packages '(multiple-cursors magit haskell-mode lsp-mode lsp-ui lsp-haskell racket-mode helm projectile ibuffer-projectile auto-complete org-bullets tablist))
(graphics)
(keys)
(hooks)
(linum-init)
(misc-options)

;recursion should be eliminated here, but it's way easier to write this way, for now...
(defun _future-date (date n)
  (if (= n 0)
      date
    (let ((year (car date))
	  (month (cadr date))
	  (day (caddr date)))
      (cond ((eq month 12)
	     (if (eq day 31)
		 (_future-date (list (+ year 1) 1 1) (- n 1))
	       (_future-date (list year month (+ day 1)) (-n 1))))
	    ((or (eq month 1) (eq month 3) (eq month 5) (eq month 7) (eq month 8) (eq month 10))
	     (if (eq day 31)
		 (_future-date (list year (+ month 1) 1) (- n 1))
	       (_future-date (list year month (+ day 1)) (- n 1))))
	    ((eq month 2)
	     (if (eq day 28)
		 (_future-date (list year (+ month 1) 1) (- n 1))
	       (_future-date (list year month (+ day 1)) (- n 1))))
	    ((or (eq month 4) (eq month 6) (eq month 9) (eq month 11))
	     (if (eq day 30)
		 (_future-date (list year (+ month 1) 1) (- n 1))
	       (_future-date (list year month (+  1)) (- n 1))))))))

;;wasted computation transforming string-to-number in lambda of mapcar in let body.
(defun future-date (n)
  (let* ((date_string (format-time-string "%Y-%m-%d"))
	 (ds_list (split-string date_string "-"))
	 (fd_as_number (_future-date (mapcar 'string-to-number ds_list) n))
	 (fd_as_string (mapcar 'number-to-string fd_as_number)))
    (apply 'concat
		   (cons (car fd_as_string)
			 (mapcar (lambda (x)
				   (if (< (string-to-number x) 10)
				       (concat "-0" x)
				     (concat "-" x)))
				 (cdr fd_as_string))))))

(defun copy-line (arg)
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;;workouts:
(defvar back
  '("bed-rows" "kettlebell-rows" "kettlebell-deadlifts" "floor-swims"))
(defvar biceps
  '("kettlebell-curls"))
(defvar chest
  '("kettlebell-press" "pushups" "overhead-thing" "closegrip-press"))
(defvar triceps
  '("kettlebell-extensions"))
(defvar shoulders
  '("kettlebell-shoulder-press" "swings" "cleans" "shrugs"))
(defvar core
  '("crunches"))
(defvar task_list
  '("Take out trash" "Clean apartment" "Study" "Workout" "Yoga/meditation" "Call or message family and friends"))

(defun insert-list (L)
  (unless (null L)
    (progn (insert (car L))
	   (insert-list (cdr L)))))

(defun exercises (G1 E1 G2 E2)
  (save-excursion 
    (insert "* " (format-time-string "%Y-%m-%d") "\n")
    (insert "** " G1 "\n")
    (insert-list (mapcar (lambda (x) (concat "*** " x ": \n")) E1))
    (insert "** " G2 "\n")
    (insert-list (mapcar (lambda (x) (concat "*** " x ": \n")) E2))))



(defun backBi ()
  (interactive)
  (exercises "Back" back "Biceps" biceps))
(defun chestTri ()
  (interactive)
  (exercises "Chest" chest "Triceps" triceps))
(defun shouldersCore ()
  (interactive)
  (exercises "Shoulders" shoulders "Core" core))
(defun tasks ()
  (interactive)
  (save-excursion
    (insert "* Week of: " (format-time-string "%Y-%m-%d") " - " (future-date 7) " [/] \n")
    (insert-list (mapcar (lambda (x) (concat "** TODO " x ": \n")) task_list))))

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
