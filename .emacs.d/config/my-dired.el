(defun my-dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternate-file"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)))

(eval-after-load 'dired
  '(progn
     (evil-define-key 'normal dired-mode-map
       (kbd "h") 'my-dired-up-directory
       (kbd "l") 'dired-find-alternate-file
       (kbd "o") 'dired-sort-toggle-or-edit
       (kbd "v") 'dired-toggle-marks
       (kbd "m") 'dired-mark
       (kbd "u") 'dired-unmark
       (kbd "U") 'dired-unmark-all-marks
       (kbd "c") 'dired-create-directory
       (kbd "n") 'evil-search-next
       (kbd "N") 'evil-search-previous
       (kbd "q") 'kill-this-buffer)))
(evil-set-initial-state 'dired-mode 'normal)
(provide 'my-dired)
	    
       
