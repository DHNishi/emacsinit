(require 'package)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; Remove toolbars.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)

;;; Give proper line-wrapping.
;;; Add proper word wrapping
(global-visual-line-mode t)

;;; Actual overwriting of relevant keybinds.
;;; Overwrite 'help' and 'recenter-something-or-another'.
(global-set-key "\C-h" 'backward-word)
(global-set-key "\C-l" 'forward-word)

;;; Set up mark-ring management.
;; This overwrites the forward and back char.
(require 'show-marks)
;;; Set up mark-ring management.
(require 'show-marks)
(global-set-key "\M-l" 'forward-mark)
(global-set-key "\M-h" 'backward-mark)

;; Remap the kill-line command.
(global-set-key [(control \;)] 'kill-line)

;; Add in goto-line.
(global-set-key "\M-g" 'goto-line)

;; Add in an easy comment function.
(global-set-key (kbd "C-c c") 'comment-dwim)

;; Use a standard undo.
(global-set-key "\C-z" 'undo)

;; Overwrite 'open-line' with open file keybind.
(global-set-key "\C-o" 'ido-find-file)

;; Avoid using M-x when possible. (citation: Yegge)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;;; Avoid killing buffers accidentally.
;;; This doesn't actually work.
(defun ask-before-killing-buffer ()
  (let ((buffer (current-buffer)))
    (cond
     ((equal (buffer-name) "*scratch*")
      ;; Never kill *scratch*
      nil)
     ((and buffer-file-name (buffer-modified-p))
      ;; If there's a file associated with the buffer,
      ;; make sure it's saved
      (y-or-n-p (format "Buffer %s modified; kill anyway? "
			(buffer-name))))
     ((get-buffer-process buffer)
      ;; If there's a process associated with the buffer,
      ;; make sure it's dead
      (y-or-n-p (format "Process %s active; kill anyway? "
			(process-name (get-buffer-process buffer)))))
     (t t))))
(add-to-list 'kill-buffer-query-functions
             'ask-before-killing-buffer)

;;;;;;; Packages

;; Autocomplete everywhere.
(require 'auto-complete)
(global-auto-complete-mode t)

;; Change default font.
;; set font for all windows
(add-to-list 'default-frame-alist '(font . "Source Code Pro-16"))

;; dirty fix for having AC everywhere
;;;(define-globalized-minor-mode real-global-auto-complete-mode
;;;  auto-complete-mode (lambda ()
;;;                       (if (not (minibufferp (current-buffer)))
;;;                         (auto-complete-mode 1))
;;;                       ))
;;;(real-global-auto-complete-mode t)

;;; Python stuff.
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(setq column-number-mode t)

;;; Add in multi-cursor much like Sublime Text 2.
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;; Remote trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; sr-speedbar
(add-to-list 'load-path "~/.emacs.d/manual/")
(setq sr-speedbar-right-side nil)
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Autocomplete everywhere.
(require 'auto-complete)
(global-auto-complete-mode t)

;; Change default font.
;; set font for all windows
(add-to-list 'default-frame-alist '(font . "Source Code Pro-16"))


;; dirty fix for having AC everywhere
;;;(define-globalized-minor-mode real-global-auto-complete-mode
;;;  auto-complete-mode (lambda ()
;;;                       (if (not (minibufferp (current-buffer)))
;;;                         (auto-complete-mode 1))
;;;                       ))
;;;(real-global-auto-complete-mode t)

;;; Python stuff.
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(setq column-number-mode t)

;;; Add in multi-cursor much like Sublime Text 2.
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;; Remote trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; sr-speedbar
(require 'sr-speedbar)
(add-to-list 'load-path "~/.emacs.d/manual/")
(setq sr-speedbar-right-side nil)
(setq speedbar-frame-parameters
      '((minibuffer)
	(width . 40)
	(border-width . 0)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(unsplittable . t)
	(left-fringe . 0)))
(when window-system
  (defadvice sr-speedbar-open (after sr-speedbar-open-resize-frame activate)
    (set-frame-width (selected-frame)
                     (+ (frame-width) sr-speedbar-width)))
  (ad-enable-advice 'sr-speedbar-open 'after 'sr-speedbar-open-resize-frame)

  (defadvice sr-speedbar-close (after sr-speedbar-close-resize-frame activate)
    (sr-speedbar-recalculate-width)
    (set-frame-width (selected-frame)
                     (- (frame-width) sr-speedbar-width)))
  (ad-enable-advice 'sr-speedbar-close 'after 'sr-speedbar-close-resize-frame))

(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;;; Set up window management.
(defun back-window ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'back-window)

;;; Set up window size management.
(global-set-key (kbd "s-<down>") 'enlarge-window)
(global-set-key (kbd "s-<up>") 'shrink-window)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)

;;; Set up frame management.
(global-set-key "\M-`" 'other-frame)
(global-set-key "\C-c=" 'shell)

;;; Set up indentation rules.
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-j") 'newline)

;;; Makes it so yank does indentation.
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode
						     clojure-mode    scheme-mode
						     haskell-mode    ruby-mode
						     rspec-mode      python-mode
						     c-mode          c++-mode
						     objc-mode       latex-mode
						     plain-tex-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))

;;; Gotta have my parens.
(require 'smartparens)
(require 'smartparens-config)
(require 'paren)
(setq show-paren-style 'mixed)
(show-paren-mode +1)

;;; Activate IDO.
;;; This adds a great deal of autocompletion-y style stuff everywhere.
;;; http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (wheatgrass))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
