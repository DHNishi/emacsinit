

(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

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

;; Overwrite 'open-line' with open file keybind.
(global-set-key "\C-o" 'ido-find-file)

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
