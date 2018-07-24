;;-----------------------------------------------------
;; packaging system
;;-----------------------------------------------------
;; load emacs 24's package system. Add MELPA repository.
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://stable.melpa.org/packages/"))

(package-initialize)

;; define all packages needed
(defvar lx-required-packages
  '(
    material-theme
    multiple-cursors
;    irony
    company                 ; c++
    company-irony           ; c++
    company-irony-c-headers ; c++
;    company-rtags
    flycheck            ;; c++
    flycheck-irony      ;; c++
    elpy                ;; python

					;    ein                 ;; python-interactive
    py-autopep8         ;; python-standard-conformance-check
    ) "a list of packages to ensure are installed at launch.")

;;-----------------------------------------------------
;; checking and installing missing packages
;;-----------------------------------------------------
(require 'cl)
; method to check if all packages are installed
(defun lx-packages-installed ()
  (cl-loop for p in lx-required-packages
           when (not (package-installed-p p)) do (return nil)
           finally (return t)))

; if not all packages are installed,
; check one by one and install the missing ones.
(unless (lx-packages-installed)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p lx-required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;-----------------------------------------------------
;; Editor customizations:
;; Hide: menu-bar, tool-bar, scrollbar, startup screen
;; Show: line numbers
;;-----------------------------------------------------
;; fboundp -- checks if arg is a function
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; do not show startup screen
(setq inhibit-startup-screen t)

; show line numbers
(when (fboundp 'global-linum-mode) (global-linum-mode t))

;;-----------------------------------------------------
;; Theme customization (colors, etc.)
;;-----------------------------------------------------
(load-theme 'material t)

;;-----------------------------------------------------
;; Multiple Cursors
;;-----------------------------------------------------
;;(require 'multiple-cursors)
(define-key global-map (kbd "C-c s a") 'mc/mark-all-like-this)
(define-key global-map (kbd "C-c s n") 'mc/mark-next-like-this)
(define-key global-map (kbd "M-N")      'mc/mark-next-lines)
(define-key global-map (kbd "M-S-<down>") 'mc/mark-next-lines)
(define-key global-map (kbd "M-S-<up>")   'mc/mark-previous-lines)
(define-key global-map (kbd "M-P")      'mc/mark-previous-lines)


(setq irony-additional-clang-options (quote ("-std=c++14")))
(setq flycheck-clang-language-standard (quote ("-std=c++14")))
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)

(define-key global-map (kbd "C-SPC")  'company-complete)

;; --------------------------------------
;; PYTHON CONFIGURATION
;; --------------------------------------

(setq elpy-rpc-python-command "python3")  ; for elpy
(setq python-shell-interpreter "python3") ; for interactive shell


(add-hook 'python-mode-hook 'elpy-mode)
(with-eval-after-load 'elpy
  (remove-hook 'elpy-modules 'elpy-module-flymake)
;  (add-hook 'elpy-mode-hook 'elpy-rpc-python-command)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
;  (add-hook 'elpy-mode-hook 'elpy-use-ipython)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))
