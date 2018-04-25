;;-----------------------------------------------------
;; packaging system
;;-----------------------------------------------------
;; load emacs 24's package system. Add MELPA repository.
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; define all packages needed
(defvar lx-required-packages
  '(
    material-theme
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
