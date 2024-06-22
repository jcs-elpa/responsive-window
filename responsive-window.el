;;; responsive-window.el --- Adapt to different screen sizes automatically  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs090218/responsive-window
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: frames

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Adapt to different screen sizes automatically.
;;

;;; Code:

(defgroup responsive-window nil
  "Adapt to different screen sizes automatically."
  :prefix "responsive-window-"
  :group 'frames
  :link '(url-link :tag "Github" "https://github.com/jcs-elpa/responsive-window"))

(defcustom responsive-window-register ?h
  "Window configuration's register."
  :type 'symbol
  :group 'responsive-window)

(defcustom responsive-window-min-width 60
  "Minimum window width to view."
  :type 'integer
  :group 'responsive-window)

(defcustom responsive-window-min-height 15
  "Minimum window height to view."
  :type 'integer
  :group 'responsive-window)

;;
;;; Entry

(defun responsive-window--enable ()
  "Enable `responsive-window-mode'."
  (add-hook 'window-size-change-functions #'responsive-window--size-change))

(defun responsive-window--disable ()
  "Disable `responsive-window-mode'."
  (remove-hook 'window-size-change-functions #'responsive-window--size-change))

;;;###autoload
(define-minor-mode responsive-window-mode
  "Minor mode `responsive-window-mode'."
  :global t
  :require 'responsive-window-mode
  :group 'responsive-window
  (if responsive-window-mode (responsive-window--enable)
    (responsive-window--disable)))

;;
;;; Layout

(defvar responsive-window--was-max-size nil
  "Record if it was max size.")

(defun responsive-window--reach-size ()
  "Reach the size will want to remember the layout."
  (let ((max-size (* (display-pixel-width) (display-pixel-height)))
        (frame-size (* (frame-pixel-width) (frame-pixel-height))))
    (< (* max-size 0.8) frame-size)))

(defun responsive-window--remember-layout ()
  "Remember the frame layout once."
  (let ((reached (responsive-window--reach-size)))
    (when reached
      (window-configuration-to-register responsive-window-register))
    (setq responsive-window--was-max-size (if reached t nil))))

(defun responsive-window--revert-layout ()
  "Revert window configuration."
  (save-selected-window
    (ignore-errors (jump-to-register responsive-window-register))))

;;
;;; Core

(defun responsive-window--list ()
  "Return the window list."
  (seq-filter (lambda (win) (not (equal (minibuffer-window) win)))
              (reverse (window-list))))

(defun responsive-window--do ()
  "Do responsive work."
  (let ((revert t))
    (dolist (win (responsive-window--list))
      (let* ((w (window-text-width win))
             (h (window-text-height win))
             (min-w2 (* 2 responsive-window-min-width))
             (min-h2 (* 2 responsive-window-min-height)))
        (when (or (< w  responsive-window-min-width)
                  (< h responsive-window-min-height))
          (delete-window win))
        (when revert
          (when (or (< w min-w2)
                    (< h min-h2))
            (setq revert nil)))))
    (when (and revert
               (not responsive-window--was-max-size))
      (responsive-window--revert-layout))))

(defun responsive-window--size-change (&rest _)
  "Window resize hook."
  (unless (active-minibuffer-window)
    (responsive-window--do))
  (responsive-window--remember-layout))

(provide 'responsive-window)
;;; responsive-window.el ends here
