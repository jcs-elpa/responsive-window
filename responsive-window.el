;;; responsive-window.el --- Adapt to different screen sizes automatically  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/responsive-window
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (elenv "0.1.0"))
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

(require 'elenv)

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

(defcustom responsive-window-occupied-ratio 0.8
  "If the frame occupied this much from the monitor; remember the layout.

The value should be set from 0 to 1."
  :type 'float
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

(defvar responsive-window--was-reached nil
  "Record if it was max size.")

(defun responsive-window--reach-size ()
  "Reach the size will want to remember the layout."
  (let* ((m-width    (elenv-monitor-pixel-width))
         (m-height   (elenv-monitor-pixel-height))
         (f-width    (frame-pixel-width))
         (f-height   (frame-pixel-height))
         (max-size   (* m-width m-height))
         (frame-size (* f-width f-height)))
    (< (* max-size responsive-window-occupied-ratio) frame-size)))  ; if almost full monitor

(defun responsive-window--remember-layout (&optional force)
  "Remember the frame layout once.

If optional argument FORCE is non-nil, force remember it."
  (let ((reached (or force (responsive-window--reach-size))))
    (when reached
      (window-configuration-to-register responsive-window-register))
    (setq responsive-window--was-reached (if reached t nil))))

;; Initialize once
(responsive-window--remember-layout t)

(defun responsive-window--revert-layout ()
  "Revert window configuration."
  (save-selected-window
    (ignore-errors (jump-to-register responsive-window-register))))

;;
;;; Core

(defun responsive-window--do ()
  "Do responsive work."
  (let ((revert t))
    (dolist (win (window-list))
      (let* ((w (window-text-width win))
             (h (window-text-height win))
             (min-w2 (* 2 responsive-window-min-width))
             (min-h2 (* 2 responsive-window-min-height)))
        (cond ((or (< w  responsive-window-min-width)
                   (< h responsive-window-min-height))
               (ignore-errors (delete-window win)))
              (revert
               (when (or (< w min-w2)
                         (< h min-h2))
                 (setq revert nil))))))
    (when (and revert
               (not responsive-window--was-reached))
      (responsive-window--revert-layout))))

(defun responsive-window--size-change (&rest _)
  "Window resize hook."
  (unless (active-minibuffer-window)
    (responsive-window--do))
  (responsive-window--remember-layout))

(provide 'responsive-window)
;;; responsive-window.el ends here
