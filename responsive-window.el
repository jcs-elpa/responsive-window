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

(defcustom responsive-window-min-height 0
  "Minimum window height to view."
  :type 'integer
  :group 'responsive-window)

(defcustom responsive-window-occupied-ratio 0.8
  "If the frame occupied this much from the monitor; remember the layout.

The value should be set from 0 to 1."
  :type 'float
  :group 'responsive-window)

(defvar responsive-window--first-reached-p nil
  "Set to t if first reached occupied ratio.")

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

(defvar responsive-window--dirty-p nil
  "Record if the frame is polluted.")

(defvar responsive-window--dirty-buffers nil
  "Keep the dirty buffers.")

(defun responsive-window--reach-size ()
  "Reach the size will want to remember the layout."
  (let* ((m-width    (elenv-monitor-pixel-width))
         (m-height   (elenv-monitor-pixel-height))
         (f-width    (frame-pixel-width))
         (f-height   (frame-pixel-height))
         (max-size   (* m-width m-height))
         (frame-size (* f-width f-height)))
    (< (* max-size responsive-window-occupied-ratio) frame-size)))  ; if almost full monitor

(defun responsive-window--remember-layout ()
  "Remember the frame layout once."
  (window-configuration-to-register responsive-window-register)
  (setq responsive-window--first-reached-p t))

(defun responsive-window--revert-layout ()
  "Revert window configuration."
  (save-selected-window
    (ignore-errors (jump-to-register responsive-window-register)))
  (setq responsive-window--dirty-p nil)
  (walk-windows
   (lambda (win)
     (when-let* ((buf (pop responsive-window--dirty-buffers))
                 ((buffer-live-p buf)))
       (set-window-buffer win buf)))))

;;
;;; Core

(defun responsive-window--do ()
  "Do responsive work."
  (when responsive-window--first-reached-p
    (setq responsive-window--dirty-buffers nil)  ; clear
    (dolist (win (reverse (window-list)))
      (let ((w   (window-text-width win))
            (h   (window-text-height win))
            (buf (window-buffer win)))
        (cond ((or (< w  responsive-window-min-width)
                   (< h responsive-window-min-height))
               (ignore-errors (delete-window win))
               (setq responsive-window--dirty-p t))
              (t
               (push buf responsive-window--dirty-buffers)))))))

(defun responsive-window--size-change (&optional frame &rest _)
  "Window resize hook.

Argument FRAME is the effected frame."
  (elenv-with-no-redisplay
    (cond ((responsive-window--reach-size)
           (cond (responsive-window--dirty-p
                  (responsive-window--revert-layout))
                 (t
                  (responsive-window--remember-layout))))
          (t
           (unless (or (frame-parent frame)  ; only root frame
                       (active-minibuffer-window))
             (responsive-window--do))))))

(provide 'responsive-window)
;;; responsive-window.el ends here
