;;; github-marathon-notifier.el --- Check if you have finished today's marathon task -*- lexical-binding: t; -*-

;; Copyright (C) 2016  DarkSun

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/lujun9972/github-marathon-notifier
;; Package-Requires: ((emacs "24"))
;; Keywords: github, mode-line

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a global minor-mode. Turn it on everywhere with:
;;
;;   M-x github-marathon-notifier-mode

;;; Code:
(require 'url)

(defgroup github-marathon-notifier nil
  "Github Marathon Notifier"
  :group 'emacs)

(defcustom github-marathon-notifier-user user-login-name
  "Who will be checked"
  :group 'github-marathon-notifier
  :type 'string)

(defcustom github-marathon-notifier-check-interval 600
  "Seconds after which the github marathon notifier will check if you have finished today's task."
  :type 'integer
  :group 'github-marathon-notifier)

(defvar github-marathon-notifier-check-timer nil
  "Timer which perform the check action.")

(defvar github-marathon-notifier-commit-count nil 
  "How many github commit have been made today.")

(defun github-marathon-notifier-indicator (commit-count)
  (let (text face)
    (cond ((equal 0 commit-count)
           (setq text "-!"
                 face font-lock-warning-face))
          ((null commit-count)
           (setq text "-?"
                 face font-lock-warning-face))
          (t
           (setq text ""
                 face nil)))
    (propertize (concat " GMH" text)
                'face face)))

(defun github-marathon-notifier--start-check (interval)
  (setq github-marathon-notifier-check-timer (run-with-timer 0 interval #'github-marathon-notifier-check)))

(defun github-marathon-notifier--stop-check ()
  (when (timerp github-marathon-notifier-check-timer)
    (cancel-timer github-marathon-notifier-check-timer))
  (setq github-marathon-notifier-check-timer nil))

(defun github-marathon-notifier--update-modeline (indicator)
  (setf (cadr (assoc 'github-marathon-notifier-mode minor-mode-alist)) indicator)
  (force-mode-line-update t))

(defun github-marathon-notifier-check-cb (_status)
  (set-buffer-multibyte t)
  (if (string-match "200 OK" (buffer-string))
      (let* ((today (format-time-string "%Y-%m-%d"))
             (regex (format "data-count=\"\\([0-9]+\\)\" data-date=\"%s\"" today)))
        (goto-char (point-max))
        (re-search-backward regex nil 'move)
        (setq github-marathon-notifier-commit-count (string-to-number (match-string 1))))
    (message "[github-marathon-notifier] Problem connecting to the server")
    (setq github-marathon-notifier-commit-count nil))
  (kill-buffer)
  (github-marathon-notifier--update-modeline (github-marathon-notifier-indicator github-marathon-notifier-commit-count))
  (when (and (numberp github-marathon-notifier-commit-count)
             (> github-marathon-notifier-commit-count 0))
    ;; 若今天已经签到成功，则今天不用再做检查了
    (github-marathon-notifier--stop-check)
    ;; 第二天开始重新开始检查
    (run-at-time "00:00am" nil #'github-marathon-notifier--start-check github-marathon-notifier-check-interval)))

(defun github-marathon-notifier-check (&optional user)
  "Check weather USER has finished today's task"
  (let* ((user (or user github-marathon-notifier-user))
         (condibutions-url (format "https://github.com/users/%s/contributions" user)))
    (url-retrieve condibutions-url #'github-marathon-notifier-check-cb nil t t)))


(define-minor-mode github-marathon-notifier-mode
  "Toggle github notifications count display in mode line (Github Notifier mode).
With a prefix argument ARG, enable Github Notifier mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  nil
  " GMH-?"
  :global t :group 'github-marathon-notifier
  (if github-marathon-notifier-mode
      (github-marathon-notifier--start-check github-marathon-notifier-check-interval)
    (github-marathon-notifier--stop-check)))

(provide 'github-marathon-notifier)
;;; github-marathon-notifier.el ends here

