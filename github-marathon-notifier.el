(require 'url)

(defgroup github-marathon-notifier nil
  "Github Marathon Notifier"
  :group 'emacs)

(defcustom github-marathon-notifier-user user-login-name
  "Who will be checked"
  :group 'github-marathon-notifier
  :type 'string)

(defcustom github-marathon-notifier-mode-line
  '(:eval
    (let (unread-text help-text)
      (cond ((= 0 github-marathon-notifier-commit-count)
             (setq unread-text "-!"
                   help-text "You haven't push any commit yet!"))
            ((null github-marathon-notifier-commit-count)
             (setq unread-text "-?"
                   help-text "connect to github error!"))
            (t
             (setq unread-text ""
                   help-text "Good job.")))
      (propertize (concat " GMH" unread-text)
                  'help-echo help-text)))
  "Mode line lighter for Github Notifier."
  :type 'sexp
  :group 'github-marathon-notifier)

(defcustom github-marathon-notifier-check-interval 600
  "Seconds after which the github marathon notifier will check if you have finished today's task."
  :type 'integer
  :group 'github-marathon-notifier)

(defvar github-marathon-notifier-check-timer nil
  "Timer which perform the check action.")

(defvar github-marathon-notifier-commit-count 0
  "How many github commit have been made today.")

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
  (kill-buffer))

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
  :global t :group 'github-marathon-notifier
  (unless global-mode-string
    (setq global-mode-string '("")))
  (if (not github-marathon-notifier-mode)
      (progn
        (cancel-timer github-marathon-notifier-check-timer)
        (setq global-mode-string (delq 'github-marathon-notifier-mode-line global-mode-string)
              github-marathon-notifier-check-timer nil))
    (add-to-list 'global-mode-string 'github-marathon-notifier-mode-line t)
    (setq github-marathon-notifier-check-timer (run-with-timer 0 github-marathon-notifier-check-interval #'github-marathon-notifier-check))))

(provide 'github-marathon-notifier)
;;; github-marathon-notifier.el ends here

