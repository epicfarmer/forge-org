(require 'forge)

;;; Code:

(defclass forge-org-issue ()
  ((repository-id :initarg :repository-id)
   (forge :initarg :forge)
   (owner :initarg :owner)
   (repository-name :initarg :repository-name)
   (milestone :initarg :milestone)
   (id :initarg :id)
   (state :initarg :state)
   (title :initarg :title)
   (assignee-id :initarg :assignee-id)
   (assignee-login :initarg :assignee-login)
   (assignee-name :initarg :assignee-name)
   (schedule-id :initarg :schedule-id)
   (scheduled :initarg :scheduled)
   (due :initarg :due)
   (logbook :initarg :logbook)
   (priority :initarg :priority)
   ))

(provide 'forge-schedule)
