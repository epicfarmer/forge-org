(require 'forge)

;;; Code:

;;; Classes:

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

;;; Get:

(defvar repository-default-name '"No Repository")
(defvar milestone-default-name '"No Milestone")

(cl-defmethod forge-org-get-repository-org (issue)
  "Convert ISSUE's repository information into an org buffer at cursor in FORGE-ORG-FILE-NAME."
  (let* (
	 (repository (oref issue :repository-id))
	 (repository-name (if (oref issue :repository-name) (oref issue :repository-name) repository-default-name))
	 (owner (oref issue :owner))
	 (forge (oref issue :forge))
	 )
    ;; Skip if current repository has already been added.
    (if (or (not repository) (string= current-repository repository))
      current-repository
      (progn
	;; Insert heading
	(find-file forge-org-file-name)
	(org-insert-heading nil nil 1)
	(insert repository-name)
	(newline 1)
	;; Add properties
	(org-entry-put nil '"repository-id" repository)
	(org-entry-put nil '"repository" repository-name)
	(org-entry-put nil '"owner" owner)
	(org-entry-put nil '"forge" forge)
	;; Return repository
	(setq current-repository repository)
	(setq current-milestone nil)
	repository
	)
      )
    )
  )

(cl-defmethod forge-org-get-milestone-org (issue)
  "Convert ISSUE's milestone information into an org buffer at cursor in FORGE-ORG-FILE-NAME."
  (let* (
	 (raw-milestone (oref issue :milestone))
	 (milestone (if raw-milestone raw-milestone milestone-default-name))
	 )
    (if (and current-milestone (string= current-milestone milestone))
	current-milestone
      (progn
	;; Insert heading
	(find-file forge-org-file-name)
	(org-insert-heading nil nil 2)
	(insert milestone)
	(newline 1)
	;; Return milestone
	(setq current-milestone milestone)
	milestone
	)
      )
    )
  )

(cl-defmethod forge-org-get-issue-org (issue)
  "Convert ISSUE's issue information into an org buffer at cursor in FORGE-ORG-FILE-NAME."
  (let* (
	 (status (oref issue :state))
	 (priority (oref issue :priority))
	 (title (oref issue :title))
	 (scheduled (oref issue :scheduled))
	 (deadline (oref issue :due))
	 (issue-id (oref issue :id))
	 (assignee (oref issue :assignee-login))
	 (assignee-name (oref issue :assignee-name))
	 (assignee-id (oref issue :assignee-id))
	 (schedule-id (oref issue :schedule-id))
	 (logbook-result (oref issue :logbook))
	 )
    (progn
      (find-file forge-org-file-name)
      (org-insert-heading nil nil 3)
      (insert title)
      (newline 1)
      (org-entry-put nil '"TODO" (if (string= status '"open") '"TODO" '"DONE"))
      (org-entry-put nil '"PRIORITY" priority)

      (if (not (string= scheduled '"")) (org-entry-put nil '"SCHEDULED" scheduled))
      (if (not (string= deadline '"")) (org-entry-put nil '"DEADLINE" deadline))
      (if issue-id (org-entry-put nil '"issue-id" issue-id))
      (if assignee (org-entry-put nil '"assignee" assignee)) ;; Switch to multivalued property
      (if assignee-id (org-entry-put nil '"assignee-id" assignee-id)) ;; Switch to multivalued property
      (if schedule-id (org-entry-put nil '"schedule-id" (format '"%d" schedule-id)))
      (if (not (string= logbook-result '"")) (insert-logbook-entry logbook-result))
      )
    )
  )

(defun forge-org-org-to-issue (filename issue-min issue-max)
  (progn
    (find-file filename)
    (goto-char issue-min)
    (let* ((raw-repository-name (org-entry-get nil '"repository" t))
	   (repository-name (if (string= raw-repository-name repository-default-name) nil raw-repository-name))
	   (raw-milestone-name (org-entry-get nil '"milestone" t))
	   (milestone-name (if (string= raw-milestone-name milestone-default-name) nil raw-milestone-name))
	   (logbook-entry (search-within-issue issue-min issue-max '"^\\( *CLOCK: .*\\)$" )))
      (message raw-repository-name)
      (make-instance 'forge-org-issue
		     :repository-id   (org-entry-get nil '"repository-id" t)
		     :forge           (org-entry-get nil '"forge" t)
		     :owner           (org-entry-get nil '"owner" t)
		     :repository-name repository-name
		     :milestone       (org-entry-get nil '"milestone" t)
		     :id              (org-entry-get nil '"issue-id" nil)
		     :state           (org-entry-get nil '"TODO" nil)
		     :title           (org-entry-get nil '"ITEM" nil)
		     :assignee-id     (org-entry-get nil '"assignee-id" nil)
		     :assignee-login  (org-entry-get nil '"assignee" nil)
		     :assignee-name   (org-entry-get nil '"assignee-name" nil)
		     :schedule-id     (org-entry-get nil '"schedule-id" nil)
		     :scheduled       (org-entry-get nil '"SCHEDULED" nil)
		     :due             (org-entry-get nil '"DEADLINE" nil)
		     :logbook         logbook-entry
		     :priority        (org-entry-get nil '"PRIORITY" nil)
		     )
      )
    )
  )

(provide 'forge-schedule)
