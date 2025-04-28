;;; forge-org.el --- Create org file for integrating scheduling with issues managed with forge.

;; Author: epicfarmer <magicgathering2001 at gmail dot com>
;; Maintainer: epicfarmer <magicgathering2001 at gmail dot com>
;; Created: 2021-03-24
;; Keywords: org, synchronization, issue tracking, forge
;; Homepage: https://github.com/epicfarmer/forge-org
;; Package-Requires: ((cl-lib "0.5") (org "8.2") (emacs "28") (forge "0.5"))
;; Version: 0.3.0
;; This file is not part of gnu emacs

;;; package --- Summary
;; forge-org is a package for using org-mode as to integrate forge tasks with an org mode scheduling interface
;;; Commentary:
;; The database and the org mode file both contain the same information.  If either is deleted, it should be obtainable from the other.
;; If both are deleted, you'll need to rebuild the org file by hand (though forge can rebuild the database).

(require 'forge)
(require 'org)
(require 'forge-schedule)

;;; Code:

(defcustom forge-org-file-name
  (expand-file-name "forge.org"  user-emacs-directory)
  "The file forge-org writes forge topics to."
  :package-version '(forge . "0.3.0")
  :group 'forge-org
  :type 'file)

(defcustom forge-org-issue-filters
  (list
   '"((issue.state != 'completed'))"
   )
  "A list of string filters to determine which issues to pull from the forge databse.  Issues that satisfy all filters will be returned.  This is passed raw to the forge-sqlite."
  :package-version '(forge . "0.3.0")
  :group 'forge-org
  :type 'list)

(defvar forge-org-query-path
  (file-name-concat (file-name-directory (or load-file-name (buffer-file-name))) '"forge-query.sql")
  )

;; This seems like poor elisp style
(defvar current-repository '"")
(defvar current-milestone '"")
(defvar current-milestone-name '"")

(defun construct-forge-issue-query (filters)
  "Get a query to find appropriate issues from the forge database where appropriate is determined by FILTERS."
  (let* (
	 (non-nil-filters (if filters filters '"TRUE"))
	 (filter-sql-string (if (listp non-nil-filters) (string-join non-nil-filters '"AND") non-nil-filters))
	 (sql-query-raw (with-temp-buffer (insert-file-contents forge-org-query-path) (buffer-string)))
	 (sql-query (format sql-query-raw filter-sql-string))
	 )
    (message filter-sql-string)
    sql-query
    )
  )

(defun query-row-to-issue (query-row)
  (make-instance 'forge-org-issue
		 :repository-id   (nth  0 query-row)
		 :forge           (nth  1 query-row)
		 :owner           (nth  2 query-row)
		 :repository-name (nth  3 query-row)
		 :milestone       (nth  4 query-row)
		 :id              (nth  5 query-row)
		 :state           (nth  6 query-row)
		 :title           (nth  7 query-row)
		 :assignee-id     (nth  8 query-row)
		 :assignee-login  (nth  9 query-row)
		 :assignee-name   (nth 10 query-row)
		 :schedule-id     (nth 11 query-row)
		 :scheduled       (nth 12 query-row)
		 :due             (nth 13 query-row)
		 :logbook         (nth 14 query-row)
		 :priority        (nth 15 query-row)
		 )
  )

(defun issue-forge-query (filters)
  "Query the forge database for issues matching \"FILTERS\"."
					;TODO deal with filters
  ;; Output indices (for use with nth)
					;  0 repository.id
					;  1 repository.forge
					;  2 repository.owner
					;  3 repository.name
					;  4 issue.milestone
					;  5 issue.id
					;  6 issue.state
					;  7 issue.title
					;  8 assignee.id
					;  9 assignee.login
					; 10 assignee.name
					; 11 issue_schedule.id
					; 12 issue_schedule.scheduled
					; 13 issue_schedule.due
					; 14 issue_schedule.clock
					; 15 issue_schedule.priority
  (let* ((query-results (forge-sql (construct-forge-issue-query filters)))
	 (query-issues (mapcar 'query-row-to-issue query-results))
	 )
    query-issues
    )
  )


(defun insert-logbook-entry (entry)
  "Insert a :LOGBOOK: entry at cursor position.  ENTRY is the contents of the logbook."
  (progn
    (insert '":LOGBOOK:")
    (newline 1)
    (insert entry)
    (newline 1)
    (insert '":END:")
    (newline 1)
    t
    )
  )
    

(defun sql-result-to-org (sql-result)
  "Convert an issue in the form of an sql result into org mode text.  \"SQL-RESULT\" is the sql result to convert."
  (progn
    (message (prin1-to-string sql-result))
    (forge-org-get-repository-org sql-result)
    (forge-org-get-milestone-org sql-result)
    (forge-org-get-issue-org sql-result)
    )
  )

(defun sql-to-org (sql-results)
  "Convert an entire sql query into org mode text.  \"SQL-RESULTS\" is the sql result to convert."
  (progn
    (setq current-repository nil)
    (setq current-milestone nil)
    (find-file forge-org-file-name)
    (erase-buffer)
    (insert '"# Issue tracking for use with forge")
    (newline 1)
    (insert '"#+BEGIN: clocktable :maxlevel 4 :scope file")
    (newline 1)
    (insert '"#+END:")
    (newline 1)
    (insert '"#+PRIORITIES: A E C")
    (newline 1)
    (mapc 'sql-result-to-org sql-results)
    )
  )

(defun test-sql-query (filters)
  "This is a test of the sql query used to get issues and their scheduling (as filtered by FILTERS)."
  (concat

   '"SELECT
       repository.id,
       repository.forge,
       repository.owner,
       repository.name,
       issue.milestone,
       issue.id,
       issue.state,
       issue.title,
       assignee.id,
       assignee.login,
       assignee.name
     FROM
       ((issue LEFT JOIN repository ON issue.repository == repository.id)
         LEFT JOIN
        (issue_assignee LEFT JOIN assignee ON issue_assignee.id == assignee.id)
         ON
        issue_assignee.issue == issue.id
       )"
   (if filters
       (concat
	'"WHERE ("
	(if (listp filters)
	    (string-join filters '" AND ")
	  filters)
	'")"
	)
     nil)
   )
  )

(defun search-within-issue (issue-min issue-max regex)
  "Search for text within the body of an \"org-mode\" issue.  Start at ISSUE-MIN, go to ISSUE-MAX, find the first thing matching REGEX.  There is nothing about this function that is particular to issues."
  (if (> issue-min issue-max)
      nil
    (let* ((rc '""))
      (progn
	(goto-char issue-min)
	(setq rc '"")
	(while (re-search-forward regex issue-max t);Get it's id
	  (setq rc (concat rc (if (not (string= rc '"")) '"\n" '"") (match-string 1)))
	  nil)
	rc)))
  )

(defun org-to-issue-list (filename)
  "Read through an org file FILENAME, and create a list of issues."
  (let* ((issues-list (list))
	 (issue-min (point-max))
	 (issue-max (point-max))
	 (this-id)
	 (this-schedule-id)
	 (this-schedule)
	 (this-deadline)
	 (this-clock)
	 (this-priority)
	 (this-issue))
  (progn
    (find-file filename)
    (goto-char (point-max))
    (while (re-search-backward '"^[*][*][*] " (point-min) t)    ;Find next issue
      (progn
	;; Set bounds for issue
	(setq issue-max issue-min)
	(setq issue-min (point))
	(let*

	    
	    ((this-id (org-entry-get nil '"issue-id"))
	     (this-schedule-id (org-entry-get nil '"schedule-id"))
	     (this-schedule (org-entry-get nil '"SCHEDULED"))
	     (this-deadline (org-entry-get nil '"DEADLINE"))
	     (this-clock (search-within-issue issue-min issue-max '"^\\( *CLOCK: .*\\)$" ))
	     (this-priority (org-entry-get nil '"PRIORITY"))
	     (this-status (org-entry-get nil '"TODO"))
	     (this-title (org-entry-get nil '"ITEM"))
	     (this-issue (list this-id this-schedule-id this-schedule this-deadline this-clock this-priority this-status this-title))); Construct issue
	  ;; Create issue
	  (setq issues-list (cons this-issue issues-list))
	  )
	(goto-char issue-min)
	))
    (save-buffer)
    (kill-buffer)
    issues-list))
  )

(defun diff-issue-list-with-database (filename)
  (progn
    (mapc 'diff-and-update-issue (org-to-issue-list filename))
    (sql-to-org (issue-forge-query forge-org-issue-filters))
    )
  )

(defun get-default-todo ()
  (car (cdr (car org-todo-keywords)))
  )

(defun diff-and-update-issue (issue)
  (let* (
	 (issue-id (nth 0 issue))
	 (schedule-id (nth 1 issue))
	 (scheduled (nth 2 issue))
	 (due (nth 3 issue))
	 (clock (nth 4 issue))
	 (priority (nth 5 issue))
	 (status (nth 6 issue))
	 (issue-title (nth 7 issue))
	 )
    (if (not issue-id)
	(progn
	  (message '"Issue creation is not yet integrated. Please create the issue for this entry manually")
	  (message issue-id)
	  )
      (progn
	;; If we don't have a schedule id, create the entry
	(if (or (not schedule-id) (string= schedule-id '""))
	    (let*
		(
		 (query (concat
			 '"INSERT INTO issue_schedule(issue,scheduled,due,clock,priority) "
			 '"VALUES('\"" (string-join (cons (car issue) (cdr (cdr issue))) "\"', '\"") "\"')"))
		 )
	      (progn
		(forge-sql query)
		)
	      )
	  )
	;; If we have (or created) a schedule id, update the parameters
	(if (and schedule-id (not (string= schedule-id '"")))
	    (let*
		((query (
			 concat
			 '"UPDATE issue_schedule SET due = '\"" (nth 3 issue)
			 '"\"', scheduled = '\"" (nth 2 issue)
			 '"\"' , clock= '\"" (nth 4 issue)
			 '"\"' , priority= '\"" (nth 5 issue)
			 '"\"' WHERE id == " (nth 1 issue))
			))
	      (progn
		(forge-sql query)
		)
	      )
	  )
	(if (and status (not (string= status '"")))
	    (let*
		(
		 (query (format '"SELECT state from issue where id = '\"%s\"'" (nth 0 issue)))
		 (forge-state (nth 0 (nth 0 (forge-sql query))))
		 (expected-forge-state (if (string= status '"DONE") 'completed 'open))
		 )
	      (if (not (string= forge-state expected-forge-state))
		  (progn 
		    (message (format '"Changing issue %s state for issue from %s to %s" issue-id forge-state status))
		    ;;    (forge-org-edit-topic-state-by-id issue-id)
		    )
		)
	      )
	  )
	)
      )
    )
  )

(defun forge-destroy-scheduling-table ()
  (forge-sql '"DROP TABLE issue_schedule"
	     ))

(defun forge-create-scheduling-table ()
  (forge-sql '"CREATE TABLE IF NOT EXISTS issue_schedule (
  id integer PRIMARY KEY,
  issue text NOT NULL,
  due text,
  scheduled text,
  clock text,
  priority text
)"
  ))

(defun forge-org-update
    ()
  (interactive)
  (progn
    ;;(forge-org-pull-all-forges)
    (diff-issue-list-with-database forge-org-file-name)
    (save-buffer)))

(defun forge-org-jump-to-forge
    ()
  (interactive)
  (let*
      ((repository-name (org-entry-get nil '"repository" t)))
    (progn
      (magit-list-repositories)
      (switch-to-buffer (other-buffer (current-buffer) t))
      (goto-char (point-min))
      (
       re-search-forward
       (concat '"^" repository-name '" ")
       (point-max)
       t)    ;Find next issue
      (magit-repolist-status)
      (switch-to-buffer (other-buffer (current-buffer) t))
      (kill-buffer)
      )
    )
  )

(defun forge-org-get-all-repositories-in-database ()
  (mapcar (lambda (repo) (forge-get-repository repo)) (forge-sql "select distinct forge,owner,name from repository")))

(defun forge-org-pull-all-forges (&optional until)
  (interactive)
  (dolist (repo (forge-org-get-all-repositories-in-database))
    ;; This call is not ideal, since it gives an error, but it works so leaving in for now.
    ;; The issue is a problem with forge, where it doesn't appropriately pass the repo to the callback
    (forge--pull repo until)
    (sit-for 1)
    ))

(defun forge-org-create-bug (.title .body .labels .assignees .state .forge .owner .repo)
  "Create an issue for a forge defined by .HOST .OWNER and .REPO from a .TITLE .BODY .LABELS .ASSIGNEES and .STATE ."
  ;; This is a ghub call because normal forge calls fail to recognize the repo in the callbacks
  (forge--ghub-post (forge-get-repository (list .forge .owner .repo)) "/repos/:owner/:repo/issues"
    `((title . , .title)
      (body  . , .body)
      ,@(and .labels    (list (cons 'labels    .labels)))
      ,@(and .assignees    (list (cons 'assignees    .assignees)))
      )
    ;; :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)
    )
  )

(defun forge-org-add-bug-to-forge ()
  "Create a bug and add to the forge from the currently selected TODO item."
  (interactive)
  (let* ((forge (org-entry-get nil '"forge" t))
	 (owner (org-entry-get nil '"owner" t))
	 (repo (org-entry-get nil '"repository" t))
	 (issue-id (org-entry-get nil '"issue-id"))
	 (title (org-entry-get nil '"ITEM"))
	 (assignees (org-entry-get nil '"assignee"))
	 (labels (org-entry-get nil '"label"))
	 (state (if (equal (org-entry-get nil '"TODO") "TODO") 'open 'completed ))
	 )
    (if (null issue-id)
	(forge-org-create-bug title '"" labels assignees state forge owner repo)
      (message '"This issue already has an issue id, so we will not add it to the forge")
      )
    )
  )

(defun forge-org-edit-topic-state-by-id (topic-id)
  "Change the state of a topic by TOPIC-ID."
  (let* ((topic (forge-get-topic topic-id))
	 (repo (forge-get-repository topic))
	 )
    (progn

      (forge--ghub-patch topic
	"/repos/:owner/:repo/issues/:number"
	`((state . ,(cl-ecase (oref topic state)
		      (completed "OPEN")
		      (open   "CLOSED"))))
	)
      (let*
	  ((query (
		   format
		   '"UPDATE issue SET state = '%s' where id = '\"%s\"'"
		   (cl-ecase (oref topic state)
		      (completed 'open)
		      (open   'completed))
		   topic-id
		   )))
	(forge-sql query)
	)
      )
    ))

(defun forge-org-edit-topic-state ()
  "Close or open a topic at point."
  (interactive)
  (let* ((issue-id (org-entry-get nil '"ISSUE-ID")))
    (forge-org-edit-topic-state-by-id issue-id)
    ))

(defun forge-org-reset-database ()
  "Reset forge-org's additions to a forge sqlite database."
  (interactive)
  (progn
    (forge-destroy-scheduling-table)
    (forge-create-scheduling-table)
    )
  )

(defun forge-org-reset-org ()
  "Delete the org file forge-org is using."
  (interactive)
  (delete-file forge-org-file-name)
  )

(forge-create-scheduling-table)

(provide 'forge-org)
;;; Tests

(defvar run-my-tests nil)
(eval-when-compile (setq run-my-tests t))

;(when run-my-tests
;  (assert (= 1 1))
;  )

;;; forge-org.el ends here
