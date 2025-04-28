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

(defcustom forge-org-preamble
  (string-join '(
		"# Issue tracking for use with forge"
		"#+BEGIN: clocktable :maxlevel 4 :scope file"
		"#+END:"
		"#+PRIORITIES: A E C"
		) '"\n"
		  )
  "The preamble to put at the top of the forge org output file."
  :package-version '(forge . "0.3.0")
  :group 'forge-org
  :type 'string)

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
		 :schedule-id     (if (nth 11 query-row) (format '"%d" (nth 11 query-row)) (nth 11 query-row))
		 :scheduled       (nth 12 query-row)
		 :due             (nth 13 query-row)
		 :logbook         (nth 14 query-row)
		 :priority        (nth 15 query-row)
		 )
  )

(defun issue-forge-query (filters)
  "Query the forge database for issues matching \"FILTERS\". Return is a forge-org-issue"
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
    

(defun issue-to-org (issue)
  "Convert an issue in the form of an sql result into org mode text.  \"ISSUE\" is the sql result to convert."
  (progn
    (forge-org-get-repository-org issue)
    (forge-org-get-milestone-org issue)
    (forge-org-get-issue-org issue)
    )
  )

(defun issue-list-to-org (issue-list)
  "Convert an entire issue list query into org mode text.  \"ISSUE-LIST\" is the issue list result to convert."
  (progn
    (setq current-repository nil)
    (setq current-milestone nil)
    (find-file forge-org-file-name)
    (erase-buffer)
    (insert forge-org-preamble)
    (newline 1)
    (mapc 'issue-to-org issue-list)
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
	 (issue-max (point-max)))
    (progn
      (find-file filename)
      (goto-char (point-max))
      (while (re-search-backward '"^[*][*][*] " (point-min) t)    ;Find next issue
	(progn
	  ;; Set bounds for issue
	  (setq issue-max issue-min)
	  (setq issue-min (point))
	  (setq issues-list (cons (forge-org-org-to-issue filename issue-min issue-max) issues-list))
	  (print issues-list)
	  (goto-char issue-min)
	  ))
      (save-buffer)
      (kill-buffer)
      issues-list))
  )

(defun diff-issue-list-with-database (filename)
  (progn
    (mapc 'diff-and-update-issue (org-to-issue-list filename))
    (issue-list-to-org (issue-forge-query forge-org-issue-filters))
    )
  )

(defun get-default-todo ()
  (car (cdr (car org-todo-keywords)))
  )

(defun diff-and-update-issue (issue)
  (if (not (oref issue :id))
      ;; If no issue exists, create one.
      (message (format '"Issue creation is not yet integrated. Please create the issue %s for this entry manually." (prin1-to-string issue)))
    (let*
	((other-issue (nth 0 (issue-forge-query (format '"issue.id == '\"%s\"'" (oref issue :id)))))
	 (issue-diff (forge-org-diff-issues issue other-issue))
	 )
      ;; If we don't have a schedule id, create a scheduling table entry
      (if (or
	   (oref issue-diff :scheduled)
	   (oref issue-diff :due)
	   (oref issue-diff :logbook)
	   (oref issue-diff :priority))
	  (if (not (oref issue :schedule-id))
	      (let* ((query (concat
			     '"INSERT INTO issue_schedule(issue,scheduled,due,clock,priority) "
			     '"VALUES('\"" (string-join (list
							 (oref issue :id)
							 (oref issue :scheduled)
							 (oref issue :due)
							 (oref issue :logbook)
							 (oref issue :priority))
							"\"', '\"")
			     "\"')"))
		     )
		(forge-sql query)
		)
	    (let*
		((query (concat
			 '"UPDATE issue_schedule SET due = '\"" (oref issue :due)
			 '"\"', scheduled = '\"" (oref issue :scheduled)
			 '"\"' , clock= '\"" (oref issue :logbook)
			 '"\"' , priority= '\"" (oref issue :priority)
			 '"\"' WHERE id == " (oref issue :schedule-id)
			 )))
	      (forge-sql query)
	      )
	    )
	)
      (if (oref issue-diff :state)
	  (progn
	    (message (format '"Changing state for issue %s from %s to %s" (oref issue :id) (oref other-issue :state) (oref issue :state)))
	    ;;    (forge-org-edit-topic-state-by-id issue-id)
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
