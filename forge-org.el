;;; forge-org.el --- Create org file for integrating scheduling with issues managed with forge.

;; Author: jkamins7 <jkaminsky at jhu dot edu>
;; Maintainer: jkamins7 <jkaminsky at jhu dot edu>
;; Created: 2021-03-24
;; Keywords: org, synchronization, issue tracking, forge
;; Homepage: https://github.com/jkamins7/todolist
;; Package-Requires: ((cl-lib "0.5") (org "8.2") (emacs "24") (forge "0.1"))
;; Version: 0.3.0
;; This file is not part of gnu emacs

;;; package --- Summary
;; forge-org is a package for using org-mode as to integrate forge tasks with an org mode scheduling interface
;;; Commentary:
;; The database and the org mode file both contain the same information.  If either is deleted, it should be obtainable from the other.
;; If both are deleted, you'll need to rebuild the org file by hand (though forge can rebuild the database).

;;; Code:

(defvar org-file-name)
(setq org-file-name '"~/Dropbox/forge.org")

;; This seems like poor emacs style
(defvar  current-repository '"")
(defvar current-milestone '"")
(defvar current-milestone-name '"")

(defun write-property (name value indentation)
  "Write an \"org-mode\" property for a list item at indent level \"INDENTATION\" called \"NAME\" with value \"VALUE\"."
  (if value
      (progn
	(insert indentation)
	(insert (concat '":" name '": "))
	(if (stringp value)
	    (insert value)
	  (if (numberp value)
	      (insert (number-to-string value))
	    (if (and (listp value) (length value))
		(insert (string-join value '" "))
	      nil)))
	(newline 1)
	)
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
  ;;(forge-sql '"SELECT * FROM (SELECT repository.id as rid,issue.milestone as imi,issue.id as iid,issue.state as ist,issue.title as iti,issue.labels as ila,repository.forge as rfo,repository.owner as row,repository.name as rna FROM issue LEFT JOIN repository ON issue.repository = repository.id) ORDER BY rid,imi")
  (forge-sql
   (concat
    '"SELECT * FROM (SELECT
       repository.id as rid,
       repository.forge as rfo,
       repository.owner as row,
       repository.name as rna,
       issue.milestone as imi,
       issue.id as iid,
       issue.state as ist,
       issue.title as iti,
       assignee.id as aid,
       assignee.login as alo,
       assignee.name as ana,
       issue_schedule.id as sid,
       issue_schedule.scheduled as ssc,
       issue_schedule.due as sdu,
       issue_schedule.clock as scl,
       issue_schedule.priority as spr
     FROM
       (((issue LEFT JOIN repository ON issue.repository == repository.id)
         LEFT JOIN
        (issue_assignee LEFT JOIN assignee ON issue_assignee.id == assignee.id)
         ON
        issue_assignee.issue == issue.id
       ) LEFT JOIN
         issue_schedule
         ON
         issue.id == issue_schedule.issue)"
    (if filters
	(concat
	 '"WHERE ("
	 (if (listp filters)
	     (string-join filters '" AND ")
	   filters)
	 '")"
	 )
      nil)
    ") ORDER BY rid, imi, iid, sid"
    )
   )
  )

;;;(forge-sql '"SELECT repository.id,repository.forge,repository.owner,repository.name,issue.milestone,issue.id,issue.state,issue.title,assignee.id,assignee.name FROM ((issue LEFT JOIN repository ON issue.repository == repository.id) LEFT JOIN (issue_assignee LEFT JOIN assignee ON issue_assignee.id == assignee.id) ON issue_assignee.issue == issue.id)")

(defun issue-to-org (sql-result)
  "Convert an issue in the form of an sql result into org mode text.  \"SQL-RESULT\" is the sql result to convert."
  (progn
    (if (or (not (nth 0 sql-result)) (string= current-repository (nth 0 sql-result)))
	nil
      (progn
        (find-file org-file-name)
        (insert '"* ")
        (insert (nth 3 sql-result))
        (newline 1)
        (insert '"  :PROPERTIES:")
        (newline 1)
	(write-property '"repository-id" (nth 0 sql-result) '"  ")
	(write-property '"repository" (nth 3 sql-result) '"  ")
	(write-property '"owner" (nth 2 sql-result) '"  ")
	(write-property '"forge" (nth 1 sql-result) '"  ")
        (insert '"  :END:")
        (newline 1)
        (setq current-repository (nth 0 sql-result))
	(setq current-milestone '"")
        't
	)
      )

    (if (nth 4 sql-result) (setq current-milestone-name (nth 4 sql-result)) (setq current-milestone-name '"No Milestone"))
    (if (not (string= current-milestone current-milestone-name))
	(progn
	  (find-file org-file-name)
	  (insert '"** ")
	  (insert current-milestone-name)
	  (newline 1)
	  (insert '"   :PROPERTIES:")
	  (newline 1)
	  (insert '"   :END:")
	  (newline 1)
	  (setq current-milestone current-milestone-name)
	  't
	  )
      )

    (progn
      (find-file org-file-name)
      (insert '"*** ")
      (if (string= (nth 6 sql-result) '"open")
          (insert '"TODO ")
	(insert '"DONE ")
	)
      (if (not (string= (nth 15 sql-result) '""))
          (insert (concat '"[#" (nth 15 sql-result) "] "))
	)
      (insert (nth 7 sql-result))
      (newline 1)
      (if (and (nth 12 sql-result) (not (string= (nth 12 sql-result) '"")))
	  (progn
	    (insert "    SCHEDULED:")
	    (insert '" <")
	    (insert (nth 12 sql-result))
	    (insert '"> ")
	    )
	nil
	)
      (if (and (nth 13 sql-result) (not (string= (nth 13 sql-result) '"")))
	  (progn
	    (insert "    DEADLINE:")
	    (insert '" <")
	    (insert (nth 13 sql-result))
	    (insert '"> ")
	    )
	nil
	)
      (if (or
	   (and (nth 12 sql-result) (not (string= (nth 12 sql-result) '"")))
	   (and (nth 13 sql-result) (not (string= (nth 13 sql-result) '""))))
	  (newline 1))
      (if (and (nth 14 sql-result) (not (string= (nth 14 sql-result) '"")))
	  (progn
	    (insert (nth 14 sql-result))
	    (newline 1)
	    )
	nil
	)
      (insert '"    :PROPERTIES:")
      (newline 1)
      (write-property '"issue-id" (nth 5 sql-result) '"    ")
      (write-property '"assignee" (nth 9 sql-result) '"    ")
      (write-property '"assignee-name" (nth 10 sql-result) '"    ")
      (write-property '"assignee-id" (nth 8 sql-result) '"    ")
      (write-property '"schedule-id" (nth 11 sql-result) '"    ")
      (insert '"    :END:")
      (newline 1)
      )
    )
  )

(defun sql-to-org (sql-results)
  "Convert an entire sql query into org mode text.  \"SQL-RESULTS\" is the sql result to convert."
  (progn
    (setq sql-results sql-results)
    (setq current-repository nil)
    (setq current-milestone nil)
    (find-file org-file-name)
    (erase-buffer)
    (insert '"# Issue tracking for use with forge")
    (newline 1)
    (insert '"#+BEGIN: clocktable :maxlevel 4 :scope file")
    (newline 1)
    (insert '"#+END:")
    (newline 1)
    (insert '"#+PRIORITIES: A E C")
    (newline 1)
    (mapcar 'issue-to-org sql-results)
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
  (let* ((rc '""))
    (progn
      (goto-char issue-min)
      (setq rc '"")
      (while (re-search-forward regex issue-max t);Get it's id
	(setq rc (concat rc (if (not (string= rc '"")) '"\n" '"") (match-string 1)))
	nil)
      rc))
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
      (let*
	  ((this-id (search-within-issue issue-min issue-max '":issue-id: \\(.*\\)$"))
	   (this-schedule-id (search-within-issue issue-min issue-max '":schedule-id: \\(.*\\)$" ));Get it's iD
	   (this-schedule (search-within-issue issue-min issue-max '"SCHEDULED: <\\([^>]*\\)>" ));Get it's id
	   (this-deadline (search-within-issue issue-min issue-max '"DEADLINE: <\\([^>]*\\)>" ));Get it's id
	   (this-clock (search-within-issue issue-min issue-max '"^\\( *CLOCK: .*\\)$" ));Get it's id
	   (this-priority (search-within-issue issue-min issue-max '"^[*]* [TOD][OPO][DEN][ONE] \\[#\\(.*\\)\\]" ));Get it's priority
	   (this-issue (list this-id this-schedule-id this-schedule this-deadline this-clock this-priority))); Construct issue
	(setq issues-list (cons this-issue issues-list))
	(goto-char issue-min)
	))
    (save-buffer)
    (kill-buffer)
    issues-list))
  )

(defun diff-issue-list-with-database (filename)
  (progn
    (mapcar 'diff-and-update-issue (org-to-issue-list filename))
    (sql-to-org (issue-forge-query (list
				    '"assignee.login == '\"jkamins7\"'"
				    "(NOT (issue.state == 'closed'))"
				    )))
    )
  )

(defun diff-and-update-issue (issue)
  (progn
    (if (and (nth 1 issue) (not (string= (nth 1 issue) '"")));Do we have a unique scheduling id
	;; We do
	;(forge-sql (concat '"UPDATE issue_schedule SET due = '\"" (nth 2 issue) '"\"', scheduled = '\"" (nth 3 issue) '"\"' WHERE id == " (nth 1 issue)))
	(progn
	  (setq query (
		       concat
		       '"UPDATE issue_schedule SET due = '\"" (nth 3 issue)
		       '"\"', scheduled = '\"" (nth 2 issue)
		       '"\"' , clock= '\"" (nth 4 issue)
		       '"\"' , priority= '\"" (nth 5 issue)
		       '"\"' WHERE id == " (nth 1 issue)))
	  (forge-sql query)
	  )
      ;; We do not
      (setq query (concat '"INSERT INTO issue_schedule(issue,scheduled,due,clock,priority) VALUES('\"" (string-join (cons (car issue) (cdr (cdr issue))) "\"', '\"") "\"')"))
      (forge-sql query)
      ;; (forge-sql (concat '"INSERT INTO issue_schedule (issue scheduled due) VALUES '\"" (string-join (cons (car issue) (cdr (cdr issue))) "\"', '\"") "\"'"))
      )
    (setq tmp issue)
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

;(defvar tmp3)
;(setq tmp3 (test-sql-query (list '"repository.forge == '\"github.com\"'")))
;(setq current-repository '"")
;(setq current-milestone '"")
;(defvar tmp)
(defun forge-org-update
    ()
  (interactive)
  (progn
    (forge-org-pull-all-forges)
    (diff-issue-list-with-database org-file-name)
    (save-buffer)))

(defun forge-org-jump-to-forge () (interactive)
       (progn
	 (setq tmp (concat '"^" (org-entry-get nil '"repository" t) '" "))
	 (magit-list-repositories)
	 (switch-to-buffer (other-buffer (current-buffer) t))
	 (goto-char (point-min))
	 (re-search-forward tmp (point-max) t)    ;Find next issue
	 (magit-repolist-status)
	 (switch-to-buffer (other-buffer (current-buffer) t))
	 (kill-buffer)
	 )
       )

(defun forge-org-get-all-repositories-in-database ()
  (mapcar (lambda (repo) (forge-get-repository repo)) (forge-sql "select distinct forge,owner,name from repository")))

(defun forge-org-pull-all-forges ()
  (interactive)
  (dolist (repo (forge-org-get-all-repositories-in-database))
    (forge-pull repo)
    (sit-for 1)
    ))

(defun forge-org-create-bug (.title .body .labels .assignees .state url)
  "Create an issue for a forge named URL from a .TITLE .BODY .LABELS .ASSIGNEES and .STATE ."
  (forge--ghub-post (forge-get-repository (forge-org-parse-url url)) "/repos/:owner/:repo/issues"
    `((title . , .title)
      (body  . , .body)
      ,@(and .labels    (list (cons 'labels    .labels)))
      ,@(and .assignees    (list (cons 'assignees    .assignees)))
      )
    ;; :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)
    )
  )

(defun forge-org-edit-topic-state-by-id (topic-id)
  "Change the state of a topic by TOPIC-ID."
  (let* ((topic (forge-get-topic topic-id))
	 (repo (forge-get-repository topic))
	 )
    (debug)
    (forge--set-topic-state repo topic)
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
  (delete-file org-file-name)
  )

(forge-create-scheduling-table)

(provide 'forge-org)
;;; forge-org.el ends here
