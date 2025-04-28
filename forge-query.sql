SELECT
  repository.id as rid
  , repository.forge as rfo
  , repository.owner as row
  , repository.name as rna
  , issue.milestone as imi
  , issue.id as iid
  , issue.state as ist
  , issue.title as iti
  , assignee.id as aid
  , assignee.login as alo
  , assignee.name as ana
  , issue_schedule.id as sid
  , issue_schedule.scheduled as ssc
  , issue_schedule.due as sdu
  , issue_schedule.clock as scl
  , issue_schedule.priority as spr
FROM
  issue
  LEFT JOIN repository ON issue.repository == repository.id
  LEFT JOIN issue_assignee ON issue.id == issue_assignee.issue
  LEFT JOIN assignee ON issue_assignee.id == assignee.id
  LEFT JOIN issue_schedule ON issue.id = issue_schedule.issue
WHERE
  %s
ORDER BY rid, imi, iid, sid
