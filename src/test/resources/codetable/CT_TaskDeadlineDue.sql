--
-- Generated by IBM Curam Generator.
--
-- Generator Copyright IBM Corporation 1999, 2012. All Rights Reserved.
--
-- US Government User Restricted Rights - Use, duplication restricted by GSA ADP Schedule Contract with IBM Corp.
--
--
-- CODETABLE TaskDeadlineDue
--
INSERT INTO CodeTableHeader (TableName, TimeEntered, DefaultCode, LASTWRITTEN, VERSIONNO) VALUES ('TaskDeadlineDue', CURRENT_TIMESTAMP(''), '', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CTDISPLAYNAME (TableName, LOCALEIdentifier,TEXT,LASTWRITTEN ) VALUES ( 'TaskDeadlineDue','en','Task Deadline Due', CURRENT_TIMESTAMP(''));
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('TaskDeadlineDue', 'TDD1', 'Overdue', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('TaskDeadlineDue', 'TDD2', 'Due Today', '', '', '1', 1, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('TaskDeadlineDue', 'TDD3', 'Due This Week', '', '', '1', 2, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('TaskDeadlineDue', 'TDD4', 'Due This Month', '', '', '1', 3, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('TaskDeadlineDue', 'TDD5', 'Due After This Month', '', '', '1', 4, 'en', CURRENT_TIMESTAMP(''), 1);

