--
-- Generated by IBM Curam Generator.
--
-- Generator Copyright IBM Corporation 1999, 2012. All Rights Reserved.
--
-- US Government User Restricted Rights - Use, duplication restricted by GSA ADP Schedule Contract with IBM Corp.
--
--
-- CODETABLE AssessmentStatus
--
INSERT INTO CodeTableHeader (TableName, TimeEntered, DefaultCode, LASTWRITTEN, VERSIONNO) VALUES ('AssessmentStatus', CURRENT_TIMESTAMP(''), 'AS1', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CTDISPLAYNAME (TableName, LOCALEIdentifier,TEXT,LASTWRITTEN ) VALUES ( 'AssessmentStatus','en','Assessment Status', CURRENT_TIMESTAMP(''));
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('AssessmentStatus', 'AS1', 'Open', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('AssessmentStatus', 'AS2', 'In Progress', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('AssessmentStatus', 'AS3', 'Completed', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);

