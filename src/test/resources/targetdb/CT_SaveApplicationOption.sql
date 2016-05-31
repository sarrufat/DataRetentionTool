--
-- Generated by IBM Curam Generator.
--
-- Generator Copyright IBM Corporation 1999, 2012. All Rights Reserved.
--
-- US Government User Restricted Rights - Use, duplication restricted by GSA ADP Schedule Contract with IBM Corp.
--
--
-- CODETABLE SaveApplicationOption
--
INSERT INTO CodeTableHeader (TableName, TimeEntered, DefaultCode, LASTWRITTEN, VERSIONNO) VALUES ('SaveApplicationOption', CURRENT_TIMESTAMP(''), 'SAO19001', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CTDISPLAYNAME (TableName, LOCALEIdentifier,TEXT,LASTWRITTEN ) VALUES ( 'SaveApplicationOption','en','Save Application Option', CURRENT_TIMESTAMP(''));
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('SaveApplicationOption', 'SAO19001', 'Save the application and come back to work on it later.', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('SaveApplicationOption', 'SAO19002', 'Quit without saving the application. You will not be able to come back to the application.', '', '', '1', 1, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('SaveApplicationOption', 'SAO19003', 'Submit application.', '', '', '1', 2, 'en', CURRENT_TIMESTAMP(''), 1);
