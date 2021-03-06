--
-- Generated by IBM Curam Generator.
--
-- Generator Copyright IBM Corporation 1999, 2012. All Rights Reserved.
--
-- US Government User Restricted Rights - Use, duplication restricted by GSA ADP Schedule Contract with IBM Corp.
--
--
-- CODETABLE ContactStatus
--
INSERT INTO CodeTableHeader (TableName, TimeEntered, DefaultCode, LASTWRITTEN, VERSIONNO) VALUES ('ContactStatus', CURRENT_TIMESTAMP(''), 'CS1', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CTDISPLAYNAME (TableName, LOCALEIdentifier,TEXT,LASTWRITTEN ) VALUES ( 'ContactStatus','ca','Estat del contacte', CURRENT_TIMESTAMP(''));
INSERT INTO CTDISPLAYNAME (TableName, LOCALEIdentifier,TEXT,LASTWRITTEN ) VALUES ( 'ContactStatus','en','Contact Status', CURRENT_TIMESTAMP(''));
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('ContactStatus', 'CS1', 'Actiu', '', '', '1', 0, 'ca', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('ContactStatus', 'CS1', 'Active', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('ContactStatus', 'CS2', 'Cancel·lat', '', '', '1', 0, 'ca', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('ContactStatus', 'CS2', 'Canceled', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('ContactStatus', 'CS2', 'Cancelled', '', '', '1', 0, 'en_GB', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('ContactStatus', 'CS3', 'Arxivat', '', '', '1', 0, 'ca', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('ContactStatus', 'CS3', 'Archived', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);

