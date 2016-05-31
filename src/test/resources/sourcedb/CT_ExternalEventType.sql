--
-- Generated by IBM Curam Generator.
--
-- Generator Copyright IBM Corporation 1999, 2012. All Rights Reserved.
--
-- US Government User Restricted Rights - Use, duplication restricted by GSA ADP Schedule Contract with IBM Corp.
--
--
-- CODETABLE ExternalEventType
--
INSERT INTO CodeTableHeader (TableName, TimeEntered, DefaultCode, LASTWRITTEN, VERSIONNO) VALUES ('ExternalEventType', CURRENT_TIMESTAMP(''), '', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CTDISPLAYNAME (TableName, LOCALEIdentifier,TEXT,LASTWRITTEN ) VALUES ( 'ExternalEventType','en','External Event Type', CURRENT_TIMESTAMP(''));
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('ExternalEventType', 'ea.ipa.pc', 'Event type issued to the EventAdaptor after the creation of an Intake Program Application', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('ExternalEventType', 'ea.ipa.pa', 'Event Type issued to the EventAdaptor after the approval of an Intake Program Application', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('ExternalEventType', 'ea.ipa.pd', 'Event Type issued to the EventAdaptor after the denial of an Intake Program Application', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);
INSERT INTO CodeTableItem (TABLENAME, CODE, DESCRIPTION, ANNOTATION, COMMENTS, ISENABLED, SORTORDER, LOCALEIDENTIFIER, LASTWRITTEN, VERSIONNO) VALUES ('ExternalEventType', 'ea.ipa.pw', 'Event Type issued to the EventAdaptor after the withdrawal of an Intake Program Application', '', '', '1', 0, 'en', CURRENT_TIMESTAMP(''), 1);
