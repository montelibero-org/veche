DROP TABLE event;

ALTER TABLE comment ADD COLUMN event_delivered BOOLEAN NOT NULL DEFAULT FALSE;
UPDATE comment SET event_delivered = TRUE WHERE event_delivered = FALSE;

ALTER TABLE issue ADD COLUMN event_delivered BOOLEAN NOT NULL DEFAULT FALSE;
UPDATE issue SET event_delivered = TRUE WHERE event_delivered = FALSE;

ALTER TABLE request ADD COLUMN event_delivered BOOLEAN NOT NULL DEFAULT FALSE;
UPDATE request SET event_delivered = TRUE WHERE event_delivered = FALSE;
ALTER TABLE request ADD COLUMN created TIMESTAMP NOT NULL DEFAULT 0;
UPDATE request SET created = CURRENT_TIMESTAMP WHERE created = 0;
