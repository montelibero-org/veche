DROP TABLE escrow;

CREATE TABLE telegram_state (id INTEGER PRIMARY KEY, offset INTEGER NULL);
INSERT INTO telegram_state (id, offset) VALUES (0, NULL);
