ALTER TABLE issue ADD COLUMN poll VARCHAR NULL;
UPDATE issue SET poll = "BySignerWeight" WHERE poll IS NULL;
