ALTER TABLE issue ADD COLUMN poll VARCHAR NULL;
UPDATE issue SET poll = "BySignerWeight" WHERE poll IS NULL;

CREATE TABLE forum (
    id                  VARCHAR PRIMARY KEY,
    title               VARCHAR NOT NULL,
    access_issue_read   VARCHAR NOT NULL
);

INSERT INTO forum VALUES
    ("MTL-SIGNERS", "MTL signers", "Signer"    ),
    ("MTL-HOLDERS", "MTL holders", "Holder"    ),
    ("OFFTOPIC",    "Offtopic",    "Uninvolved");

ALTER TABLE issue
    ADD COLUMN forum
    VARCHAR NOT NULL
    DEFAULT "MTL-HOLDERS"
    REFERENCES forum ON DELETE RESTRICT ON UPDATE RESTRICT;
