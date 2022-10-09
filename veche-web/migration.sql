ALTER TABLE issue ADD COLUMN poll VARCHAR NULL;
UPDATE issue SET poll = "BySignerWeight" WHERE poll IS NULL;

CREATE TABLE forum
    ( id                    VARCHAR PRIMARY KEY
    , title                 VARCHAR NOT NULL
    , access_issue_read     VARCHAR NOT NULL
    , access_issue_write    VARCHAR NOT NULL
    , access_issue_comment  VARCHAR NOT NULL
    );

INSERT INTO forum VALUES
    ("MTL-SIGNERS", "MTL signers", "Signer"    , "Signer"    , "Signer"    ),
    ("MTL-HOLDERS", "MTL holders", "Holder"    , "Holder"    , "Holder"    ),
    ("OFFTOPIC",    "Offtopic",    "Uninvolved", "Uninvolved", "Uninvolved");

ALTER TABLE issue
    ADD COLUMN forum
    VARCHAR NOT NULL
    DEFAULT "MTL-HOLDERS"
    REFERENCES forum ON DELETE RESTRICT ON UPDATE RESTRICT;
