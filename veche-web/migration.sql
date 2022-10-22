CREATE TABLE escrow
    ( id            INTEGER PRIMARY KEY
    , amount        VARCHAR NOT NULL
    , asset         VARCHAR NOT NULL
    , issue         INTEGER NOT NULL
                    REFERENCES issue ON DELETE RESTRICT ON UPDATE RESTRICT
    , sponsor       VARCHAR NOT NULL
    , sponsor_tx    VARCHAR NOT NULL
    , paid_tx       VARCHAR NULL
    );
