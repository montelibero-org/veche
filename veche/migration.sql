CREATE TABLE "request" (
    "id" INTEGER PRIMARY KEY,
    "user" INTEGER NOT NULL
        REFERENCES "user"
        ON DELETE RESTRICT
        ON UPDATE RESTRICT,
    "comment" INTEGER NOT NULL
        REFERENCES "comment"
        ON DELETE RESTRICT
        ON UPDATE RESTRICT
);
