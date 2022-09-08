CREATE TABLE "notification" (
    "id" INTEGER PRIMARY KEY,
    "recipient" INTEGER NOT NULL
        REFERENCES "user" ON DELETE RESTRICT ON UPDATE RESTRICT,
    "text" VARCHAR NOT NULL
);

CREATE TABLE "telegram" (
    "id" INTEGER PRIMARY KEY,
    "chatid" INTEGER NOT NULL,
    "username" VARCHAR NOT NULL
);
