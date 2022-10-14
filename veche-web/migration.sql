CREATE TABLE "user_role"
    ( "id" INTEGER PRIMARY KEY
    , "role" VARCHAR NOT NULL
    , "user" INTEGER NOT NULL
        REFERENCES "user" ON DELETE RESTRICT ON UPDATE RESTRICT
    , CONSTRAINT "unique_role" UNIQUE ("role", "user")
    );
INSERT INTO "user_role" (role, user) VALUES ("Admin", 1);
