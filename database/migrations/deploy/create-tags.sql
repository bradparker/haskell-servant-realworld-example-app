-- Deploy conduit:create-tags to pg

BEGIN;

CREATE TABLE IF NOT EXISTS tags (
  id SERIAL PRIMARY KEY,
  name TEXT UNIQUE NOT NULL
);

COMMIT;
