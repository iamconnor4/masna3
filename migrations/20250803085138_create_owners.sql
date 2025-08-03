CREATE TABLE owners (
    owner_id uuid PRIMARY KEY
  , owner_name text NOT NULL
  , created_at timestamptz NOT NULL
  , updated_at timestamptz 
);

CREATE INDEX owner_name_idx ON owners (owner_name);
