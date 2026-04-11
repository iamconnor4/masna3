CREATE TABLE processes (
    process_id uuid PRIMARY KEY
  , owner_id uuid REFERENCES owners (owner_id) NOT NULL
  , created_at timestamptz NOT NULL
  , updated_at timestamptz
);