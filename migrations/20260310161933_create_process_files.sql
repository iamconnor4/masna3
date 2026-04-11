CREATE TABLE process_files (
    process_file_id uuid PRIMARY KEY
  , process_id uuid NOT NULL REFERENCES processes (process_id) ON DELETE CASCADE
  , file_id uuid NOT NULL REFERENCES files (file_id) ON DELETE CASCADE
  , created_at timestamptz NOT NULL
  , updated_at timestamptz
);