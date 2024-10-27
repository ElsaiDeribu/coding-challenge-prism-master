-- Create your database tables here. Alternatively you may use an ORM
-- or whatever approach you prefer to initialize your database.
CREATE TABLE example_table (id SERIAL PRIMARY KEY, some_int INT, some_text TEXT);
INSERT INTO example_table (some_int, some_text) VALUES (123, 'hello');

CREATE TABLE elements (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    type VARCHAR(50) NOT NULL
);
CREATE TABLE element_styles (
    id SERIAL PRIMARY KEY,
    element_id INTEGER NOT NULL,
    margin_top VARCHAR(50),
    margin_right VARCHAR(50),
    margin_bottom VARCHAR(50),
    margin_left VARCHAR(50),
    padding_top VARCHAR(50),
    padding_right VARCHAR(50),
    padding_bottom VARCHAR(50),
    padding_left VARCHAR(50),
    FOREIGN KEY (element_id) REFERENCES elements(id)
);

-- Insert a sample element
INSERT INTO elements (name, type) VALUES ('Sample Element', 'div');

-- Insert a sample element_style
INSERT INTO element_styles (
    element_id,
    margin_top,
    margin_right,
    margin_bottom,
    margin_left,
    padding_top,
    padding_right,
    padding_bottom,
    padding_left
) VALUES (
    1,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
);
