CREATE TABLE users(              
    user_id SERIAL PRIMARY KEY,             
    name_user VARCHAR(30),                 
    login VARCHAR(30),            
    pass TEXT,           
    creation_date DATE,
    is_admin  BOOLEAN,
    is_author BOOLEAN
); 


CREATE TABLE category(               
    category_id SERIAL PRIMARY KEY,              
    name_category VARCHAR(100), 
    parent_category VARCHAR(100)
);


CREATE TABLE news(               
    news_id SERIAL PRIMARY KEY,
    title TEXT,
    creation_date DATE,               
    user_id INT NOT NULL,
    category_id INT NOT NULL,
    content TEXT,    
    photo INT[],
    is_published  BOOLEAN,
    FOREIGN KEY (user_id)   REFERENCES users   (user_id),
    FOREIGN KEY (category_id) REFERENCES category (category_id)
);


CREATE TABLE photo(               
    photo_id SERIAL PRIMARY KEY,              
    image TEXT
);


