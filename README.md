# haskell-server
------

This is a news web server with a REST API that accepts HTTP requests and gives responses in JSON format.

The project is a task from Metalamp's internship (link: https://coda.io/@metalamp/education/4-15). 

## Project deployment

You should have installed PostgreSQL and created the database before following the steps.
Also you will need the Stack.

To deploy the project, you need to perform the following steps:
1. Clone this repository.
2. Open terminal and go to the root folder of the project.
3. Compile the project with the 
   ```haskell
   stack build
   ```
   command.
4. Open file **config.json** in root folder of the project and change database parameters on parameters your database 
    - "dbHost" : "localhost"              - change it if your database in another place,        
    - "dbPort"      : 5432                - replace "5432" with the port number of your database,
    - "dbname"      : "haskellserverlite" - replace "haskellserverlite" with the name of your database,
    - "dbUser"      : "haskell"           - replace "haskell" with the username of your database,
    - "dbPassword"  : "haskell"           - replace "haskell" with the password of your database.
   and save the file.
   You also may change other parameters. The values of the parameters will be given below.
5. Run the server with the 
   ```haskell
   stack run
   ```
   command from the root folder of the project.
   After running the server will check database and create necessery tables.
6. Open one more terminal to send requests to the server.
7. Create a user by sending the following request to the server
   ```  
   curl -X POST 'http://Adam:sixthDay@localhost:SERVER_PORT/user?name_user=NAME&login=LOGIN&pass=PASSWORD&is_admin=true&is_author=true'
   ```
   where
    - SERVER_PORT - TCP port number, specified in the **config.json** file as **ServerPort**;    
    - NAME - name for your user;
    - LOGIN - login for your user;
    - PASSWORD - password for your user.
8. For security reasons, it is necessary to block the administrator rights of the user automatically created by the system. 
   You should to execute following request to do it
   ```
   curl -X PUT 'http://LOGIN:PASSWORD@localhost:SERVER_PORT/user?block_admin=Adam'
   ```  
   where
    - SERVER_PORT - TCP port number, specified in the **config.json** file as **ServerPort**;    
    - LOGIN - login of the user created by the last request;
    - PASSWORD - password of the user created by the last request.
    
That is all. Your server is ready to work!  

You can stop the server by pressing **Ctrl + C** in the first terminal

## Starting and stopping the server

To start the server, open the terminal, go to the root of the project and run the following command 
```haskell
stack run
```
To stop the server operation, press **Ctrl + C** in the terminal where the server was started.

## Settings in config.json

```haskell
{
  "serverPort"  : 8080,                 -- TCP port on which the server will run.       
  "comment_serverPort" : "TCP port number",
  "dbHost"      : "localhost",          -- database host.       
  "comment_db"  : "db is Data Base",             
  "dbPort"      : 5432,                 -- database port.
  "dbname"      : "haskellserverlite",  -- name of the database.
  "dbUser"      : "haskell",            -- user name to log in to the database.
  "dbPassword"  : "haskell",            -- password to log in to the database
  "maxElem"     : 20,                   -- limiting the maximum number of rows in the server response.
  "comment_maxElem" : "The maximum number of elements in the server response", 
  "priorityLevel"   : "DEBUG",          -- logging level.
  "comment_priorityLevel" : "DEBUG || INFO || WARNING || ERROR",    -- possible logging levels.
  "logOutput"       : "cons",           -- the place where the logs will be output.
  "comment_logOutput" : "file || cons"  -- possible log output locations (file or console).
}
```  

## Basic project structure

```haskell
haskell-server                       -- the root folder of the project.
  ├── app                            
  │   └── Main.hs
  ├── config.json                    -- server settings file.
  ├── log.log                        -- file for server logs.
  ├── _scripts                       -- folder with request file folders for endpoints.
  ├── sql
  │   └── db.sql                     -- sql queries for creating a database.
  ├── src
  │   ├── Auth.hs                    -- lib for user authorization.
  │   ├── Category.hs                -- lib for working with categories.
  │   ├── Config.hs                  -- lib for working with Config.json, database and logger.
  │   ├── Lib.hs                     -- common functions used in the project.
  │   ├── MigrationsDB.hs            -- lib for creating and verifying a database.
  │   ├── News.hs                    -- lib for working with news.
  │   ├── Photo.hs                   -- lib for working with images.
  │   └── User.hs                    -- lib for working with users.
  └── test                           -- folder with tests for server.
```

## Request

If necessary, the following requests will be created by a user with the 
login **Sam** and password **pass123**, who has administrator and author rights. 

The server port **8080** will be used.

It is not an error to specify a username and password in the request when authorization is not required.

Limit and offset.
Using the **limit** and **offset** parameters, you can limit the number of elements output, 
as well as specify how many elements should be skipped in the output.
If **limit** is greater than **maxElem** in **config.json**, it will be ignored.
 
Limit and offset can be used together and separately.

Limit and offset can be used in all getting requests.


### Requests for working with news

#### Getting news
Without authorization, unpublished news created by the author of the request will not be included in the output.

Unpublished news from other users cannot be obtained.

The maximum number of news output is limited by the **maxElem** parameter in the **config.json file**

Getting all the published news.
Authorization is not required.    
```
curl -X GET 'http://localhost:8080/news'
```

Getting all published news and unpublished ones created by the author of the request.
Authorization is required.
```
curl -X GET 'http://Sam:pass123@localhost:8080/news'
```

Getting sorted news.
To get sorted news, you need to add **sort_by=** with parameter  to the previous request. 
News can be sorted by the following parameters
- **date**     - date of creation;
- **author**   - author (alphabetical name);
- **category** - category (alphabetically named);
- **photos**   - number of photos.

The parameter can be only one.
Example of a request to receive news sorted by creation date
```
curl -X GET 'http://Sam:pass123@localhost:8080/news?sort_by=date'
```

Filtering news.
To filter the news, you can use the following parameters:
- **created_until** - created before the date, 
- **created_since** - created from the date, 
- **created_at**    - created on the specified day;
- **author**   - author user name;
- **category** - ID category;
- **title**    - title (occurrence of substring);
- **content**  - content (occurrence of substring).

The request may include several parameters for filtering and a parameter for sorting.

Example of a request to receive news created by the user Ann before 02.24.2022, 
with the word "exhibition" in the title and sorted by the number of photos:
```
curl -X GET 'http://localhost:8080/news?author=Ann&created_until=2022-02-24&title=exhibition&sort_by=photos'
```

Limit and offset.
Example of the last request with limit and offset:
```
curl -X GET 'http://localhost:8080/news?author=Ann&created_until=2022-02-24&title=exhibition&sort_by=photos&limit=10&offset=3'
```

#### Creating news
To create news, the user must have the author's right.
A request to create a news item must have the following parameters
- **title**        - title of the news;
- **category_id**  - ID of the category to which the news belongs;
- **content**      - news text; 
- **photo**        - images for news encoded in base64, there may be several images;
- **is_published** - submit the news to public access, possible values are **true** or **false**.

Example of creating news with title "Hi Everyone!", category id 3, content "The First News That I Created.",
two images (not real, just for example),  not  publish (only  the author be able to see this news):
```
curl -X POST 'http://Sam:pass123@localhost:8080/news?title=Hi%20Everyone%21&category_id=3&content=The%20First%20News%20That%20I%20Created%2E&photo=data%3Aimage%2Fpng%3Bbase64%2CaaaHGkjHGKHJgghK&photo=data%3Aimage%2Fpng%3Bbase64%2CbbbHGkjHGKHJgghK&is_published=false'
```

#### News Editing

