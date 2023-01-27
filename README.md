# haskell-server

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
4. Open file "config.json" in root folder of the project and change database parameters on parameters your database 
    - "dbHost" : "localhost"              - change it if your database in another place,        
    - "dbPort"      : 5432                - replace "5432" with the port number of your database,
    - "dbname"      : "haskellserverlite" - replace "haskellserverlite" with the name of your database,
    - "dbUser"      : "haskell"           - replace "haskell" with the username of your database,
    - "dbPassword"  : "haskell"           - replace "haskell" with the password of your database.
   and save the file.
   You also may change other parameters. The values of the parameters will be given below.
5. Run the server with the 
   ```haskell
   stack build
   ```
   command from the root folder of the project.
   After running the server will check database and create necessery tables.
6. Open one more terminal to send requests to the server.
7. Create a user by sending the following request to the server
   ```  
   curl -X POST 'http://Adam:sixthDay@localhost:8080/user?name_user=NAME&login=LOGIN&pass=PASSWORD&is_admin=true&is_author=true'
   ```
   where
    - NAME - name for your user,
    - LOGIN - login for your user,
    - PASSWORD - password for your user.
8. Drink some coffee.    
