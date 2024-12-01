# Project Contract

## Create a Web Service for Package Tracking

### API Design, Software Design, Unit Testing, Implementation, System Stress Testing

### Design
**Package Tracker Design pt.1**
<img width="1267" alt="Package Tracker pt1" src="https://github.com/14th-Reason/Package_Tracker/assets/114119794/9ce44416-1ba5-43b7-a6ed-7b85b71719a6">

**Package Tracker Design pt.2**
<img width="1024" alt="Package Tracker pt2" src="https://github.com/14th-Reason/Package_Tracker/assets/114119794/58cf35bc-8116-4947-ac63-e95fa41d3f3b">

**Supervisor Tree**
<img width="351" alt="Supervisor Tree" src="https://github.com/14th-Reason/Package_Tracker/assets/114119794/46d73b08-21dd-4c5e-90c7-f4d7126e5f71">

**Riak Tree**
<img width="536" alt="Riak Tree" src="https://github.com/14th-Reason/Package_Tracker/assets/114119794/941863cd-af96-43d8-a42d-64d9572ec8d2">


### Backend

1. **Package Transferred**
    - **Package Creation Endpoint**
        - Design an API endpoint and handle package creation requests, which will generate a unique package ID and store relevant package details in the database.
    - **Package Transfer Endpoint**
        - Create an endpoint to handle package transfer requests, updating the location of the package in the database.
          
    - **Pseudocode**
        ```plaintext
        IN
            {
            location_id: UUID string,
            package_id: UUID string
            }
        OUT
            200 on success 500 on error,
            URL extension /package_transferred
        ```
2. **Delivered**
    - **Package Delivery Endpoint**
        - Implement an endpoint to mark packages as delivered and update their status accordingly.
          
        - **Pseudocode**
        ```plaintext
        IN
            UUID string (this will be the package_id)
        OUT 
            200 on success 500 on error,
            URL extension /delivered
        ```
3. **Customer Requests Location**
    - **Location Request Endpoint**
        - Design an endpoint to handle location requests, retrieving the current location of the package based on its ID.
          
        - **Pseudocode**
        ```plaintext
        IN 
            UUID string (this will be the package_id)
        OUT
             {lat: float, long: float},
            200 on success 500 on error
        URL extension /location_request
        ```
4. **Location Update**
    - **Location Update Endpoint**
        - Create an endpoint to update the location of a package based on incoming latitude and longitude coordinates.
          
        - **Pseudocode**
        ```plaintext
        IN
            {
                location_id: UUID string,
                lat: float, long: float
            }
        OUT 
            200 on success 500 on error,
        URL extension /location_update
        ```

### Hints and Tips

- **Database Integration**
    - Integrate Riak to store package information, including package IDs, statuses, and locations of all packages.
- **Error Handling**
    - Implement error handling mechanisms to manage invalid requests, database errors, or other issues that may arise during package tracking.
    - Will need to document or log all the issues that occur.
    - **Error Logging / Event Handlers**
        - Supervisor
            - Manage the event and handlers
            - All errors are events
                - All non-crashing errors are events for which you would need to plan a 500 error as an event.
        - Log all state changes as a UUID and when there is a request that will initiate new state change that way we can identify where it went wrong. The UUID can be tied to the UUID so we know where it went wrong.

### Massive Hint

- This will be noted in the books -> Event Handler / Error Logger
- Have a counter for the cycles of the handlers and the managers.
- Draft a Supervision Tree

### Software

- The Erlang compiler and runtime
- Rebar3 
- Cowboy
- Riak
- Text editor of choice
- erpc (allows you to make calls between machines)
- RabbitMQ (Messaging queue -> Fake API layer for Riak)

### Hardware

- Digital Ocean - Given $200 of free usage
- Create a $6 per month DO Droplet and access it using the web console.
- Setup SSH on your DO Droplet and access it using the terminal.
- Create a user on your DO droplet. Never run as root on any machine.
- Secure your droplet
- Install Rebar3 on your droplet using apt.

### Notes

- Make sure you keep the project app in a unique GitHub repository. 
- Put all engineering designs in GitHub.
- Every *nix system has a limit on how many open files there can be on the system.
- Don't over-engineer your first design. It won't be the last. Let the bottlenecks you discover drive engineering changes that increase optimization.
