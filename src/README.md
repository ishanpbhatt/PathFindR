# CS-3110-Final-Project

Members:
Ishan Bhatt (ib263), Christopher Mohri (ccm244), Vaughn Campos (vac62), Glenn Baevsky
(ghb65)

Core Vision: As Instacart drivers, we intend to solve the issue of efficient delivery, by
creating a system to determine the shortest path for finding items in a grocery store. This
will mean creating data structures to hold the grocery store data, implementing the
traveling salesperson algorithm, and visualize the layout of each store and the location of
each item, then visualise the shortest path from item to item. We want to provide grocery
store users the ability to compare their options for a given order on different metrics from
price to convenience.

Features:
  - The system will be able to choose between different stores
  - Choose different items specific to each store and generate a “grocery list”
  - We will then visualize and generate an optimal path in a text-based way
  - The system will also be able to create and add in new stores

Roadmap:

MS1
- Satisfactory
  - Set up collaborative environment and create a private git repository
  - Create skeleton system and add signatures in .mli files
- Good
  - Design data structure for storing grocery store data
  - Hardcode data for stores in determined data structure
  - Create black box tests for functions defined in .mli file
- Excellent
  - Pick and implement the algorithm to find the shortest path

MS2
- Satisfactory
  - Optimize the shortest path algorithm and have it account for barriers in the store
- Good
  - Create user interface to allow store and item selection
  - This interface will display available stores, and once a store is selected, display the available items
- Excellent
  - Visualize the store and the path in a text based way in terminal

MS3
- Satisfactory
  - Create a feature to display metrics such as price paid, distance travelled, etc.
  - Parse user input and create search from list of items
- Good
  - Create schema to add and create new grocery stores to run the path finding algorithm on
  - Allow user to input what items they want, and then the system suggests
what store to visit based on availability, price, and distance through store
- Excellent
  - Integrate the Wegmans API so that Wegmans store have special features
  - Make recommendations for similar products if a product is not available instore
