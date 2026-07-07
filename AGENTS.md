# Ice Cream Calculator

this small webapp is a local-first calculator to determine proportions of ingredients to make ice cream.
There are 3 proportions to balance. A good ice cream must have:
- 55-64% water content
- 15-20% sugar content
- 10-16% fat content

Each of those is brought in in various quantities by the ingredients the user will select.
The idea is that the user can select his / her ingredients, a target amount of ice cream,
and the application will calculate the amount of the various ingredients. Since there are
multiple possible combinations, the app should let the user determine a fixed amount for
one or more ingredients. The app will then adjust the amount of all the other ingredients
to reach the target proportions. If no solution exists, then the app will also highlight
that and highlight which one of the three fundametal proportions is being broken.

The application has a list of predetermined ingredients that it offers the user to select
from. Every ingredient brings a certain proportion of fat, water and sugar.

The list of ingredients should be maintained in a separate file by the agent, making it
clear to read and edit for a human.


## Tech

The app is written in elm, with 0 external packages.
Maintain a single app.css file.


## UI

The style should be minimalist and light. Use little color, only for accents.
Use wide margins to make the overall look less crowded.
