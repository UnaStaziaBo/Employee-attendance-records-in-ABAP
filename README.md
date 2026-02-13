# Employee-attendance-records-in-ABAP
Employee Attendance Management (ZDT_ATTENDANCE_MGMT) is an ABAP dialog application for daily employee attendance tracking. It supports CRUD operations over table ZDT_ATTENDANCE, displays a monthly overview in an ALV Grid, and generates a monthly PDF report using SmartForms.

    • CRUD operations on its own database table,
    • user input validation,
    • structured business logic,
    • ALV-based reporting,
    • formatted output using SmartForms.
    • The solution complies with SAP development conventions
    
## Functionality
### Databases
The ZDT_ATTENDANCE database table stores daily attendance records.
Key features:

    • Record per employee per day,
    • Composite primary key (MANDT, PERNR, DATE),
    • Calculated field HOURS.

### User Interface
The application consists of multiple screens:

    • Screen 0090 – initial screen for entering the user name
    • Screen 0100 – main maintenance screen
    • Screen 0200 – monthly overview displayed as ALV Grid
    
The main screen provides fields:

    • PERNR
    • DATE
    • TIME_IN
    • TIME_OUT
    • NOTE

 and the following buttons:

    • Save
    • Update
    • Delete
    • Show Monthly Overview
    • Print Monthly Report

### Program Architecture Overview
The program is implemented as a classic report with Dynpro screens and is structured into clearly separated sections:

    • global type definitions,
    • global data declarations,
    • Dynpro modules,
    • business logic implemented using FORM routines,
    • ALV processing logic,
    • SmartForm integration.

This separation ensures clarity and simplifies future extensions.
  
## Example
Example entry: 

   PERNR: 01234567

   Date: 02022026

   TIME_IN: 08:00:00

   TIME_OUT: 14:00:00
   
Result:

   Worked hours calculated automatically

   Monthly overview displays daily records for the month

   Monthly report prints header (employee + month), attendance table, total hours and signature area.
