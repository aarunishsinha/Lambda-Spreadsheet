# Lambda-Spreadsheet
COL226 - Programming Languages

I have implemented a small language for computing on spreadsheets. A spreadsheet (similar to an Excel sheet) is a dynamic data structure essentially made up cells, arranged in rows and columns, that can help store and process data.
## File Heirarchy
### lexer.mll
The ```lex``` specifications for the language are provided in this file
### parser.mly
The structure of the language as a ```yacc``` grammar has been specified in this file.
### backend.ml and driver.ml
The backend implementation of all the functions in this language is present in the file ```backend.ml```. The entire work has be integrated in the file ```driver.ml```.
### Makefile
Contains all the commands to compile and execute the files.

Input format:
```
sheet_name.csv <num_cols> <num_rows> function_file.txt
```
## Language Description
### Informal Syntax
Informally the grammar of our spreadsheet formula language is:
- An instruction has a head and a body. The head consists of a cell indexing the output
destination of the formula, and the body consists of an expression being evaluated with
respect to the spreadsheet. The head and the body are separated by an assignment sign (:=)
- A formula ends with a semicolon (;)
- The arguments of a function are either indices, ranges, constants or expressions using
mathematical operations
- An index is a pair of integers. General format for an index I of a cell is [i, j] Examples are
[0,0], [3,4], ...
- A range consists of two indices standing for the top left index and the right bottom index.
General format for an range R of cells is ( I : I ) with an example being ( [ 5, 6] : [100, 6] )

### Tokens
Tokens for OCamllex would be:
- Float constants (with optional sign, no redundant initial zeroes before the decimal point or
unnecessary trailing zeroes after the decimal point)
- Parenthesis — ( and )
- Brackets — [ and ]
- Comma — ,
- Colon—:
- IndicesI—[i,j]
- RangesR—(I:I)
- Unary operators: SUM, AVG, MIN, MAX, COUNT, etc. (see below)
- Binary operators: ADD (addition), SUBT (subtraction), MULT (multiplication), DIV (division)
- Assignment operator :=
- Formula termination ; (semicolon)

### Functions
Formulas will be of 3 kinds: overall, row-wise, column-wise. The cell selection would be made by specifying the top-left and bottom-right cell indices. Results would be filled into the sheet by specifying only the top-left cell of the target cell/row/column.

The function operations can be one of the following:

Type 1 (unary operations on ranges of cells): 
- COUNT
- ROWCOUNT 
- COLCOUNT 
- SUM
- ROWSUM
- COLSUM 
- AVG
- ROWAVG 
- COLAVG 
- MIN
- ROWMIN 
- COLMIN 
- MAX
- ROWMAX 
- COLMAX
Type 2 (binary operations on ranges of cells): 
- ADD
- SUBT 
- MULT 
- DIV

The arguments of these functions may be either the cell ranges or might be constants. Think of weighted sum as an example. You will multiply a column by a constant and add it to another column to give the final result. Then you might multiply by 100 and divide by the total marks in order to get the percentage.
The formulas provided to you are one on each line. The general format of a formula is given below: 

Type 1 formula (unary operations on a range of cells)
- I:=FUNC R ;

Type 2 formula (binary operations on ranges)
- I := FUNC R R ;
- I:=FUNC C R;
- I := FUNC R C ;
- I:=FUNC I R;
- I := FUNC R I ;

### Backend
In order to evaluate the above functions, you might want to implement the following in OCaml:
- full_count: sheet -> range -> index -> sheet : Fills count of valid entries in the given range into
the specified cell
- row_count: sheet -> range -> index -> sheet : Fills count of valid entries per row in the given
range into the column starting from the specified cell
- col_count: sheet -> range -> index -> sheet : Fills count of valid entries per column in the given
range into the row starting from the specified cell.
- full_sum: sheet -> range -> index -> sheet : Fills the sum of entries of cells in the given range into the specified cell
- row_sum: sheet -> range -> index -> sheet : Fills the sum of entries of cells per row in the given range into the column starting from the specified cell
- col_sum: sheet -> range -> index -> sheet : Fills the sum of entries of cells per column in the given range into the row starting from the specified cell
- full_avg: sheet -> range -> index -> sheet : Fills the average of entries of cells in the given range into the specified cell
- row_avg: sheet -> range -> index -> sheet : Fills the average of entries of cells per row in the given range into the column starting from the specified cell
- col_avg: sheet -> range -> index -> sheet : Fills the sum of entries of cells per column in the given range into the row starting from the specified cell
- full_min: sheet -> range -> index -> sheet : Fills the min of entries of cells in the given range into the specified cell
- row_min: sheet -> range -> index -> sheet : Fills the min of entries of cells per row in the given range into the column starting from the specified cell
- col_min: sheet -> range -> index -> sheet : Fills the min of entries of cells per column in the given range into the row starting from the specified cell
- full_max: sheet -> range -> index -> sheet : Fills the max of entries of cells in the given range into the specified cell
- row_max: sheet -> range -> index -> sheet : Fills the max of entries of cells per row in the given range into the column starting from the specified cell
- col_max: sheet -> range -> index -> sheet : Fills the max of entries of cells per column in the given range into the row starting from the specified cell
- add_const: sheet -> range -> float -> index -> sheet : adds a constant to the contents of each cell in the selected cell range
- subt_const: sheet -> range -> float -> index -> sheet : subtracts a constant from the contents of each cell in the selected cell range
- mult_const: sheet -> range -> float -> index -> sheet : multiplies the contents of each cell in the selected cell range by a constant.
- div_const: sheet -> range -> float -> index -> sheet : divides the contents of each cell in the selected cell range by a constant.
- add_range: sheet -> range -> range -> index -> sheet : adds the cell contents for each corresponding pair of cells in two selected cell ranges
- subt_range: sheet -> range -> range -> index -> sheet : performs a subtraction on the cell contents for each corresponding pair if cells in two selected cell ranges
- mult_range: sheet -> range -> range -> index -> sheet : multiplies the cell contents for each corresponding pair of cells in two selected cell ranges
- div_range: sheet -> range -> range -> index -> sheet : performs a division on the cell contents for each corresponding pair of cells in two selected cell ranges
