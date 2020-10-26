type elem = FLO of float		(*type of elements that will be stored in the spreadsheet*)
	| EMPTY
type spreadsheet = elem array array 	(*type of spreadsheet is array of array of elem*)
let m = int_of_string Sys.argv.(2);; (*Number rows in the spreadsheet*)
let n = int_of_string Sys.argv.(3);; (*Number of columns in the spreadsheet*)
let sheet:spreadsheet = Array.make_matrix m n EMPTY	(*Default  Spreadsheet*)
exception INDEX_OUT_OF_BOUND			(*throw this exception when the ant of the input indices are out of bounds of the spreadsheet*)
exception EMPTY_CELL					(*throw this exception when a cell is empty except in functions for counting*)
(*Counts the number of non-empty entries in an array with the given range (d,f) and returns the count*)
let rec array_size arr count index d f = if index > f then count else 											(*if the index exceeds the upper bound of the given range then reutrn the count*)
											if index<d then (array_size arr count (index+1) d f) else  			(*if the index is lesser than the lower bound of the given range then move to the nex element*)
											match arr.(index) with 
												| EMPTY -> (array_size arr count (index+1) d f)					(*if the entry is empty move to the next entry without increasing the count*)
												| _ -> (array_size arr (count+1) (index+1) d f)					(*if the entry is non-empty then increment count and move to the next entry*)
											;;
(*Takes an array and checks if it is in the given range and returns the count of number of non-empty elements in it*)
let wrapper_array_size c d e f index s = if index<c || index>e then 0 else
											array_size s 0 0 d f;;
(*First checks if any index is out of bounds for the spreadsheet, throws an exception else using Array.mapi creates an array containing the count of all the arrays in the sheet and then using fold_right sums them all and finally stores it back in the sheet*)
let full_count (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						(s.(e).(f)<-FLO(float_of_int (Array.fold_right (fun x y-> x+y) (Array.mapi (wrapper_array_size a b c d) s) 0))) ;
																						s;;
(*Takes an array and stores each of its element in a cell of the spreadsheet all in the same column*)
let rec row_store i j e arr index s = if i>=(e+(Array.length arr)) then s else 			(*Upper bound to stop filling*)
										begin
										(s.(i).(j)<-FLO(float_of_int (arr.(index)))) ;	(*Stores the count of one row in the required location*)
										(row_store (i+1) j e arr (index+1) s)			(*Recursive call*)
										end ;;
(*Fills count of valid entries per row in the given range into the column starting from the specified cell*)
let row_count (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						(row_store e f e (Array.mapi (wrapper_array_size a b c d) s) 0 s);;
(*Counts the number of non-empty elements in a column of the given spreadsheet*)
let rec col_count_help s i (count: int) a c = if a>c then count else
												match s.(a).(i) with 
													| EMPTY -> col_count_help s i count (a+1) c
													| _ -> col_count_help s i (count+1) (a+1) c
												;;
(*First checks if all the indices are in the bounds of the spreadsheet, then counts the number of non-empty elements in each column and stores the count for each column in the spreadsheet*)
let col_count (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						for i = b to d do
																							s.(e).(i) <-FLO(float_of_int (col_count_help s i 0 a c))
																						done;
																						s;;
(*Goes through an  array and sums all the non-empty elements and returns the sum*)
let rec array_sum arr (sum: float) index d f = if index > f then sum else 
										if index<d then (array_sum arr sum (index+1) d f) else
										match arr.(index) with
											| EMPTY -> raise EMPTY_CELL
											| FLO(x) -> (array_sum arr (sum+.x) (index+1) d f)
										;;
(*Takes an array and checks if it is in the given range and returns the summ of all its elements*)
let wrapper_array_sum c d e f index s = if index<c || index>e then 0. else
											array_sum s 0. 0 d f;;
(*First checks if any index is out of bounds for the spreadsheet, throws an exception else using Array.mapi creates an array containing the sum of all the elements of every array respectively and then sum them all using fold_right and finally stores it back in the sheet*)
let full_sum (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						(s.(e).(f)<-FLO(Array.fold_right (fun x y-> x+.y) (Array.mapi (wrapper_array_sum a b c d) s) 0.)) ;
																						s;;
(*Goes through a column and sums all the non-empty elements and returns the sum*)
let rec col_sum_help s i (sum: float) a c = if a>c then sum else
												match s.(a).(i) with 
													| EMPTY -> raise EMPTY_CELL
													| FLO(x) -> col_sum_help s i (sum+.x) (a+1) c
												;;
(*Calculates the sum of all the non-empty elements of a column and stores it at the required output index*)
let col_sum (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						for i = b to d do
																							s.(e).(i) <-FLO(col_sum_help s i 0. a c)
																						done;
																						s;;
(*Takes an array and stores each of its element in a cell of the spreadsheet all in the same column*)
let rec row_store_flo i j e arr index s = if i>=(e+(Array.length arr)) then s else 			(*Upper bound to stop filling*)
										begin
										(s.(i).(j)<-FLO(arr.(index))) ;						(*Stores the count of one row in the required location*)
										(row_store_flo (i+1) j e arr (index+1) s)				(*Recursive call*)
										end ;;
(*Creates an array containing the sum of elements of every row in the given range and using row_store_flo stores them at the output indices*)
let row_sum (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						(row_store_flo e f e (Array.mapi (wrapper_array_sum a b c d) s) 0 s);;
(*First checks if any index is out of bounds for the spreadsheet, throws an exception else using Array.mapi creates an array containing the average of all the elements of every array respectively and then sum them all using fold_right and finally stores it back in the sheet*)
let full_avg (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						(s.(e).(f)<-FLO((Array.fold_right (fun x y-> x+.y) (Array.mapi (wrapper_array_sum a b c d) s) 0.)/.(float_of_int (Array.fold_right (fun x y-> x+y) (Array.mapi (wrapper_array_size a b c d) s) 0))));
																						s;;
(*Using col_sum_help and col_count_help calculates the average of all the elements of a column and stores them at the required output index*)
let col_avg (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						for i = b to d do
																							s.(e).(i) <-FLO((col_sum_help s i 0. a c)/.(float_of_int(col_count_help s i 0 a c)))
																						done;
																						s;;
let array_avg c d e f index s = ((wrapper_array_sum c d e f index s)/.(float_of_int(wrapper_array_size c d e f index s)));;  		(*Returns the average of all the elements of the given array*)
(*Stores the average of each row in the required output index*)
let row_avg (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						(row_store_flo e f e (Array.mapi (array_avg a b c d) s) 0 s);;
(*Goes through an array and finds the minimum element and returns it. Throws an exception whenever it encounters an empty cell*)
let rec array_min arr (min: float) index d f = if index > f then min else
												if index<d then (array_min arr min (index+1) d f) else
												match arr.(index) with 
													| EMPTY -> raise EMPTY_CELL
													| FLO(x) -> if (x<min) then (array_min arr x (index+1) d f) else (array_min arr min (index+1) d f)
												;;
(*Takes an array and checks if it is  in the given range and returns the minimum element in that array*)
let wrapper_array_min c d e f index s = if index<c || index>e then max_float else
											array_min s max_float 0 d f;;
(*First checks if any index is out of bounds for the spreadsheet, throws an exception else using Array.mapi creates an array containing the minimum element of each arrays in the spreadsheet and the finds the minimum element of that array and stores it in the spreadsheet*)
let full_min (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						(s.(e).(f)<-FLO(Array.fold_right (fun x y -> if x>y then y else x) (Array.mapi (wrapper_array_min a b c d) s) max_float));
																						s;;
(*Goes through a column and returns the minimum value in that column, raises exception is any element of the column is an empty cell*)
let rec col_min_help s i (min: float) a c = if a>c then min else
												match s.(a).(i) with 
													| EMPTY -> raise EMPTY_CELL
													| FLO(x) -> if (x<min) then (col_min_help s i x (a+1) c) else (col_min_help s i min (a+1) c)
												;;
(*Stores the minimum element of each column in the required output index*)
let col_min (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						for i = b to d do 							(*For loop iterated over all the columns where the output has to be stored*)
																							s.(e).(i) <-FLO(col_min_help s i max_float a c)
																						done;
																						s;;
(*Similar to full_min, instead of finding minimum of the elements of the array obtained from Array.mapi it stores each of the elements of the array in the sheet at output indices*)
let row_min (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						(row_store_flo e f e (Array.mapi (wrapper_array_min a b c d) s) 0 s);;
(*Goes through an array and finds the maximum element and returns it. Throws an exception whenever it encounters an empty cell*)
let rec array_max arr (max: float) index d f = if index > f then max else 
												if index<d then (array_max arr max (index+1) d f) else
												match arr.(index) with
													| EMPTY -> raise EMPTY_CELL
													| FLO(x) -> if (x>max) then (array_max arr x (index+1) d f) else (array_max arr max (index+1) d f)
												;;
(*Takes an array and checks if it is  in the given range and returns the maximum element in that array*)
let wrapper_array_max c d e f index s = if index<c || index>e then min_float else
											array_max s min_float 0 d f;;
(*First checks if any index is out of bounds for the spreadsheet, throws an exception else using Array.mapi creates an array containing the maximum element of each arrays in the spreadsheet and the finds the maximum element of that array and stores it in the spreadsheet*)
let full_max (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						(s.(e).(f)<-FLO(Array.fold_right (fun x y -> if x>y then x else y) (Array.mapi (wrapper_array_max a b c d) s) min_float));
																						s;;
(*Goes through a column and returns the maximum value in that column, raises exception is any element of the column is an empty cell*)
let rec col_max_help s i (max: float) a c = if a>c then max else
												match s.(a).(i) with 
													| EMPTY -> raise EMPTY_CELL
													| FLO(x) -> if (x>max) then (col_max_help s i x (a+1) c) else (col_max_help s i max (a+1) c)
												;;
(*Stores the maximum element of each column in the required output index*)
let col_max (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						for i = b to d do
																							s.(e).(i) <-FLO(col_max_help s i min_float a c)
																						done;
																						s;;
(*Similar to full_max, instead of finding maximum of the elements of the array obtained from Array.mapi it stores each of the elements of the array in the sheet at output indices*)
let row_max (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																						(row_store_flo e f e (Array.mapi (wrapper_array_max a b c d) s) 0 s);;
(*Takes the constant float g and returns the sum of g and the float stored at ((j+a),(i+b)) in the spreadsheet*)
let add_const_help i j g a b s = match s.(j+a).(i+b) with 
									| EMPTY -> raise EMPTY_CELL
									| FLO(x) -> (x+.g)
								;;
(*Iterates over all the elements of the spreadasheet in the given range and stores the output at the required indices*)
let add_const (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (const: float) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																								for i = 0 to (d-b) do
																									for j = 0 to (c-a) do
																										s.(e+j).(f+i) <- FLO(add_const_help i j const a b s)
																									done;
																								done;
																								s;;
let subt_const_help i j g a b s = match s.(j+a).(i+b) with 
									| EMPTY -> raise EMPTY_CELL
									| FLO(x) -> (x-.g)
								;;
let subt_const (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (const: float) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																								for i = 0 to (d-b) do
																									for j = 0 to (c-a) do
																										s.(e+j).(f+i) <- FLO(subt_const_help i j const a b s)
																									done;
																								done;
																								s;;
let mult_const_help i j g a b s = match s.(j+a).(i+b) with 
									| EMPTY -> raise EMPTY_CELL
									| FLO(x) -> (x*.g)
								;;
let mult_const (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (const: float) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																								for i = 0 to (d-b) do
																									for j = 0 to (c-a) do
																										s.(e+j).(f+i) <- FLO(mult_const_help i j const a b s)
																									done;
																								done;
																								s;;
let div_const_help i j g a b s = match s.(j+a).(i+b) with 
									| EMPTY -> raise EMPTY_CELL
									| FLO(x) -> (x/.g)
								;;
let div_const (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (const: float) (e: int) (f: int) = if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																								for i = 0 to (d-b) do
																									for j = 0 to (c-a) do
																										s.(e+j).(f+i) <- FLO(div_const_help i j const a b s)
																									done;
																								done;
																								s;;
(*Function to convert user-defined FLO to float*)
let flo_to_float const = match const with
							| EMPTY -> raise EMPTY_CELL
							| FLO(x) -> x
						;;
(*Similar to add_const, instead of constant it extracts the float value at the given index in the spreadsheet and uses it*)
let add_index (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (g: int) (h: int) (e: int) (f: int) =  if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																												let const = s.(g).(h) in
																												for i = 0 to (d-b) do
																													for j = 0 to (c-a) do
																														s.(e+j).(f+i) <- FLO(add_const_help i j (flo_to_float const) a b s)
																													done;
																												done;
																											s;;
let subt_index (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (g: int) (h: int) (e: int) (f: int) =  if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																												let const = s.(g).(h) in
																												for i = 0 to (d-b) do
																													for j = 0 to (c-a) do
																														s.(e+j).(f+i) <- FLO(subt_const_help i j (flo_to_float const) a b s)
																													done;
																												done;
																											s;;
let mult_index (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (g: int) (h: int) (e: int) (f: int) =  if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																												let const = s.(g).(h) in
																												for i = 0 to (d-b) do
																													for j = 0 to (c-a) do
																														s.(e+j).(f+i) <- FLO(mult_const_help i j (flo_to_float const) a b s)
																													done;
																												done;
																											s;;
let div_index (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (g: int) (h: int) (e: int) (f: int) =  if a>=m||c>=m||e>=m||f>=n||d>=n||b>=n then raise INDEX_OUT_OF_BOUND else
																												let const = s.(g).(h) in
																												for i = 0 to (d-b) do
																													for j = 0 to (c-a) do
																														s.(e+j).(f+i) <- FLO(div_const_help i j (flo_to_float const) a b s)
																													done;
																												done;
																											s;;																											
(*Sum the float values at indices ((j+a),(i+b)) and ((j+t),(i+u)) in the spreadsheet s, throws exception if any one of the cells are empty*)
let add_range_help i j a b t u s = match s.(j+a).(i+b), s.(j+t).(i+u) with
									| EMPTY, _ -> raise EMPTY_CELL
									| _, EMPTY -> raise EMPTY_CELL
									| FLO(x), FLO(y) -> (x+.y)
								;;
(*Iteratively goes over the whole spreadsheet in the given range and stores the outputs at the required indices*)
let add_range (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (t: int) (u: int) (v: int) (w: int) (e: int) (f: int) = if a>=m||c>=m||t>=m||v>=m||e>=m||f>=n||d>=n||b>=n||u>=n||w>=n then raise INDEX_OUT_OF_BOUND else
																															for i = 0 to (d-b) do
																																for j = 0 to (c-a) do
																																	s.(e+j).(f+i) <- FLO(add_range_help i j a b t u s)
																																done;
																															done;
																														   s;;
let subt_range_help i j a b t u s = match s.(j+a).(i+b), s.(j+t).(i+u) with
									| EMPTY, _ -> raise EMPTY_CELL
									| _, EMPTY -> raise EMPTY_CELL
									| FLO(x), FLO(y) -> (x-.y)
								;;
let subt_range (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (t: int) (u: int) (v: int) (w: int) (e: int) (f: int) = if a>=m||c>=m||t>=m||v>=m||e>=m||f>=n||d>=n||b>=n||u>=n||w>=n then raise INDEX_OUT_OF_BOUND else
																															for i = 0 to (d-b) do
																																for j = 0 to (c-a) do
																																	s.(e+j).(f+i) <- FLO(subt_range_help i j a b t u s)
																																done;
																															done;
																														   s;;
let mult_range_help i j a b t u s = match s.(j+a).(i+b), s.(j+t).(i+u) with
									| EMPTY, _ -> raise EMPTY_CELL
									| _, EMPTY -> raise EMPTY_CELL
									| FLO(x), FLO(y) -> (x*.y)
								;;
let mult_range (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (t: int) (u: int) (v: int) (w: int) (e: int) (f: int) = if a>=m||c>=m||t>=m||v>=m||e>=m||f>=n||d>=n||b>=n||u>=n||w>=n then raise INDEX_OUT_OF_BOUND else
																															for i = 0 to (d-b) do
																																for j = 0 to (c-a) do
																																	s.(e+j).(f+i) <- FLO(mult_range_help i j a b t u s)
																																done;
																															done;
																														   s;;
let div_range_help i j a b t u s = match s.(j+a).(i+b), s.(j+t).(i+u) with
									| EMPTY, _ -> raise EMPTY_CELL
									| _, EMPTY -> raise EMPTY_CELL
									| FLO(x), FLO(y) -> (x/.y)
								;;
let div_range (s: spreadsheet) (a: int) (b: int) (c: int) (d: int) (t: int) (u: int) (v: int) (w: int) (e: int) (f: int) = if a>=m||c>=m||t>=m||v>=m||e>=m||f>=n||d>=n||b>=n||u>=n||w>=n then raise INDEX_OUT_OF_BOUND else
																															for i = 0 to (d-b) do
																																for j = 0 to (c-a) do
																																	s.(e+j).(f+i) <- FLO(div_range_help i j a b t u s)
																																done;
																															done;
																														   s;;