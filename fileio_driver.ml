open Backend
let m = int_of_string Sys.argv.(2);; (*Number of rows in the spreadsheet*)
let n = int_of_string Sys.argv.(3);; (*Number of columns in the spreadsheet*)
(*Fills the corresponding values from the csv file into the spreadsheet*)
let rec sheet_fill i j = function 
[] -> ()
| x::xs -> if x = "" then Backend.sheet.(i).(j) <- EMPTY
		  else 
		  begin 
			Backend.sheet.(i).(j) <- FLO(float_of_string x); 
			sheet_fill i (j+1) xs;
		  end;;


let _ =
  try
    let in_stream = open_in Sys.argv.(1) in
        for i=0 to (m-1) do
          let line = input_line in_stream in
          let split = Str.split (Str.regexp ",") in
          let values = split line in
            sheet_fill i 0 values;
        done;
        close_in in_stream; 
  with e ->
    Printf.printf "File not found!";
    raise e



let main () =
try
let cin =
     if Array.length Sys.argv > 4
      then open_in Sys.argv.(4)
    else stdin
      in
     let lexbuf = Lexing.from_channel cin in
while true do
	Parser.main Lexer.token lexbuf;
	for i = 0 to (Array.length Backend.sheet - 1) do 
		for j = 0 to (Array.length Backend.sheet.(0) - 1) do 
			match Backend.sheet.(i).(j) with
			| EMPTY -> Printf.printf "Empty "
			| FLO(v) -> Printf.printf "%f " v 
		done;
		Printf.printf "\n"
	done
done;
with Lexer.Eof -> exit 0
let _ = Printexc.print main ()