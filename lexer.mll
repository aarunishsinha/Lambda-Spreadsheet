{	
	open Parser
	exception Eof 	
}
(*CHANGE : All the tokens removed as the tokens are defined in the parser file*)
	let digit = ['0' - '9']						(*Defining a regexp digit*)
					(* Pattern Matching *)
rule token = parse
	| [' ' '\t']		{token lexbuf }			(*Skipping whitespaces*)
	| "\n"				{token lexbuf } 		(* Move to next line*)
	| ';' 				{ TERMIN }
	| ":=" 				{ ASSIGN }
	| ',' 				{ COMMA }
	| '('				{ OPENP }
	| ')'				{ CLOSEP }
	| '['				{ OPENB }
	| ']'				{ CLOSEB }
	| ','				{ COMMA }
	| ':'				{ COLON }
	| "COUNT"			{ COUNT }
	| "ROWCOUNT"		{ ROWCOUNT }
	| "COLCOUNT"		{ COLCOUNT }
	| "SUM"				{ SUM }
	| "ROWSUM"			{ ROWSUM }
	| "COLSUM"			{ COLSUM }
	| "AVG"				{ AVG }
	| "ROWAVG"			{ ROWAVG }
	| "COLAVG" 			{ COLAVG }
	| "MIN" 			{ MIN }
	| "ROWMIN" 			{ ROWMIN }
	| "COLMIN" 			{ COLMIN }
	| "MAX"				{ MAX }
	| "ROWMAX"			{ ROWMAX }
	| "COLMAX" 			{ COLMAX }
	| "ADD"				{ ADD }
	| "SUBT"			{ SUBT }
	| "MULT" 			{ MULT }
	| "DIV" 			{ DIV }
	| digit+ as lsm		{ INDEX(int_of_string lsm) }
	| digit+('.'(digit+)) as lxm	{ NUM(float_of_string lxm) }		(*For floats check for digits then a decimal point and then again digits*)
	| eof				{raise Eof}									(* Raising End-of-file exception when all the input in the file has been tokenised*)
{

}
			
	
