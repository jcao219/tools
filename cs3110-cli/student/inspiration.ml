
Random.self_init ()

(* This is the bank of CS3110 Quotes from over the semesters *)
let quote_lst = 
 ["Don't grade bad -kns";
	"Frankly, I would say I have a gluttony for parentheses - RDZ";
	"Sometimes in life, you have to give up - Mr. Donalson";
	"That's all just ML minutiae. - RDZ";
	"Repeat after me: I am not a no-compile - blg59";
	"I talked to Mr. Springsteen and he suggested we defer logic. And he's The Boss - RDZ";
	"Peter, I really enjoyed the email thread where you sustained a conversation with yourself over the course of roughly 17 hours -gez3";
	"A perfect hash function will take a Perl program and translate it to OCaml -3110 student";
	"These aren't bags! They're douche-bags! -3110 student";
	"We shared the work equally, with one student writing code, the other compiling it, and also using the general discussions that are prevalent in these group assignments. -3110 student, PS6 documentation";
	"CS3110 is like the ugly girl your best friend paid you to date for a semester. At first, you question why you're even doing it, and keep telling yourself to trudge onward despite your reluctance to do so. However, as time goes on, you realize her distinctive personality traits, and come to not loathe those biweekly Thursday-night dates, sometimes even preparing well in advance. By the end of the course, your attitude towards her has changed from tolerating to accepting, where you come to find that hey, maybe she's not that ugly after all. But then you come to your senses and realize that she actually is that ugly and useless, and then you hate yourself. - ACSU Wiki Page";
	"If any of you guys need something to hack on this summer, the ICFP programming contest might keep you busy! -- Andrew";
	"A list of known (and unknown) problems -3110 student, PS6 documentation";
	"OCaml Version: [emacs] - PS1 submission header";
	"3110 Staff A: 'Can I yell at them for bad variable names?'  3110 Staff B: 'Yeah, someone named the head of the list [t]' - Grading Session";
	"| 0 -> (fun x -> x) x - PS1 submission function base case";
	"let b = (*complicated expression*) in b - PS1 submission";
	"Student A: 'Does your function work?' Student B: 'Well, it compiles.' - Office Hours";
	"What... what is their default tab size? - 3110 staff";
	"Ben Greenman is God - MapReduce commit message 07102ce207b5220376c624fb3b751124da6221a6";
	"A good fisherman never chooses his own lure - Fishing Hut Owner";
	"Whether a parting be forever or merely for a short time... That is up to you - Happy Mask man";
	"I don't remember much of the 90s - mgn29";
	"May the almighty 3110 gods have mercy on my poor, pathetic soul - 3110 student";
	"3110 is really just intro to match statements with some other stuff thrown in - Luke";
	"Instructions: Light 7 candles and pray to the Gods of OCaml. Offer sever bowls of curry along with incense and then use the Makefile -Design Document";
	"Some people learned some things this semester -bkc39";
	"Windows is an operating system used by people who design PowerPoints -mdgeorge";
	"Jianneng! You're incredible! -3110 student";
	"A computer is an instrument whose music is ideas - Alan Kay";
	"Priority queues just make me get excited -mdgeorge";
	"Implement fold_left (without using rec?) -problem set 4, part 3";
	"'Is there an advanced programming languages course?' 'Yeah, it's called coffee with Nate Foster' -students";
	"I tend to not use the ps6 constants given cause I know them all... -ejy"]

let random_quote () = 
	let i = Random.int (List.length quote_lst) in
	print_string (List.nth quote_lst i)





















