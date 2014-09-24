%% pset.cls (A class file for CS 3110 Problem Sets)
%%
%% version 2
%% Aug 28, 2013

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{pset}
\LoadClass[12pt]{article}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Package Imports %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{3110}
\RequirePackage{hyperref}
\hypersetup{
  colorlinks=true,
  urlcolor=blue
}
\RequirePackage[letterpaper,margin=1in]{geometry}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page Layout %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\setlength\parindent{1em}
\addtolength\parskip{0.1cm}
\setlength\fboxrule{.5mm}\setlength{\fboxsep}{1.2mm}
\newlength\boxlength\setlength\boxlength\textwidth
\addtolength\boxlength{-4mm}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Assignment Header %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand\@psnum{}
\newcommand\@psname{}
\newcommand\psname[1]{\renewcommand\@psname{#1}}
\newcommand\psnum[1]{\renewcommand\@psnum{#1}}

\newcommand\instructions{\section*{Instructions}}

\renewcommand\@date{}
\renewcommand\date[1]{\renewcommand{\@date}{#1}}
\newcommand\psdue[1]{\date{Due at 11:59 PM, #1}}

\newcommand\@versionnumber{}
\newcommand\versionnumber[1]{\renewcommand{\@versionnumber}{#1}}

\newcommand\@datemodified{}
\newcommand\datemodified[1]{\renewcommand{\@datemodified}{#1}}
\datemodified{\today}

\renewcommand\maketitle{
	\vspace*{-5em}
	\begin{center}\framebox{\parbox{\boxlength}{\bf
		CS 3110 Fall 2014\hfill \@date \\
        Problem Set \@psnum \@psname\\ 
        Version \@versionnumber~(last modified \@datemodified)}}
	\end{center}
	\vspace{1em}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Standard Instructions %%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\standardinstructions}{
  \section*{Grading issues}
  \paragraph*{Compilation errors:}
  All code you submit must compile.
  If your submission does not compile, we will notify you
  immediately.  You will have 48 hours after the due date
  to supply us with a patch.  If you do not submit a patch, or if
  your patched code does not compile, you will receive an
  automatic zero.
  
  \paragraph*{Naming:}
  We use automated grading, so it is
  crucial that you name your functions and order their
  arguments according to the problem set instructions, and that you
  place the functions in the correct files. Incorrectly named
  functions will be treated as compilation errors.

  \paragraph*{Code style:}
  Refer to the \href{http://www.cs.cornell.edu/Courses/cs3110/2014fa/handouts/style.html}
  {CS 3110 style guide} and lecture notes. 
  Ugly code that is functionally correct will nonetheless be penalized. 
  Take extra time to think and find elegant solutions. 
  
  \paragraph*{Late submissions:}
  Carefully review the \href{http://www.cs.cornell.edu/Courses/cs3110/2014fa/course_info.php\#programming_assignments}
  {course policy on submission and late assignments}.
  Verify before the deadline on CMS that you have submitted the correct version. 
}

%% TODO:
%%   Write a macro that generates the files to submit. Usage:
%%   \maketosubmit{<list-of-files>}

% Problems and solutions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand*{\spelled}[1]{%
  \expandafter\@spelled\csname c@#1\endcsname
}
\newcommand*{\@spelled}[1]{%
  \ifcase#1\or{One}\or{Two}\or{Three}\or{Four}\or{Five}\or{Six}\or{Seven}\or{Eight}\or{Nine}\or{Ten}\else\@ctrerr\fi
}

\newcounter{problem}
\newcounter{exercise}
\newcounter{pspart}

\renewcommand{\part}[1]{
	\addtocounter{pspart}{1}
	\section*{Part \spelled{pspart}: #1}
}

\newcommand\bracket[1]{\ifx#1\empty\else[#1]\fi}

\newcommand\problem[2]{
	\refstepcounter{problem}
	\setcounter{exercise}{0}
	\ifx&#1&%
  		\section*{Problem \theproblem~{\normalsize(#2 points)}}
	\else
		\section*{Problem \theproblem: #1~{\normalsize(#2 points)}} 		
	\fi
}

\newcommand\exercise[1]{
	\refstepcounter{exercise}
	\ifx&#1&%
  		\subsection*{Exercise \theexercise.}
	\else
		\subsection*{Exercise \theexercise: #1.}  		
	\fi
}

\newcommand\examples{\subsubsection*{Examples}}

\newcommand{\todo}[1]{\textcolor{blue}{[[~\textit{TODO: #1}~]]}}

%
% vim: syntax=tex
%

%%
%% End of file 'pset.cls'
