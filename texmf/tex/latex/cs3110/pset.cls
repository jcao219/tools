%% pset.cls (A class file for CS 3110 problem sets)
%%
%% version 2
%% Aug 28, 2013

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{pset}
\LoadClass[12pt]{article}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Package Imports %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{3110}
\RequirePackage{hyperref}
\RequirePackage{fourier}
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
		Data Structures and Functional Programming\hfill Problem Set \@psnum \@psname\\ 
		CS 3110, Spring 2014\hfill \@date \\
                Version: \@versionnumber \hfill Last Modified: \@datemodified
	}}
	\end{center}
	\vspace{1em}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Standard Instructions %%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\standardinstructions}{
  \section*{Instructions}
  \subsection*{Compile Errors}
  All code you submit must compile.
  {\bf Programs that do not compile will be heavily penalized.}
  If your submission does not compile, we will notify you
  immediately.  You will have 48 hours after the submission date
  to supply us with a patch.  If you do not submit a patch, or if
  your patched code does not compile, you will receive an
  automatic zero.
  
  \subsection*{Naming}
  We will be using an automatic grading script, so it is
  {\bf{crucial}} that you name your functions and order their
  arguments according to the problem set instructions, and that you
  place the functions in the correct files. Incorrectly named
  functions are {\bf treated as compile errors} and you will have to
  submit a patch.

  \subsection*{Code Style}
  Finally, please pay attention to style. Refer to the
  \link{http://www.cs.cornell.edu/courses/cs3110/2012fa/handouts/style.html}{CS
    3110 style guide} and lecture notes. Ugly code that is
  functionally correct may still lose points. Take the extra time to
  think out the problems and find the most elegant solutions before
  coding them up. Good programming style is important for all assignments
  throughout the semester.
  
  \subsection*{Late Assignments}
  Please carefully review the course website's policy on late
  assignments, as {\bf{all}} assignments handed in after the
  deadline will be considered late. Verify on CMS that you have
  submitted the correct version, {\bf{before}} the deadline.
  Submitting the incorrect version before the deadline and realizing
  that you have done so after the deadline will be counted as a late
  submission.  }

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
	\addtocounter{problem}{1}
	\setcounter{exercise}{0}
	\section*{Problem \theproblem: #1 {\normalsize(#2 pts)}}
}

\newcommand\exercise[1]{
	\addtocounter{exercise}{1}
	\subsection*{Exercise \theexercise: #1}
}

\newcommand\examples{\subsubsection*{Examples}}

%
% vim: syntax=tex
%

%%
%% End of file 'pset.cls'
