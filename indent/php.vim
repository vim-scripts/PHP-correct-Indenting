" Vim indent file
" Language:	PHP
" Author:	John wellesz <John.wellesz (AT) teaser (DOT) fr>
" URL:		http://www.2072productions.com/vim/indent/php.vim
" Last Change:	2004 December 20th
" Version: 1.06
"
" Changes: 1.06:    - Switch block were no longer indented correctly...
"					- Added an option to use a default indenting instead of 0.
"					  (whereas I still can't find any good reason to use it!)
"					- A problem with ^\s*);\= lines where ending a non '{}'
"					  structure.
"					- Changed script local variable to be buffer local
"					  variable instead.
"
" Changes: 1.05:    Lines containing "<?php ?>" and "?> <?php"
"					(start and end tag on the same line) are no
"					longer indented at col 1 but as normal code.
"
"
" Changes: 1.04:	Strings containing "//" could break the indenting
"					algorithm.
"					When a '{}' block was at col 1, the second line of the
"					block was not indented at all (because of a stupid
"					optimization coupled with a bug).
"
" Changes: 1.03:	Some indenting problems corrected: end of non '{}'
"					structures was not detected in some cases. The part of
"					code concerned have been re-written
"					
"					PHP start tags were not indented at col 1
"					Wrong comment in the code have been corrected
"
" Changes: 1.02:	The bug I was talking about in version 1.01 (right below) has
"					been corrected :)
"					Also corrected another bug that could occur in
"					some special cases.
"					I removed the debug mode left in 1.01 that could
"					cause some Vim messages at loading if other script were
"					bugged.
"
" Changes: 1.01:	Some little bug corrections reguarding automatic optimized
"					mode that missed some tests and could break the indenting.
"
"					There is also a problem with complex non bracked structures, when several
"					else are following each other, the algorithm do not indent the way it
"					should.
"					That will be corrected in the next version.
" 
" Changes: improvments with regard to the original version (0.5) by Miles Lott (whose this script was inspired):
"			   - Commented part of code or non PHP code no longer break the
"				 indent algorithm, the content of those parts are indented
"				 separatly
"			   - corrected a strange thing (a part of code was duplicated) ->
"			   the last version couldn't work.
"		       - Folds can be used without problem
"		       - multilevel non bracked structures are indented (like
"		       in C)
"		         Exemple:
"					if (!isset($History_lst_sel)) 
"						if (!isset($blabla)) 
"							if (!isset($bloum)) {
"								$History_lst_sel=0;
"							} else
"							    $foo="truc";
"						else $bla= "foo";
"					$command_hist = TRUE;
"
"			   - "array( 'blabla'," lines no longer break the indent of the
"			     following lines
"			   - the options php_noindent_switch and php_indent_shortopentags have been removed
"			     (couldn't find any reason why one would want to use them)
"			   - PHP open and close tags are always set to col 1 as for the
"			   immediate following php code
"			   
"  If you find a bug, please e-mail me at John.wellesz (AT) teaser (DOT) fr
"  with an example of code that break the algorithm.
"
"
"	Thanks a lot for using this script.
"



" Options: PHP_default_indenting = # of sw (default is 0).



if exists("PHP_default_indenting")
	let b:PHP_default_indenting = PHP_default_indenting * &sw
else
	let b:PHP_default_indenting = 0
endif
" Only load this indent file when no other was loaded. But reset those state
" variables if needed

let b:PHP_lastindented = 0
let b:PHP_identbeforelast = 0
let b:PHP_indentinghuge = 0
let b:PHP_CurrentIndentLevel = b:PHP_default_indenting
let b:PHP_LastIndentedWasComment = 0

if exists("b:did_indent")
	finish
endif
let b:did_indent = 1
setlocal nosmartindent

setlocal nolisp
setlocal indentexpr=GetPhpIndent()
"setlocal indentkeys+=0=,0),=EO
setlocal indentkeys=0{,0},0),:,!^F,o,O,e

" Only define the function once.
if exists("*GetPhpIndent")
	finish
endif

"TODO:	FIX THE PROBLEM THAT WILL OCCUR IF A BLOCK STARTER IS DEFINED OVER
"		SEVERAL LINES
"update: partially fixed

"TODO:	Detect /**/ comment type and format them properly ie: don't loose time
"		on them
"		Indent them sligthly higher than the rest

let s:endline= '\s*\(//.*\|#.*\|/\*.*\*/\s*\)\=$'
" setlocal  debug=msg


function! GetLastRealCodeLNum(startline) " {{{
	"Inspired from the function SkipJavaBlanksAndComments by Toby Allsopp for indent/java.vim 
	let lnum = a:startline
	while lnum > 1
		let lnum = prevnonblank(lnum)
		" Non PHP code is considered as comment
		if getline(lnum) =~ '\(\*/\|\(//\s*\)\@<!<?\(php\)\=\)\s*$'   " if the end of a comment we need to find the beginning
			while getline(lnum) !~ '/\*\|?>' && lnum > 1 || getline(lnum) =~ '\(?>.*\)\@<!<?' && lnum > 1 " while still inside comment
				let lnum = lnum - 1
			endwhile
			if getline(lnum) =~ '^\s*\(/\*\|?>\)' " if line contains nothing but comment
				let lnum = lnum - 1
			else
				break
			endif
		elseif getline(lnum) =~ '^\s*\(//\|#\)' " if line is under comment
			let lnum = lnum - 1
		else
			break
		endif
	endwhile
	return lnum
endfunction
" }}}

function! Skippmatch()  " {{{
   	" the slowest instruction of this script, remove it and the script is 3
	" times faster but you may have troubles with '{' inside comments or strings
	" that will break the indent algorithm...
	let synname = synIDattr(synID(line("."), col("."), 0), "name")
	if synname =~? "string" || synname =~? "phpComment"
		return 1
	else
		return 0
	endif
endfun
" }}}

function! FindOpenBracket(lnum) " {{{
	call cursor(a:lnum, 1) " set the cursor to the start of the lnum line
	return searchpair('{', '', '}', 'bW', 'Skippmatch()')
endfun
" }}}

function! FindTheIfOfAnElse (lnum, StopAfterFirstPrevElse) " {{{
" A very clever recoursive function created by me (John Wellesz) that find the "if" corresponding to an
" "else". This function can easily be adapted for other languages :)
	
	if getline(a:lnum) =~# '^\s*}\s*else\(if\)\=\>'
		let beforeelse = a:lnum " we do this so we can find the opened bracket to speed up the process
	else
		let beforeelse = GetLastRealCodeLNum(a:lnum - 1)
	endif

	if !s:level
		let s:iftoskip = 0
	endif

	" If we found another "else" then it means we need to skip the next "if"
	" we'll found. (since version 1.02)
	if getline(beforeelse) =~# '^\s*\(}\s*\)\=else\(\s*if\)\@!\>'
		let s:iftoskip = s:iftoskip + 1
	endif
	
	" A closing bracket? let skip the whole block to save some recursive calls
	if getline(beforeelse) =~ '^\s*}' " .s:endline
		let beforeelse = FindOpenBracket(beforeelse)

		" Put us on the block starter
		if getline(beforeelse) =~ '^\s*{'
			let beforeelse = GetLastRealCodeLNum(beforeelse - 1)
		endif
	endif


	if !s:iftoskip && a:StopAfterFirstPrevElse && getline(beforeelse) =~# '^\s*\([}]\s*\)\=else\(if\)\=\>'
		return beforeelse
	endif
	" if there was an else, then there is a if...
	if getline(beforeelse) !~# '^\s*if\>' && beforeelse>1 || s:iftoskip && beforeelse>1
		
		if  s:iftoskip && getline(beforeelse) =~# '^\s*if\>'
			let s:iftoskip = s:iftoskip - 1
		endif

		let s:level =  s:level + 1
		let beforeelse = FindTheIfOfAnElse(beforeelse, a:StopAfterFirstPrevElse)
	endif

	return beforeelse

endfunction
" }}}

let s:blockstart = '\(\(\(}\s*\)\=else\)\=if\>\|while\>\|for\(each\)\=\>\|declare\|||\|&&\>\)'

function! GetPhpIndent()
	"##############################################
	"########### MAIN INDENT FUNCTION #############
	"##############################################

	if b:PHP_default_indenting
		let b:PHP_default_indenting = g:PHP_default_indenting * &sw
	endif

	" Let's detect if we are indenting just one line or more than 3 lines
	" in the last case we can slightly optimize our algorithm
	if !b:PHP_indentinghuge && b:PHP_lastindented > b:PHP_identbeforelast 
		if b:PHP_identbeforelast
			let b:PHP_indentinghuge = 1
			echom 'Large indenting detected, speed optimizations engaged'
		endif
		let b:PHP_identbeforelast = b:PHP_lastindented
	endif

	" If the line we are indenting isn't directly under the previous non-blank
	" line of the file then deactivate the optimization procedure
	if b:PHP_indentinghuge && prevnonblank(v:lnum - 1) != b:PHP_lastindented
		echom 'Large indenting deactivated'
		let b:PHP_indentinghuge = 0
		let b:PHP_identbeforelast = 0
		let b:PHP_lastindented = 0
		let b:PHP_CurrentIndentLevel = b:PHP_default_indenting
		let b:PHP_LastIndentedWasComment=0
	elseif v:lnum > b:PHP_lastindented
		let b:PHP_lastindented = v:lnum
	endif

	let s:level = 0

	let cline = getline(v:lnum) " current line

	if b:PHP_indentinghuge && cline =~'^\s*//'
		if b:PHP_LastIndentedWasComment==1
			return indent(b:PHP_lastindented) "  line replaced in 1.02
		endif
		let b:PHP_LastIndentedWasComment=1
	else
		let b:PHP_LastIndentedWasComment=0
	endif

	" Find an executable php code line above the current line.
	let lnum = GetLastRealCodeLNum(v:lnum - 1)

	let cline = getline(v:lnum) " current line
	let last_line = getline(lnum)    " last line
	let ind = indent(lnum) " by default
	let endline= s:endline

	if ind==0 && b:PHP_default_indenting
		let ind = b:PHP_default_indenting
	endif

	" PHP start tags are always at col 1, useless to indent unless the end tag
	" is on the same line
	if cline =~# '^\s*<?\(php\)\='  && cline !~ '?>' " Added the ^\s* part in version 1.03
		return 0
	endif

	" PHP end tags are always at col 1, useless to indent unless if it's
	" followed by a start tag on the same line
	if  cline =~ '^\s*?>' && cline !~# '<?\(php\)\='  
		return 0
	endif
	" Hit the start of the file, use zero indent.
	if lnum == 0
		return b:PHP_default_indenting
	endif

	" if the last line is a stated line and it's not indented then why should
	" we indent this one??
	" if optimized mode active and nor current or previous line are an 'else'
	" or the end of a possible bracketless thing then indent the same as the previous
	" line
	if last_line =~ '[;}]'.endline
		if ind==b:PHP_default_indenting
			return b:PHP_default_indenting
		elseif b:PHP_indentinghuge && ind==b:PHP_CurrentIndentLevel && getline(GetLastRealCodeLNum(lnum - 1))=~';'.endline && cline !~# '^\s*\(else\|\(case\|default\).*:\|[})];\=\)' && last_line !~# '^\s*\(\(}\s*\)\=else\)'
			return b:PHP_CurrentIndentLevel
		endif
	endif

	" Search the matching open bracket (with searchpair()) and set the indent of cline
	" to the indent of the matching line.
	if cline =~ '^\s*}\(}}\)\@!'
		let ind = indent(FindOpenBracket(v:lnum))
		" let b:PHP_CurrentIndentLevel = ind " removed in 1.02
		let b:PHP_CurrentIndentLevel = b:PHP_default_indenting "XXX added in 1.02, optimized mode could do bad things in some cases
		return ind
	endif

	let LastLineClosed = 0
	" The commented line below was a stupidity, the \(;'.endline.'\)\@<! part
	" was always true because the contrary was impossible
	"let unstated='\(;'.endline.'\)\@<!\(^\s*'.s:blockstart.'.*)\|e'.'lse\>\)'.endline
	let unstated='\(^\s*'.s:blockstart.'.*)\|e'.'lse\>\)'.endline
	" What is an unstated line?
	" - an "else" at the end of line
	" - a  s:blockstart (if while etc...) followed by anything and a ")" at
	"   the end of line

	" if the current line is an 'else' starting line
	" (to match an 'else' preceded by a '}' is irrelevant and futile)
	if ind != b:PHP_default_indenting && cline =~# '^\s*else\(if\)\=\>'
		let b:PHP_CurrentIndentLevel = b:PHP_default_indenting "line added in version 1.02 to prevent optimized mode from acting in some special cases
		return indent(FindTheIfOfAnElse(v:lnum, 1))
	elseif last_line =~# unstated && cline !~ '^\s*{\|^\s*);\='.endline
		let ind = ind + &sw
		return ind

		" If the last line is terminated by ';' or if it's a closing '}'
		" We need to check if this isn't the end of a multilevel non '{}'
		" structure such as:
		" Exemple: 
		"			if ($truc)
		"				echo 'truc';
		"
		"	OR
		"
		"			if ($truc)
		"				while ($truc) {
		"					lkhlkh();
		"					echo 'infinite loop\n';
		"				}
	elseif ind != b:PHP_default_indenting && last_line =~ ';'.endline.'\|^\s*}\(.*{'. endline.'\)\@!'
		" If we are here it means that the previous line is:
		" - a *;$ line
		" - a [beginning-blanck]} followed by anything but a {$
		let previous_line = last_line
		let last_line_num = lnum
		let LastLineClosed = 1
		" The idea here is to check if the current line is after a non '{}'
		" structure so we can indent it like the top of that structure.
		" The top of that structure is caracterized by a if (ff)$ style line
		" preceded by a stated line. If there is no such structure then we
		" just have to find two 'normal' lines following each other with the
		" same indentation and with the first of these two lines terminated by
		" a ; or by a }...
		
		while 1
			" let's skip '{}' blocks
			if previous_line =~ '^\s*}'
				" find the openning '{'
				let last_line_num = FindOpenBracket(last_line_num)

				" if the '{' is alone on the line get the line before
				if getline(last_line_num) =~ '^\s*{'
					let last_line_num = GetLastRealCodeLNum(last_line_num - 1)
				endif

				let previous_line = getline(last_line_num)
				 
				continue
			else
				" At this point we know that the previous_line isn't a closing
				" '}' so we can check if we really are in such a structure.

				" it's not a '}' but it could be an else alone...
				if getline(last_line_num) =~# '^\s*else\(if\)\=\>'
					let last_line_num = FindTheIfOfAnElse(last_line_num, 0)
					continue " re-run the loop (we could find a '}' again)
				endif

				" So now it's ok we can check :-)
				" A good quality is to have confidence in oneself so to know
				" if yes or no we are in that struct lets test the ident of
				" last_line_num and of last_line_num - 1!
				" If those are == then we are almost done.
				"
				" That isn't sufficient, we need to test how the first of the
				" 2 lines is ended...
			
				" Note the indenting of the line we are checking

				let last_match = last_line_num " remember the top line we found so far
					
				let one_ahead_indent = indent(last_line_num)
				let last_line_num = GetLastRealCodeLNum(last_line_num - 1)
				let two_ahead_indent = indent(last_line_num)
				let after_previous_line = previous_line
				let previous_line = getline(last_line_num)
			
				
				" If we find a '{' or a case/default then we are inside that block so lets
				" indent properly... Like the line following that '{'
				if previous_line =~ '^\s*\(case\|default\).*:\|{'.endline
					break
				endif

				" The 3 lines below are not necessary for the script to work
				" but it makes it work a little more faster in some (rare) cases.
				" We verify if we are at the top of a non '{}' struct.
				if after_previous_line=~# '^\s*'.s:blockstart.'.*)'.endline && previous_line =~# '[;}]'.endline
					break
				endif
				
				if one_ahead_indent == two_ahead_indent || last_line_num < 1 
					" So the previous line and the line before are at the same
					" col. Now we just have to check if the line before is a ;$ or [}]$ ended line
					" we always check the most ahead line of the 2 lines so
					" it's useless to match ')$' since the lines couldn't have
					" the same indent...
					if previous_line =~# '[;}]'.endline || last_line_num < 1
						break
					endif
				endif
			endif
		endwhile

		if indent(last_match) != ind " if nothing was done lets the old script continue
			let ind = indent(last_match) " let's use the indent of the last line matched by the alhorithm above
			let b:PHP_CurrentIndentLevel = b:PHP_default_indenting "line added in version 1.02 to prevent optimized mode
										 " from acting in some special cases
			return ind
		endif
	endif


	let plinnum = GetLastRealCodeLNum(lnum - 1)
	let pline = getline(plinnum) " previous to last line

	" REMOVE comments at end of line before treatment
	" the first par of the regex removes // from the end of line when they are
	" followed by a number of '"' which is a multiple of 2. The second part
	" removes // that are not followed by any '"'
	" Sorry for this unreadable thing...
	 let last_line = substitute(last_line,"\\(//\\|#\\)\\(\\(\\([^\"']*\\([\"']\\)[^\"']*\\5\\)\\+[^\"']*$\\)\\|\\([^\"']*$\\)\\)",'','')

	" Indent blocks enclosed by {} or () (default indenting)
	if !LastLineClosed " the last line isn't an .*; or a }$ line
		" if the last line is a [{(]$ or a multiline function call (or array
		" declaration) with
		" already one parameter on the opening ( line
		if last_line =~# '[{(]'.endline || last_line =~? '[a-z_]\w*\s*(.*,$' && pline !~ '[,(]'.endline
			let ind = ind + &sw
			if cline !~# '^\s*\(default\|case\).*:' " case and default are not indented inside blocks
				let b:PHP_CurrentIndentLevel = ind
				return ind
			endif
		endif
	endif

	if cline =~  '^\s*);\='
		let ind = ind - &sw
	elseif cline =~# '^\s*\(default\|case\).*:'
		let ind = ind - &sw
	endif

	if last_line =~# '^\s*\(default\|case\).*:'
		let ind = ind + &sw
	endif

	let b:PHP_CurrentIndentLevel = ind
	return ind
endfunction

" vim: set ts=4 sw=4:
