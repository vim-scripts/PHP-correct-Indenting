" Vim indent file
" Language:	PHP
" Author:	John wellesz <John.wellesz (AT) teaser (DOT) fr> inspired from the original version
"  by Miles Lott <milos (AT) groupwhere (DOT) org>
" URL:		http://www.2072productions.com/vim/indent/php.vim
" Last Change:	2004 December 16th
" Version: 1.05
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
" Changes: improvments with regard to the original version (0.5) by Miles Lott:
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
" Changes: (previous versions by Miles Lott)
"
"		   0.5 - fix duplicate indent on open tag, and empty bracketed
"          statements.
"          0.4 - Fixes for closing php tag, switch statement closure, and php_indent_shortopentags
"          option from Steffen Bruentjen <vim (AT) kontraphon (DOT) de>
"
"
"
"  If you find a bug, please e-mail me at John.wellesz (AT) teaser (DOT) fr
"  with an example of code that break the algorithm.
"
"
"	Thanks a lot for using this script.
"


" Only load this indent file when no other was loaded. But reset those state
" variables if needed

let s:lastindented = 0
let s:indentbeforelast = 0
let s:indentinghuge = 0
let s:CurrentIndentLevel = 0
let s:LastIndentedWasComment = 0

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
"		SEVERAL LINES (maybe include && || as starters too...)
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

	" Let's detect if we are indenting just one line or more than 3 lines
	" in the last case we can slightly optimize our algorithm
	if !s:indentinghuge && s:lastindented > s:indentbeforelast 
		if s:indentbeforelast
			let s:indentinghuge = 1
			echom 'Large indenting detected, speed optimizations engaged'
		endif
		let s:indentbeforelast = s:lastindented
	endif

	" If the line we are indenting isn't directly under the previous non-blank
	" line of the file then deactivate the optimization procedure
	if s:indentinghuge && prevnonblank(v:lnum - 1) != s:lastindented
		echom 'Large indenting deactivated'
		let s:indentinghuge = 0
		let s:indentbeforelast = 0
		let s:lastindented = 0
		let s:CurrentIndentLevel = 0
		let s:LastIndentedWasComment=0
	elseif v:lnum > s:lastindented
		let s:lastindented = v:lnum
	endif

	let s:level = 0

	let cline = getline(v:lnum) " current line

	if s:indentinghuge && cline =~'^\s*//'
		if s:LastIndentedWasComment==1
			return indent(s:lastindented) "  line replaced in 1.02
		endif
		let s:LastIndentedWasComment=1
	else
		let s:LastIndentedWasComment=0
	endif

	" Find an executable php code line above the current line.
	let lnum = GetLastRealCodeLNum(v:lnum - 1)

	" Hit the start of the file, use zero indent.
	if lnum == 0
		return 0
	endif
	let cline = getline(v:lnum) " current line
	let last_line = getline(lnum)    " last line
	let ind = indent(lnum) " by default
	let endline= s:endline

	" PHP start tags are always at col 1, useless to indent unless the end tag
	" is on the same line
	if cline =~# '^\s*<?\(php\)\='  && cline !~ '?>' " Added the ^\s* part in version 1.03
		return 0
	endif

	" PHP end tags are always at col 1, useless to indent unless if it's
	" followed by a start tag on the same line
	if  cline =~ '^\s*?>'  && cline !~# '<?\(php\)\='  
		return 0
	endif

	" if the last line is a stated line and it's not indented then why should
	" we indent this one??
	" if optimized mode active and nor current or previous line are an 'else'
	" or the end of a possible bracketless thing then indent the same as the previous
	" line
	if last_line =~ '[;}]'.endline
		if ind==0
			return 0
		elseif s:indentinghuge && ind==s:CurrentIndentLevel && getline(GetLastRealCodeLNum(lnum - 1))=~';'.endline && cline !~# '^\s*\(else\|[})];\=\)' && last_line !~# '^\s*\(\(}\s*\)\=else\)'
			return s:CurrentIndentLevel
		endif
		"elseif ind==0  " Lines inverted in version 1.03
		"	return 0
		"endif
	endif

	" Search the matching open bracket (with searchpair()) and set the indent of cline
	" to the indent of the matching line.
	if cline =~ '^\s*}\(}}\)\@!'
		let ind = indent(FindOpenBracket(v:lnum))
		" let s:CurrentIndentLevel = ind " removed in 1.02
		let s:CurrentIndentLevel = 0 " added in 1.02, optimized mode could do bad things in some cases
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
	if ind && cline =~# '^\s*else\(if\)\=\>'
		let s:CurrentIndentLevel = 0 "line added in version 1.02 to prevent optimized mode from acting in some special cases
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
	elseif ind && last_line =~ '\(^\s*)\)\@<!;'.endline.'\|^\s*}\(.*{'. endline.'\)\@!'
		" If we are here it means that the previous line is:
		" - not a [beginning-blanck]);$ line but a *;$ line
		" - a [beginning-blanck]} followed by anything but a {$
		let previous_line = last_line
		let last_line_num = lnum
		let LastLineClosed = 1

		" The idea here is to check if the current line is after a non '{}'
		" structure so we can indent it like the top of that structure.
		" The top of that structure is caracterized by a if (ff)$ style line
		" preceded by a stated line. If there is no such structure then we
		" just have to find two 'normal' lines following each other with the
		" same indentation...
		
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
				" If those are == then we are done.
				"
				" But that isn't sufficient, we need to test how those lines
				" are ended...
			
				" Note the indenting of the line we are checking
				let last_match = last_line_num
					
				let one_ahead_indent = indent(last_line_num)
				let last_line_num = GetLastRealCodeLNum(last_line_num - 1)
				let two_ahead_indent = indent(last_line_num)
				let after_previous_line = previous_line
				let previous_line = getline(last_line_num)
				
				" The 3 lines below were the 'optimization' at the origin of the bug of version 1.03
				"if two_ahead_indent==0
				"	let last_match=0 " a non-sense, last_match is a line number...
				"endif
				
				" If we find a '{' then we are inside that block so lets
				" indent properly... Like the line following that '{'
				if previous_line =~ '{'.endline
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
					" col. Now we just have to check if the line before is a ;$ or [})]$ ended line but not a ^);$ one
					if previous_line =~# '\(\(\(^\s*)\)\@<!;\)\|[})]\)'.endline || last_line_num < 1
						break
					endif
				endif
			endif
		endwhile

		if indent(last_match) != ind " if nothing was done lets the old script continue
			let ind = indent(last_match) " return to previous level
			let s:CurrentIndentLevel = 0 " line added in version 1.02 to prevent optimized mode
										 " from acting in some special cases
			"let s:CurrentIndentLevel = ind " line added in 1.01 and removed in 1.02 - wrong solution
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
	" echo last_line
	" Indent blocks enclosed by {} or () or case statements (default
	" indenting)
	if LastLineClosed==0 && last_line =~# '\([{(]\|case.*:\)'.endline || LastLineClosed==0 && pline !~ '[,(]'.endline && last_line =~? '[a-z_]\w*\s*(.*,$'
		let ind = ind + &sw
		" return if the current line is not another case statement of the previous line is a bracket open
		if  LastLineClosed==0 && last_line =~ '[{(]'.endline || cline !~# '.*case.*:\|default:'
			let s:CurrentIndentLevel = ind
			return ind
		endif
	endif
	if cline =~# '^\s*);\=\|^\s*case.*:\|^\s*default:'
		let ind = ind - &sw
		" if the last line is a break or return, or the current line is a close bracket,
		" or if the previous line is a default statement, subtract another
		if pline =~ '^\s*default:' && last_line =~# '^\s*break;\|^\s*return' && cline =~ '^\s*);\='
			let ind = ind - &sw
		endif
	endif

	if last_line =~# '^\s*default:'
		let ind = ind + &sw
	endif

	let s:CurrentIndentLevel = ind
	return ind
endfunction

" vim: set ts=4 sw=4:
