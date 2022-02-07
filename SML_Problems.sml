(* Greg Griffin *)
(* Solutions to 3 sets of problems from my Programming Languages class. *)
(* Run the tester file to verify that these all work. *)






(* Set 1 *)

(* 
#1 Define a function called pow (val pow = fn : int * int -> int) that takes two integers as 
arguments and that returns the result of raising the first integer to the power of the second.
 You may assume that the power is not negative. For our purposes, we will assume that every 
 integer to the 0 power is 1 (this isn't true of 00, but that's okay).

#2 Define a function called sumTo (val sumTo = fn : int -> real) that accepts an integer n and that
 computes the sum of the first n reciprocals. For example, sumTo(3) should return 
 (1 + 1/2 + 1/3) = 1.83333333.... The function should return 0.0 if n is 0. You may assume that 
 the function is not passed a negative value of n.

#3 Define a function called repeat (val repeat = fn : string * int -> string) that takes a string 
and an integer (>= 0) as arguments and that returns a string composed of the given number of 
occurrences of the string. For example, repeat("hello", 3) returns "hellohellohello", 
repeat("hello", 0) returns "". You may assume that the function is not passed a negative value 
for the second argument. 

#4 Define a function called binary  (val binary = fn: int -> string) that takes an integer n as an 
argument and returns a string corresponding to the 16-bit binary representation of  that integer. 
For example, binary 17 returns "0000000000010001". You need not account for numbers whose binary 
representation requires more than 16 bits. Note: If needed, you can use the Int.toString function 
to convert integers to corresponding strings. You may find it helpful to write a helper function 
for this problem.

#5 Define a function called countNegative (val countNegative = fn : int list -> int) that takes a list 
of integers as an argument and that returns a count of the number of negative integers in the list.
 For example, countNegative([3,17,~9,34,~7,2]) should return 2.

#6 Define a function called absList (val absList = fn : (int * int) list -> (int * int) list) that takes
 a list of int * int tuples and that returns a new list of int * int tuples where every integer is 
 replaced by its absolute value. For example, absList([(~38,47), (983,~14), (~17,~92), (0,34)]) 
 should return [(38,47), (983,14), (17,92), (0,34)]. HINT: This is easier to solve if you write a 
 helper function to process one tuple.

#7 Define a function called split (val split = fn : int list -> (int * int) list)  that takes a list 
of integers as an argument and that returns a list of the tuples obtained by splitting each integer 
in the list. Each integer should be split into a pair of integers whose sum equals the integer and 
which are each half of the original. For odd numbers, the second value should be one higher than the 
first. For example, split([5,6,8,17,93,0]) should return [(2,3), (3,3), (4,4), (8,9), (46,47), (0,0)].
 You may assume that all of the integers in the list passed to the function are greater than or equal
  to 0.

#8 Define a function called isSorted (val isSorted = fn : int list -> bool) that takes a list of integers
 and that returns whether or not the list is in sorted (nondecreasing) order (true if it is, false if
  it is not). By definition, the empty list and a list of one element are considered to be sorted.

#9 Define a function called collapse (val collapse = fn : int list -> int list) that takes a list of 
integers as an argument and that returns the list obtained by collapsing successive pairs in the 
original list by replacing each pair with its sum. For example, collapse([1,3,5,19,7,4]) should return
 [4,24,11] because the first pair (1 and 3) is collapsed into its sum (4), the second pair (5 and 19)
  is collapsed into its sum (24) and the third pair (7 and 4) is collapsed into its sum (11).If the 
  list has an odd length, the final number in the list is not collapsed. For example, 
  collapse([1,2,3,4,5]) should return [3,7,5].

#10 Define a function called insert (val insert = fn : int * int list -> int list)  that takes an integer
 and a sorted (nondecreasing) integer list as parameters and that returns the list obtained by 
 inserting the integer into the list so as to preserve sorted order. For example, 
 insert(8,[1,3,7,9,22,38]) should return [1,3,7,8,9,22,38].
 
#11 Define a function called decimal (val decimal = fn: string -> int) that takes a bit string 
corresponding to an integer and returns the decimal value of that integer. For example, decimal 
"10001" returns 17, decimal  "001101" returns  13.
 *)

(* #1 - pow *)
(* Also used as a helper function to decimal *)
(* if the exponent is 0 then return 1, which just raises the number to be returned by 1,
keeping it the same. Else, multiply the base number a by itself, repeated recursively, 
decrementing b each time *)
fun pow (a, b) = if b = 0 then 1 else a * pow(a, b-1);



(* #2 - sumTo *)
(* If the input is zero then return 0.0, else return the reciprocal of n plus the reciporical
of itself minus 1 *)
fun sumTo (n:int) = if n = 0 then 0.0 else 1.0 / (real n) + sumTo(n - 1);
               
               

(* #3 - repeat *)
(* if n is 0 then return an empty string, otherwise concatenate the given string s 
with itself, decrementing n each time to produce the correct output *)
fun repeat (s, n) = if n = 0 then "" else s ^ repeat(s, n - 1);



(* #4 - binary *)
(* Helper fucntion to binary *)
(* Takes an integer and converts it to a string list of the reversed binary representation
of that int. If x is 0 then return an empty list, else cons str(x mod 2) at the head of the list, 
dividing x by 2 at each recursive call. *)
fun convert x = if x = 0 then nil else Int.toString(x mod 2) :: convert (x div 2);

(* Helper function to binary *)
(* List reversal function from lecture notes, modified to convert a string list to 
a string of its reversed version: if the list is empty then return an empty string, else
recursively concatenate the head string item to the string of the rest of the list. *)
fun rever x = if null x then "" else rever (tl x) ^ hd x;

(* Helper function to addZeroes *)
(* List length function from lecture notes: used as a helper function for many problems
if the list is empty return 0, else add 1 to the recursive length of the list minus its head *)
fun lengthOf x = if null x then 0 else 1 + lengthOf (tl x);

(* Helper function to binary *)
(* If the length of the list is 16 then return the list because it is the 16 bit representation,
else add a 0 to the end of the list recursively *)
fun addZeroes x = if lengthOf(x) = 16 then x else addZeroes( x @ ["0"]);

(* Converts an integer to a string of its binary representation by calling 3 helper functions. 
Converts the passed integer into a string list of its reversed binary representation, then 
adds zeroes to the end of the list until its full reversed 16 bit representation is returned, 
then reverses that string list and converts it to a string. *)
fun binary (x:int) = rever(addZeroes(convert(x)));



(* #5 - countNegative *)
(* Helper function to countNegative and processTuple *)
(* Takes an integer and returns 1 if it is a negative integer, 0 if positive. *)
fun testNeg x = if x + 1 <= 0 then 1 else 0;

(* If the list is empty return 0 since there are no more elements to count, else pass the head
of the list to testNeg to start a counter, and add that to a recursive call of the function minus
the head. *)
fun countNegative x = if null x then 0 else testNeg(hd x) + countNegative(tl x);



(* #6 - absList *)
(* Helper function to processTuple *)
(* If the value is a negative then return its abs val, else return itself *)
fun absVal x = if testNeg(x) = 1 then ~x else x;

(* Helper funtion to absList *)
(* Takes a tuple of 2 ints and returns a tuple of each's absolute value by calling absVal *)
fun processTuple (x, y) = (absVal(x), absVal(y));

(* If the list is empty return an empty list, else call processTuple on the head of the 
list, and cons that to a recursive call of absList minus the head *)
fun absList x = if null x then nil else processTuple(hd x) :: absList(tl x);



(* #7 - split *)
(* Helper function to split *)
(* Takes an int, and simply returns a tuple with x divided by 2, and x divided by 2 plus its 
remainder. Used for readability *)
fun splitInt x = (x div 2, x div 2 + x mod 2);

(* If the list is empty return an empty list, else call splitInt on the head of the list and 
cons that to the recursive call of split of the list minus its head *)
fun split x = if null x then nil else splitInt(hd x) :: split(tl x);



(* #8 - isSorted *)
(* If the length of the passed list is 1 or 0 then the list is sorted, else see if the 
first item in the list is less than or equal to the second item, and AND that boolean with
the boolean of the recursive call of the function of the list minus its head *)
fun isSorted x = if lengthOf x <= 1 then true else hd x <= hd(tl x) andalso isSorted(tl x);


 
(* #9 - collapse *) 
(* If the length of the list is 1 or 0 then return the list, be it empty or the last element,
else add the first element to the second, and cons that to the recursive call of the function
of the list with the first two elements (that have been collapsed already) removed. *)
fun collapse x = if lengthOf(x) = 1 orelse lengthOf x = 0 
				then x else hd x + hd(tl x) :: collapse(tl(tl x));



(* #10 - insert *) 
(* If the list is empty OR n is <= the head of the passed list then cons n to the list. 
Else, cons the head of the list to the recursive call of insert on the list minus its head.
This function traverses the list and keeps its elements in the same place while doing so. *)       
fun insert (n, x) = if null x orelse (n <= hd x) then n::x else hd(x)::insert(n, tl(x));



(* #11 - decimal *)
(* Helper function to decimal *)
(* Takes a char list passed from decimal. If the integer value of the first char in the list
is equal to the char representation of 1, then return 2 raised to the length of the char list
minus 1, which ends up being the distance from the last element in the char list, so it is the 
correct power to raise 2 by to calculate the binary representation of the current 1 bit. Else,
the first element in the char list is a zero so return 0 *)
fun converter x = if ord(hd x) = 49 then pow(2,(lengthOf(x)-1) ) else 0;

(* If the length of the char list of the passed string is zero then return 0, else call converter
on the char list of the string, and add its result to the recursive call of decimal minus the 
head of the string. 
So, this function along with its helper calculates the decimal value of head of the string and 
recursively adds that to the decimal representation of the next string element. *)
fun decimal (s:string) = if lengthOf(explode(s)) = 0 then 0 else converter(explode(s)) + 
							decimal(implode(tl(explode(s))));


(* Set 2 *)

(* 
#1 Follow the algorithm outlined in your textbook on page 116. The code 
follows a pattern similar to that of the mergesort function discussed in class. 
A helper function named partition does most of the work. Write your own partition 
function. You may not use any functions in any libraries (for example List.partition):

	Write a quicksort function of type int list -> int list. Here' s a review of the quicksort 
	algorithm.First pick an element and call it the pivot. (The head of the list is an easy
	choice for the pivot.) Partition the rest of the list into two subsists, one with all 
	the elements less than the pivot and another with all the elements not less than the pivot.
	Recursively sort the sublists. Combing tile two sublists (and thepivot) into a final sorted list.

#2 Once again, write your own function from scratch. Do not use any 
membership testing functions in any libraries:

	Write a function to test whether an element is a member of a set

#3 Again, write your own function from scratch. Do not use any union 
computing functions in any libraries.

	Write a function to construct the union of two sets

#4 Again, write your own function from scratch. Do not use any 
intersection computing functions in any libraries.

	Write a function to construct the intersection of two sets

#5 A range function similar to the range function in Python. For example, range (2,12,3)
returns the list [2,5,8,11]

#6 A slice function, with functionality similar to the Python list slice operator. 
For example, slice ([11,22,3,14,5,6], 1, 4) returns the list [22,3,14]. Assume that 
the indices specified are valid for the list. Hint: Write a helper function nthElement
hat returns the element at a specified (0-based) index in a list. For example, 
nthElement([10, 2, 34, 20], 2) returns 34, the element at index 2.

#7 A binarySearch function that recursively implements the binary search algorithm 
to search a sorted integer list for a specified integer and returns true if it is 
found, false otherwise.  For example, binarySearch ([100,200,300,400,500], 200) 
returns true, whereas binarySearch([100,200,300,400,500], 299) returns false. 
	Hint: Write a helper function mid that returns a tuple (index, value) representing
	the middle value in a list. For example, mid [10, 2, 40, 8, 22] returns (2,40) 
	because the value 40 at index 2 is the middle value in the list. Similarly,
	mid [10, 20] would return (1, 20). Use mid in conjunction with slice to 
	implement binarySearch. *)


(* #1 - quicksort *)
(* Helper function to quicksort *)
(* Takes a list and a pivot and returns the elements in the list that are less than the pivot.
If nil then nil. If list has one element then only return the element if it is less than the pivot.
Otherwise, split the list into head and tail. If the head >= the pivot, then do nothing, 
recursively calling on the tail, skipping the previous element. Otherwise, the head is of importance,
so cons it to the recursive call on the tail *)
fun lessThan(nil, p) = nil
	| lessThan([s], p) = if s < p then [s] else nil
	| lessThan(head::tail, p) = if head >= p then lessThan(tail, p) else head::lessThan(tail, p);

(* Helper function to quicksort. *)
(* Written the same as lessThan, but instead returns a list with items greater than or
equal to the pivot.*)
fun greaterThan(nil, p) = nil
	| greaterThan([s], p) = if s >= p then [s] else nil
	| greaterThan(head::tail, p) = if head < p then greaterThan(tail, p) 
																	else head::greaterThan(tail, p)

(* Quicksort. Takes an int list. If nil then nil. If only one element then return it. Otherwise,
the list has two or more elements, so split them up into head and tail. Using head as the pivot, 
call quicksort on the lists of elements < the pivot (in the tail), and also the list of 
	elements >= the pivot (in the tail). Then, add those two lists, putting the pivot between them.   *)
fun quicksort nil = nil
	| quicksort [x]:int list = [x]
	| quicksort (pivot::rest) = 
				quicksort(lessThan(rest, pivot)) @ pivot::quicksort(greaterThan(rest, pivot));



(* #2 - member *)
(* If the set is empty then false. If the set has one element and that element
 is e then true. If the set has 2 or more elements, then if e is the head and
 not in the rest of the set then true. Else keep looking for e in the tail of
 the set. *)
fun member (e, nil) = false
	| member (e, [s]) = if e = s then true else false
	| member (e, head::tail) = if head = e andalso member(e, tail) = false then true
	else member(e, tail);


               
(* #3 - returns the union of sets (lists) s1 and s2 *)
(* You may assume that s1 and s2 do not have any duplicate elements *)

(* Helper function to union, removeDupes, and intersection *)
(* Takes a list and an item, and returns true if that item is in the list somewhere. IF nil then 
the item is not in the list. If the list has one item then true if that item is the item we're looking
for. Otherwise, the list has two or more elements, so split the list into head and tail. 
If the head = item then true, else call dupeChecker on the tail. *)
fun dupeChecker (nil, item) = false
	| dupeChecker ([x], item) = if item = x then true else false
	| dupeChecker(head::tail, item) = if head = item then true else dupeChecker(tail, item);

(* Helper function to union *)
(* Takes a list and removes duplicate items from it. If nil then nil, base case. IF only one item 
return it. Otherwise, split list into head and tail. Call dupechecker on the tail, looking for the 
head in it. If head is found in the list tail then skip over the head by doing a recursive call on
tail. Otherwise, head was not found in tail, so cons head to the recursive call. This builds a new list
with no duplicates.  *)
fun removeDupes nil = nil
	| removeDupes [x] = [x]
	| removeDupes (head::tail) = if dupeChecker(tail, head) = true then removeDupes(tail)
 																	else head::removeDupes(tail);
(* Returns the union of the two passed sets, without duplicates, in sorted order. Base case: if nil
then nil. Otherwise, combine the two sets into a new set, then remove all of the duplicates. The
way that removeDupes loops through the sets maintains returns a sorted set if the first set is
sorted.  *)
fun union (nil, nil) = nil
	| union (set1, set2) = removeDupes(set1 @ set2);



(* #4 - returns the intersection of sets (lists) s1 and s2 *)
(* You may assume that s1 and s2 do not have any duplicate elements *)
(* Returns the set of elements that are both in set 1 and 2. Base case: 2 nil sets, return nil,
or one nil list and one regular list, return the non empty list. 
If both sets have only one element, see if the elements are the same and act accordingly. If
one set has one element but the other has more, use dupeChecker from before to see if the element
is anywhere in the set, if so return that element, else nil. 
	Otherwise, both sets have 2 or more elements, so split the first set into head and tail. If 
	the head of set1 is not in set2 then skip that head by recursive calling on the tail. Else, 
	the head was in set2, so cons it to the recursive call on set1's tail. This creates a new list
	with only the duplicated values.  *)
fun intersection (nil, nil) = nil
	| intersection (nil, x) = x
	| intersection ([x], [y]) = if x = y then [x] else nil
	| intersection ([x], s2) = if dupeChecker(s2, x) = true then [x] else nil
	| intersection (s2, [y]) = if dupeChecker(s2, y) = true then [y] else nil

	| intersection (head::tail, s2) = if dupeChecker(s2, head) = false 
		then intersection(tail, s2) else head::intersection(tail, s2);



(* #5 - Return list of integers from start (inclusive) to stop (exclusive) by step *)
(* First check if the step is negative, if it is then if start is less than the 
stop - 1 then return empty list. Else, to a list containing the original start, add
to it the list returned by the recursive call of range() with the start decremented
by the step. 
Otherwise, the step is positive, so do the exact same thing, except increment the 
start by the step for each recursive call *)
fun range(start, stop, step) = 
if 
	step < 0 then if start < (stop + 1) then nil 
else
	[start] @ range((start + step), stop, step)
else if start > (stop - 1) then nil 
else 
	[start] @ range((start + step), stop, step);



(* #6 - Return a slice of a list between indices start inclusive, and stop exclusive.
Assume first element of list is at index 0 *)

(* Helper function to slice. *)
(* Base case: if passed a nil list and any index then nil. Else, the list has 1+ elements in it, so
if the index is >= the length of the list return nil, else if the index is 0 return the head of the 
list, otherwise recursive call on the tail of the list, and decrementing index *)
fun nthElement(nil, _) = nil
	| nthElement(l, 0) = [hd l]
	| nthElement(l, idx) = if length(l) <= idx then nil else nthElement(tl(l), (idx - 1));

(* Base cases: If nil list then nil. If passed a list with only one element, return the 
nthElement of that list at the index start. This returns the element if start is 0, and nil
otherwise. 
Else, the list has at at least 2 elements. Recursively go through the list, calling slice on the
tail, decrementing start and stop each time until stop = 1. If stop = 1, then the head will be 
the last element that we need, so return it, ending the function. Else, if we have already
decremented start to 0, that means that we have found or passed the element at the start index,
so cons the head to the recursive call on the tail, with only stop decremented. 
	Otherwise, we have not yet come across the element at the start index, so check to see if the 
	head of the list is = to the nthElement at the index start. If it is, then cons the head to
	the recursive call of slice on the tail, with both start and stop decremented, preserving
	the proper indicies of the elements we want.
Else, finally, head is not the element at start, so skip over that element and continue searching
	for it by recursively calling slice on the tail with start and stop each decremented by 1 *)
fun slice(nil, start, stop) = nil
	| slice([x], start, stop) = nthElement([x], start)
	| slice(head::tail, start, 1) = [head]
	| slice(head::tail, 0, stop) = head::slice(tail, 0, stop - 1)
	| slice(head::tail, start, stop) = 
										if [head] = nthElement(head::tail, start) 
										then head::slice(tail, start - 1, stop - 1)
										else slice(tail, start - 1, stop - 1);



(* #7 - binary search *)
(* Helper function to binarySearch *)
(* If the passed list is empty then raise empty. Else if the list only has one element, then return
a tuple of index 0 and the sole element. Else, the passed list has multiple elements, so, return
a tuple of the index of the middle element, which is the length of the list / 2, and the
element at that index, for which we can use nthElement at that index. nthElement returns a list so 
we get the head of its result. *)
fun mid(nil) = raise Empty
	| mid([x]) = (0, x)
	| mid(aList) = 
		let
			val midIndex = length(aList) div 2
		in
			(midIndex, hd(nthElement(aList, midIndex)))
		end;

(* Base case: if passed an empty list then false. If the list only has one element then return
true if it is the one we are looking for. Otherwise, the passed list has multiple elements. 
If the middle value in the list is less than the one we are looking for, then discard the lower
half of the list and recursive call binarySearch on the top half. Else if midVal > value, do the
same but to the bottom half of the list. Else, the midVal is the value we are looking for, so return
true.  *)
fun binarySearch(nil, value) = false
	| binarySearch([x], value) = if x = value then true else false
	| binarySearch(sortedList, value) = 
		let
			val startIDX = 0
			val middleIDX = #1(mid(sortedList))
			val lastIDX = length(sortedList)
			val midVal = #2(mid(sortedList))

		in
			if midVal < value 
			then binarySearch(slice(sortedList, middleIDX, lastIDX), value)
			else if midVal > value
			then binarySearch(slice(sortedList, startIDX, middleIDX), value)
			else true

		end;



(* Set 3 *)

(* 

Functions to implement :
Problems 10, 11, 12, 15, 16, 23, 24, 28 (from Chapter 9 of your textbook). To receive credit, 
your implementation must adhere to the following requirements:
The first 7 exercises above must have one-line solutions expressed using map, foldl, foldr 
exclusively. You can use other predefined functions if needed, but do not write any additional 
NAMED functions, and do not use explicit recursion.

Solve Exercise 28 as a curried function without using map, foldr, or foldl.


#1 Write a function dupList of type a list -> 'a list whose output list is the same as the input
list, but with each element of the input list
repeated twice in a row. For example, if the input list is [1,3,2] , the output list should 
be[1,1,3,3,2,2], If the input list is [],the output list should be [],

#2 Write a function mylength of type ' a list -> int that returns the length of a list. 
(Of course, you may not use the predefined length function to
do it)

#3 Write a function illabsrl of type int list -> real list that takes a list of integers and returns
 a list containing the absolute values of those integers, converted to real numbers.

#4 Write a function myimplode that works just like the predefined implode. in other words,
it should be a function of type char list -> string that lakes a list of characters and returns 
the string containing those same characters in that same order

#5 Write a function lconcat of type 'a list list -> 'a list that takes a list of lists as 
Input and returns the list formed by appending the input lists together inorder. For example,
if the input is [[1,2],[3,4,5,6],[7]] your function should return [1, 2, 3, 4, 5, 6, 7].
(There is a predefined function lakethas called concat, which of course you should not use.)

#6 Write a function convert of type ('a * 'b) list -> 'a list * 'b list ,
that converts a list of pairs into a pair of lists,preserving the order of the elements, 
For example, convert [(1,2),(3,4),(5,6)] should evaluate to ([1,3,5],[2,4,6]).

#7 Define a function mymap with the same type and behavior as map, but without using map.
 (Note this should still be a one-liner: use foldl or foldr .)

#8 Define a function myfoldl with the same type and behavior as foldl.

#9 A curried implementation (without using map, foldr, or foldl) of a function sumSome with the 
following signature: val it = fn : (int -> bool) -> int list -> int that accepts a function, and 
a list of integers, and returns the sum of all values in the list for which the specified function 
returns true. Once you define sumSome, the following functions must work as described.  
These functions are tested in the test file:
val sumAll = sumSome (fn x => true); ( sum all the numbers in a list )
val sumEvens = sumSome (fn x => x mod 2 = 0); ( sum all the even numbers in a list )
val sumOdds = sumSome (fn x => x mod 2 = 1); ( sum all the odd numbers in a list 

(* #1 - duplist *)
(* First maps each element in x to a list in the form [x,x]. Then, 
foldr comes in and concetenates each [x,x] to themselves to produce the correct list*)
fun duplist x = foldr (op @) [] (map (fn y => [y,y]) x);

(* #2 - mylength *)
(* Call foldr, passed (_,a), 0, and x. Where _ is any list to be folded, a is the 
recursive result of foldr on the rest of the list, 0 is the base case from which 
to begin counting, and x is the passed list. So, until we reach the base case 
of a nil list, a is saved through foldr's recursion and incremented by 1 
	We can pass _ (any list) to foldr because its implementation handles empty
	lists (as the base case) *)
fun mylength x = foldr(fn (_, a) => a + 1) 0 x;

(* #3 - il2absrl *)
(* Applies real() and abs() to each element in list x. *)
fun il2absrl x = map (fn y => abs(real(y))) x;

(* #4 - myimplode *)
(* Works the same as duplist. Maps each element to a string using str(). Then
folds that list into a string using the string concatenation operator and an empty
string as a base case.  *)
fun myimplode x = foldr (op ^) "" (map (fn y => str(y)) x);

(* #5 - lconcat *)
(* Just folds the list with the list concatenation operator with nil as base case. 
The way that foldr recurses makes it the right choice.  *)
fun lconcat x = foldr (op @) [] x;

(* #6 - convert *)
(* Returns a tuple of the results of two foldr+map operations on the list x. 
The first one maps each tuple in the list to single lists of just their first
elements. Then foldr concatenates those lists. The same thing repeats, but this time
grabbing and folding the second element in the tuples.  *)
fun convert x = ((foldr (op @) [] (map (fn (x,y) => [x]) x)),
				 (foldr (op @) [] (map (fn (x,y) => [y]) x)));

(* #7 - mymap *)
(* Folds the list from the right, applying a specified function to the last element, 
and cons-ing the result of that to the recursive call of the function on the 
rest of the list. Nil list as the base case.  *)
fun mymap f x = foldr (fn (y,a) => (f y)::a) [] x


(* #8 - myfoldl *)
(* Base case, if the list is empty then return the initial value, this is what is to
be added to. Otherwise, call foldl recursively with the value returned by the
function on the head being the new inital value, and the tail being the list to fold.
*)
fun myfoldl f initialValue nil = initialValue
	| myfoldl f initialValue (head::tail) = myfoldl f (f(head, initialValue)) tail;


(* #9 - sumSome *)
(* Base case: empty list, return 0 to be added to sum. Otherwise, take the head
and the tail of the list. If the function returns true for the head integer then 
add the head to the recursive sumSome on the tail. Otherwise do not add the head
and sumSome the tail.  *)
fun sumSome (f:int -> bool) nil = 0
	| sumSome (f:int -> bool) (aList:int list) = 
  			let
  				val head:int = hd(aList)
  				val tail = tl(aList)
  			in
  				if f(head) then head + sumSome f tail
  				else sumSome f tail
  			end;




