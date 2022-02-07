(* Tester file for SML_Problems *)

(* After installing smlnj and saving SML_Problems, type sml SML_Tester.sml in the terminal
 to run this program. *)








use "SML_Problems.sml";

(* TESTER FOR PROBLEM SET 1 *)
print("\n\n\nSET 1 TESTS: \n\n");

fun pass_fail x = if x then "Pass" else "\t\tFail";

fun test(function, input, expected_output) = pass_fail ((function(input) = expected_output));

fun error_print (n, f) = print (Int.toString(n)^"."^f^": Error encountered in test of " ^f^"\n");

print("*********TEST RESULTS******************\n");
print("\n1.Pow: "^test(pow, (2,3), 8)^"\n") handle _ => error_print (1, "pow");
print("\n2.Pow: "^test(pow, (~2,3), ~8)^"\n") handle _ => error_print (2, "pow");
print("\n3.Pow: "^test(pow, (2,0), 1)^"\n") handle _ => error_print (3, "pow");
print("\n4.Binary: "^test(binary, 17, "0000000000010001")^"\n") handle _ => error_print(4, "binary");
print("\n5.Binary: "^test(binary, 1023, "0000001111111111")^"\n") handle _ => error_print(5, "binary");
print("\n6.Binary: "^test(binary, 62345, "1111001110001001")^"\n") handle _ => error_print(6, "binary");
print("\n7.1:Repeat: "^test(repeat, ("hello", 3), "hellohellohello")^"\n") handle _ => error_print(7, "repeat");
print("\n7.2:Repeat: "^test(repeat, ("hello", 1), "hello")^"\n") handle _ => error_print(7, "repeat");
print("\n7.3:Repeat: "^test(repeat, ("hello", 0), "")^"\n") handle _ => error_print(7, "repeat");
print("\n8.CountNegative: "^test(countNegative,[3,17,~9,34,~7,2],2)^"\n") handle _ => error_print(8, "countNegative") ;
print("\n9.CountNegative: "^test(countNegative,[3,17,9,34,7,2],0)^"\n") handle _ => error_print(9, "countNegative");
print("\n10.1:absList: "^test(absList,[(~38,47), (983,~14), (~17,~92), (0,34)], [(38,47),(983,14), (17,92), (0,34)])^"\n") handle _ => error_print(10, "absList");
print("\n10.2:absList: "^test(absList,[(~38,~47), (~983,~14)], [(38,47),(983,14)])^"\n") handle _ => error_print(10, "absList");
print("\n11.split: "^test(split,[5,6,8,17,93,0],[(2,3), (3,3), (4,4), (8,9), (46,47), (0,0)])^"\n") handle _ => error_print(11, "split");
print("\n12.split: "^test(split,[5,1,8,17,93,0],[(2,3), (0,1), (4,4), (8,9), (46,47), (0,0)])^"\n") handle _ => error_print(12, "split");
print("\n13.isSorted: "^test(isSorted, [1,2,3,4,4,5,5], true)^"\n") handle _ => error_print(13, "isSorted");
print("\n14.isSorted: "^test(isSorted, [1,2,3,4,3,5,5], false)^"\n") handle _ => error_print(14, "isSorted");
print("\n15.isSorted: "^test(isSorted, [1], true)^"\n") handle _ => error_print(15, "isSorted");
print("\n16.isSorted: "^test(isSorted, [], true)^"\n") handle _ => error_print(16, "isSorted");
print("\n17.isSorted: "^test(isSorted, [1,1,1,1,1], true)^"\n") handle _ => error_print(17, "isSorted");
print("\n18.Collapse: "^test(collapse,[1,3,5,19,7,4],[4,24,11])^"\n") handle _ => error_print(18, "collapse");
print("\n19.Collapse: "^test(collapse,[1,2,3,4,5],[3,7,5])^"\n") handle _ => error_print(19, "collapse");
print("\n20.Collapse: "^test(collapse,[1,2],[3])^"\n") handle _ => error_print(20, "collapse");
print("\n21.Collapse: "^test(collapse,[1],[1])^"\n") handle _ => error_print(21, "collapse");
print("\n22.Insert: "^test(insert,(8,[1,3,7,9,22,38]), [1,3,7,8,9,22,38])^"\n") handle _ => error_print(22, "insert");
print("\n23.Insert: "^test(insert,(88,[1,3,7,9,22,38]), [1,3,7,9,22,38,88])^"\n") handle _ => error_print(23, "insert");
print("\n24.Insert: "^test(insert,(0,[1,3,7,9,22,38]), [0,1,3,7,9,22,38])^"\n") handle _ => error_print(24, "insert");
print("\n25.Insert: "^test(insert,(22,[1,3,7,9,22,38]), [1,3,7,9,22,22,38])^"\n") handle _ => error_print(25, "insert");
print("\n26.1:Decimal: "^test(decimal, "0000000000010001", 17)^"\n") handle _ => error_print(26, "decimal");
print("\n26.2:Decimal: "^test(decimal, "0000001111111111", 1023)^"\n") handle _ => error_print(27, "decimal");
print("\n26.3:Decimal: "^test(decimal, "1111001110001001", 62345)^"\n\n\n\n\n\n\n\n") handle _ => error_print(28, "decimal");


(* TESTER FOR PROBLEM SET 2 *)


print("\n\n\nSET 2 TESTS: \n\n");

(* Sort a list of integers. *)
fun myMergeSort nil = nil
| myMergeSort [e] = [e]
| myMergeSort theList =
     let
         (* From the given list make a pair of lists
         * (x,y), where half the elements of the
         * original are in x and half are in y. *)
         fun halve nil = (nil, nil)
         | halve [a] = ([a], nil)
         | halve (a::b::cs) =
             let
             val (x, y) = halve cs
             in
             (a::x, b::y)
             end; 
         (* Merge two sorted lists of integers into
         * a single sorted list. *)
         fun merge (nil, ys) = ys
         | merge (xs, nil) = xs
         | merge (x::xs, y::ys) =
         if (x < y) then x :: merge(xs, y::ys)
         else y :: merge(x::xs, ys);
         val (x, y) = halve theList
         in
         merge(myMergeSort x, myMergeSort y)
     end;
     
fun pass_fail x = if x then "*Pass*" else "\t\tFail";

fun test(function, input, expected_output) = pass_fail (function(input) = expected_output);

fun test_set(function, input, expected_output) = pass_fail (myMergeSort(function(input)) = expected_output);

print("*********TEST RESULTS******************\n");
print("\n1.Quicksort "^test(quicksort, [1,3,2,5,4,8,7,9,0], [0,1,2,3,4,5,7,8,9])^"\n");
print("\n2.Quicksort "^test(quicksort, [1,3,2,5,4,2, 8,6,7,9,0], [0,1,2,2,3,4,5,6,7,8,9])^"\n");
print("\n3.Member "^test(member, (2,[1,3,2,5,4,2, 8,6,7,9,0]), true)^"\n");
print("\n4.Member "^test(member, (12,[1,3,2,5,4,2, 8,6,7,9,0]), false)^"\n");
print("\n5.Union "^test_set(union, ([1,3,2,5], [0,1,2,33]),[0,1,2,3,5,33])^"\n");
print("\n6.Union "^test_set(union, ([1,3,2,5], [5,2,3,1]),[1,2,3,5])^"\n");
print("\n7.Union "^test_set(union, ([1,3,2,5], []),[1,2,3,5])^"\n");
print("\n8.intersection "^test_set(intersection, ([1,3,22,5], [5,12,3,1]),[1,3,5])^"\n");
print("\n9.intersection "^test_set(intersection, ([1,3,2,5], [15,12,13,11]),[])^"\n");
print("\n10.intersection "^test_set(intersection, ([1,3,2,5], [5,2,3,1]),[1,2,3,5])^"\n");
print("\n11.Range "^test(range, (2,12,3), [2,5,8,11])^"\n");
print("\n12.Range "^test(range, (12,34,5), [12,17,22,27,32])^"\n");
print("\n13.Range "^test(range, (10,2,~1), [10,9,8,7,6,5,4,3])^"\n");
print("\n14.Slice "^test(slice, (range(2,20,2),2,7), [6,8,10,12,14])^"\n");
print("\n15.Slice "^test(slice, (range(1,100,4),3,12), [13, 17, 21, 25, 29, 33, 37, 41, 45])^"\n");
print("\n16.Binary Search "^test(binarySearch, ([1,3,21,52,54,61, 80,90,100],52), true)^"\n");
print("\n17.Binary Search "^test(binarySearch, ([1,3,21,52,54,61, 80,90,100],100), true)^"\n");
print("\n18.Binary Search "^test(binarySearch, ([1,3,21,52,54,61, 80,90,100],1), true)^"\n");
print("\n19.Binary Search "^test(binarySearch, ([1,3,21,52,54,61, 80,90],52), true)^"\n");
print("\n20.Binary Search "^test(binarySearch, ([1,3,21,52,54,61, 80,90],53), false)^"\n");
print("\n21.Binary Search "^test(binarySearch, ([1],1), true)^"\n\n\n\n\n\n\n");


(* TESTER FOR PROBLEM SET 3 *)



print("\n\n\nSET 3 TESTS: \n\n");

(* These definitions use the function sumSome *)
val sumAll = sumSome (fn x => true);
val sumEvens = sumSome (fn x => x mod 2 = 0);
val sumOdds = sumSome (fn x => x mod 2 = 1);

fun match(nil,nil) = true
|match(nil, _) = false
|match(_, nil) = false
|match(f1::r1, f2::r2) = Real.==(f1,f2) andalso match(r1,r2)

fun pass_fail x = if x then "Pass" else "\t\tFail";

fun test(function, input, expected_output) = pass_fail ((function(input) = expected_output));

fun test_match(function, input, expected_output) = pass_fail (match(function(input),expected_output));

fun error_print (n, f) = print (Int.toString(n)^"."^f^": Error encountered in test of " ^f^"\n");

print("*********TEST RESULTS******************\n");
print("\n1.duplist: "^test(duplist, [1,3,2], [1,1,3,3,2,2])^"\n") handle _ => error_print (1, "duplist");
print("\n2.duplist: "^test(duplist, [], [])^"\n") handle _ => error_print (2, "duplist");
print("\n3.duplist: "^test(duplist, ["apple","ball","cat"], ["apple","apple","ball","ball","cat","cat"])^"\n") handle _ => error_print (3, "duplist");
print("\n4.mylength: "^test(mylength, [1,2,3], 3)^"\n") handle _ => error_print(4, "mylength");
print("\n5.mylength: "^test(mylength, [1,2,3,4,5], 5)^"\n") handle _ => error_print(5, "mylength");
print("\n6.mylength: "^test(mylength, [], 0)^"\n") handle _ => error_print(6, "mylength");
print("\n7.il2absrl: "^test_match(il2absrl, [~1, ~3], [1.0, 3.0])^"\n") handle _ => error_print(7, "il2absrl");
print("\n8.il2absrl: "^test_match(il2absrl,[3,17,~9,34,~7,2],[3.0,17.0,9.0,34.0,7.0,2.0])^"\n") handle _ => error_print(8, "il2absrl") ;
print("\n9.il2absrl: "^test_match(il2absrl,[~3,17,9,~34,~7,2],[3.0,17.0,9.0,34.0,7.0,2.0])^"\n") handle _ => error_print(9, "il2absrl");
print("\n10.myimplode: "^test(myimplode,explode("hello"), "hello")^"\n") handle _ => error_print(10, "myimplode");
print("\n11.myimplode: "^test(myimplode,explode("abracadabra"),"abracadabra")^"\n") handle _ => error_print(11, "myimplode");
print("\n12.myimplode: "^test(myimplode,explode(""),"")^"\n") handle _ => error_print(12, "myimplode");
print("\n13.lconcat: "^test(lconcat, [[1,2],[3,4],[4,5,5]], [1,2,3,4,4,5,5])^"\n") handle _ => error_print(13, "lconcat");
print("\n14.lconcat: "^test(lconcat, [[1,2,3,4,3,5],[5]], [1,2,3,4,3,5,5])^"\n") handle _ => error_print(14, "lconcat");
print("\n15.lconcat: "^test(lconcat, [[1],[2]], [1,2])^"\n") handle _ => error_print(15, "lconcat");
print("\n16.lconcat: "^test(lconcat, [[],[1],[2,3]], [1,2,3])^"\n") handle _ => error_print(16, "lconcat");
print("\n17.lconcat: "^test(lconcat, [[1,2,3,4,5]], [1,2,3,4,5])^"\n") handle _ => error_print(17, "lconcat");
print("\n18.convert: "^test(convert,[(1,3),(5,19),(7,4)],([1,5,7],[3,19,4]))^"\n") handle _ => error_print(18, "convert");
print("\n19.convert: "^test(convert,[(1,2),(3,4)],([1,3],[2,4]))^"\n") handle _ => error_print(19, "convert");
print("\n20.convert: "^test(convert,[("hello","good"),("there","bye")],(["hello","there"],["good","bye"]))^"\n") handle _ => error_print(20, "convert");

let
val sqList = mymap (fn x=>x*x);
in
print("\n21.mymap:sqList: "^test(sqList,[1,2,3,4,5], [1,4,9,16,25])^"\n") handle _ => error_print(22, "mymap:sqList")
end;

let
val incList = mymap (fn x=>x+1);
in
print("\n22.mymap:incList: "^test(incList,[1,2,3,4,5,6], [2,3,4,5,6,7])^"\n") handle _ => error_print(23, "mymap:incList")
end;

let
val decList = mymap (fn x=>x-1);
in print("\n23.mymap:decList: "^test(decList,[1,2,3,4,5,6], [0,1,2,3,4,5])^"\n") handle _ => error_print(24, "mymap:decList")
end;

let
val sumList = myfoldl (fn(a,b) =>a+b) 0;
in
print("\n24.myfoldl:sumList: "^test(sumList,[1,3,7,9,22,38], 80)^"\n") handle _ => error_print(25, "myfoldl:sumList")
end;

let
val subList = myfoldl (op -) 10;
in
print("\n25.myfoldl:subList: "^test(subList,[1,3,7,9,22,38], 30)^"\n") handle _ => error_print(25, "myfoldl:sumList")
end;


print("\n26.sumAll: "^test(sumAll,[2,1,3,5,4],15)^"\n") handle _ => error_print(26, "sumAll");
print("\n27.sumAll: "^test(sumAll,[2],2)^"\n") handle _ => error_print(27, "sumAll");
print("\n28.sumAll: "^test(sumAll,[],0)^"\n") handle _ => error_print(28, "sumAll");
print("\n29.sumEvens: "^test(sumEvens,[2,1,3,4,5],6)^"\n") handle _ => error_print(29, "sumEvens");
print("\n30.sumEvens: "^test(sumEvens,[1,3,5],0)^"\n") handle _ => error_print(30, "sumEvens");
print("\n31.sumOdds: "^test(sumOdds,[5,2,1,3,4],9)^"\n") handle _ => error_print(31, "sumOdds"); 
print("\n32.sumOdds: "^test(sumOdds,[2,4],0)^"\n") handle _ => error_print(32, "sumOdds"); 


