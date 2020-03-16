module Synthesis
open System

let abelar A = 
     ((A >12) && (A < 3097) && (A% 12 = 0)) 
   // failwith "Not implemented"

let area b h =
    match (b<0.0) || (h<0.0) with
    |true -> failwith "negative value"
    |false ->  (b * h) * 0.5
    //failwith "Not implemented"

let zollo n =
    match n<=0 with
    |true -> -1 * n
    |false -> n * 2
    //failwith "Not implemented"

let min a b =
    match a<b with|true-> a|false ->b
    //failwith "Not implemented"

let max a b =
    match a>b with |true -> a| false -> b
    //failwith "Not implemented"

let ofTime h m s = (h * 60 * 60)+(m*60)+ s
    //failwith "Not implemented"

let toTime t =
    let h = (t/ 3600)
    let m = ((t%3600)/60) 
    let s = ((t%3600)%60)
    match t < 0 with
    |true -> 0,0,0 
    |_-> (h,m,s)
    //failwith "Not implemented"

let digits ( n) = 
    let rec countDigits v acc=
        match (v:int) >= 1 with|true ->countDigits (v/10) (acc+1)|false -> acc
    match n = 0 with|true -> 1|false -> (match n > 0 with |false -> countDigits (-1 * n) 0|true -> countDigits n 0)
    
   
let minmax a =
    let b,c,d,e = a
    (min(min b c) (min d e)),(max(max b c) (max d e))
    //failwith "Not implemented"

let isLeap y =
    match y<1582 with
    |true -> failwith "input year less than 1582"
    |false -> match y%4=0 with|false -> false|true ->(match y%100=0 with |false->true|true -> (match y%400=0 with|false -> false|true ->true))
    //failwith "Not implemented"

let month n =
    match n<1 || n>12 with|true->failwith "input out of range"|false -> match n with
                                                                        |1->"January", 31
                                                                        |2->"February", 28
                                                                        |3->"March", 31
                                                                        |4->"April", 30
                                                                        |5->"May", 31
                                                                        |6->"June", 30
                                                                        |7->"July", 31
                                                                        |8->"August", 31
                                                                        |9->"September", 30
                                                                        |10->"October", 31
                                                                        |11->"November", 30
                                                                        |12->"December", 31
                                
    //failwith "Not implemented"

let toBinary n =
    let rec convert c acc = 
        match n < 0 with
        |true -> failwith "number less than 1"
        |false -> match c = 0 with 
                    |true -> acc
                    |false -> match c % 2 with 
                             |0 -> convert (c/2)("0"+acc)
                             |1-> convert (c/2) ("1"+acc)
    match n=0 with 
    |true -> "0" 
    |false -> convert n ""
    //failwith "Not implemented"

let bizFuzz n =
    let rec DivCheck m (div3,div5,div35)=
        match m < 0 with
        |true->(div3,div5,div35)
        |false-> match m = 0 with
                 |true-> (div3,div5,div35)
                 |false-> match (m % 3 = 0) && (m % 5 = 0) with
                          |true-> DivCheck (m-1)(div3+1, div5+1, div35+1)
                          |false-> match (m % 5 = 0) with
                                  |true -> DivCheck (m-1)(div3, div5+1, div35)
                                  |false -> match (m % 3 = 0) with
                                            |true -> DivCheck (m-1)(div3+1, div5, div35)
                                            |false -> DivCheck (m-1)(div3,div5,div35)
    DivCheck n (0,0,0)
    //failwith "Not implemented"

let monthDay d y =
    let monthly d a =
        match ((1<=d)&& (d<=31)) with |true-> "January" | false -> match ((32 >= d) && (d <=(59 + a))) with
                                                                    |true -> "February"
                                                                    |false -> match (((60+a) >=d) && (d <=(90+a))) with 
                                                                                 |true -> "March"
                                                                                 |false -> match (((91+a) >=d) && (d <=(120+a))) with 
                                                                                              |true -> "April"
                                                                                              |false -> match (((121+a) >=d) && (d <=(151+a))) with
                                                                                                        |true -> "May"
                                                                                                        |false -> match (((152+a) >=d) && (d <=(181+a))) with
                                                                                                                    |true -> "June"
                                                                                                                    |false -> match (((182+a) >=d) && (d <=(212+a))) with
                                                                                                                                |true -> "July"
                                                                                                                                |false -> match (((213+a) >=d) && (d <=(243+a))) with
                                                                                                                                            |true -> "August"
                                                                                                                                            |false -> match (((244+a) >=d) && (d <=(273+a))) with
                                                                                                                                                        |true -> "September"
                                                                                                                                                        |false -> match (((274+a) >=d) && (d <=(304+a))) with
                                                                                                                                                                    |true -> "October"
                                                                                                                                                                    |false -> match (((305+a) >=d) && (d <=(334+a))) with
                                                                                                                                                                                |true -> "November"
                                                                                                                                                                                |false -> match (((335+a) >=d) && (d <=(365+a))) with
                                                                                                                                                                                            |true -> "December"
                                                                                                                                                                                            |false -> failwith "out of range"
                                                                    
    match d =0 with
    |true -> failwith "cant have day 0"
    |false -> match isLeap(y) with 
             |true -> (match d >=1 && d <=366 with |false -> failwith "day out of range"|true -> monthly d 1)
             |false -> (match d >=1 && d <=365 with |false -> failwith "day out of range"|true-> monthly d 0)
    //failwith "Not implemented"

let coord _ =
    failwith "Not implemented"